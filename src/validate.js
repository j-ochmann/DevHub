import fs from "fs";
import path from "path";
import Ajv from "ajv";
import addFormats from "ajv-formats";

const ajv = new Ajv({ allErrors: true });
addFormats(ajv);

// --- Cesty ---
const VERSIONS_DIR = "content/versions";
const LANGUAGES_FILE = "content/languages.json";
const FEATURES_DIR = "content/features";
const KEYWORDS_DIR = "content/lexicon/keywords";
const PARADIGMS_DIR = "content/lexicon/paradigms";

// --- Pomocné funkce ---
function loadJson(filePath) {
  const raw = fs.readFileSync(filePath, "utf-8");
  try {
    return JSON.parse(raw);
  } catch (e) {
    console.error(`Invalid JSON in ${filePath}:`, e.message);
    throw e;
  }
}

function collectFiles(dir, ext = ".json") {
  return fs.readdirSync(dir)
    .filter(f => f.endsWith(ext))
    .map(f => path.join(dir, f));
}

function checkIdPrefix(id) {
  return (
    id.startsWith("concept.") ||
    id.startsWith("feature.") ||
    id.startsWith("keyword.") ||
    id.startsWith("operator.") ||
    id.startsWith("literal.") ||
    id.startsWith("paradigm.") ||
    id.startsWith("proglang.")
  );
}

// --- Načtení všech entity slovníků ---
function loadEntities(dir) {
  const files = collectFiles(dir);
  const map = {};
  for (const f of files) {
    const data = loadJson(f);
    map[data.id] = data;
  }
  return map;
}

const featuresMap = loadEntities(FEATURES_DIR);
const keywordsMap = loadEntities(KEYWORDS_DIR);
const paradigmsMap = loadEntities(PARADIGMS_DIR);

// --- Load versions ---
const versionFiles = collectFiles(VERSIONS_DIR);
const versionMap = {};

for (const file of versionFiles) {
  const data = loadJson(file);
  versionMap[data.id] = data;

  // Kontrola ID prefixu
  if (!checkIdPrefix(data.id)) {
    console.error(`Invalid version ID: ${data.id}`);
    process.exit(1);
  }

  // Kontrola add položek
  if (Array.isArray(data.add)) {
    for (const item of data.add) {
      if (!checkIdPrefix(item)) {
        console.error(`Invalid ID prefix in add: ${item} (version ${data.id})`);
        process.exit(1);
      }
    }
  }
}

// --- Load languages ---
const languages = loadJson(LANGUAGES_FILE);

// --- Kontrola existence jazyků ---
for (const vid of Object.keys(versionMap)) {
  const langKey = vid.split(".")[1]; // proglang.csharp.07 → csharp
  if (!languages[langKey]) {
    console.error(`Language ${langKey} not found in languages.json for version ${vid}`);
    process.exit(1);
  }
}

// --- Kontrola existence položek add ---
function checkAddExistence(item) {
  if (item.startsWith("feature.") && !featuresMap[item]) {
    throw new Error(`Feature not found: ${item}`);
  }
  if (item.startsWith("keyword.") && !keywordsMap[item]) {
    throw new Error(`Keyword not found: ${item}`);
  }
  if (item.startsWith("paradigm.") && !paradigmsMap[item]) {
    throw new Error(`Paradigm not found: ${item}`);
  }
}

// --- Merge add rekurzivně s kontrolou duplicit ---
function mergeAdds(versionId) {
  const visited = new Set();
  const features = new Set();
  const keywords = new Set();
  const paradigms = new Set();

  function recurse(id) {
    if (!id || visited.has(id)) return;
    visited.add(id);

    const v = versionMap[id];
    if (!v) throw new Error(`Version not found: ${id}`);

    if (v.extends) recurse(v.extends);

    if (Array.isArray(v.add)) {
      for (const item of v.add) {
        checkAddExistence(item);

        if (item.startsWith("feature.")) {
          if (features.has(item)) console.warn(`Warning: Feature ${item} already inherited in ${versionId}`);
          features.add(item);
        } else if (item.startsWith("keyword.")) {
          if (keywords.has(item)) console.warn(`Warning: Keyword ${item} already inherited in ${versionId}`);
          keywords.add(item);
        } else if (item.startsWith("paradigm.")) {
          if (paradigms.has(item)) console.warn(`Warning: Paradigm ${item} already inherited in ${versionId}`);
          paradigms.add(item);
        } else {
          console.warn(`Unknown add type: ${item}`);
        }
      }
    }
  }

  recurse(versionId);

  return {
    features: Array.from(features),
    keywords: Array.from(keywords),
    paradigms: Array.from(paradigms),
  };
}

// --- Vytvoření JSON exportu pro frontend ---
const exportJson = {};

for (const vid of Object.keys(versionMap)) {
  exportJson[vid] = {
    language: languages[vid.split(".")[1]].name,
    ...mergeAdds(vid)
  };
}

// --- Uložení exportu ---
fs.writeFileSync("content/export/versions.json", JSON.stringify(exportJson, null, 2), "utf-8");
console.log("\n✅ JSON export saved to content/export/versions.json");

// --- Log ---
for (const vid of Object.keys(versionMap)) {
  const merged = mergeAdds(vid);
  console.log(`\nVersion ${vid} (${languages[vid.split(".")[1]].name}):`);
  console.log("Features:", merged.features);
  console.log("Keywords:", merged.keywords);
  console.log("Paradigms:", merged.paradigms);
}

console.log("\n✅ All versions processed successfully.");
