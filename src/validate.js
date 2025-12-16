import fs from "fs";
import path from "path";

// --- CESTY ---
const DIRS = {
  concepts: "content/concepts",
  features: "content/features",
  lexicon: "content/lexicon",
  versions: "content/versions",
  languages: "content/languages.json",
};

// --- POMOCNÉ FUNKCE ---
function walk(dir) {
  let results = [];
  const list = fs.readdirSync(dir, { withFileTypes: true });
  for (const file of list) {
    const fullPath = path.join(dir, file.name);
    if (file.isDirectory()) results = results.concat(walk(fullPath));
    else if (file.isFile() && file.name.endsWith(".json")) results.push(fullPath);
  }
  return results;
}

function readJSON(file) {
  try {
    return JSON.parse(fs.readFileSync(file, "utf8"));
  } catch (e) {
    throw new Error(`Failed to parse JSON ${file}: ${e.message}`);
  }
}

function loadMap(dir, typeName) {
  const map = {};
  for (const file of walk(dir)) {
    const data = readJSON(file);
    if (!data.id) throw new Error(`${typeName} missing id: ${file}`);
    if (map[data.id]) throw new Error(`Duplicate ${typeName} id: ${data.id}`);
    map[data.id] = { ...data, __file: file };
  }
  return map;
}

// --- VERZE JAZYKŮ ---
function loadVersions(dir) {
  const map = {};
  for (const file of walk(dir)) {
    const data = readJSON(file);
    if (!data.id) throw new Error(`Version missing id: ${file}`);
    const shortId = data.id.replace(/^proglang\./, "");
    map[shortId] = { ...data, __file: file, __shortId: shortId };
  }
  return map;
}

function normalizeVersionId(id) {
  return id.replace(/^proglang\./, "");
}

// --- NAČTENÍ DAT ---
console.log("🔍 Collecting files...");
const concepts = loadMap(DIRS.concepts, "Concept");
const features = loadMap(DIRS.features, "Feature");
const keywords = loadMap(path.join(DIRS.lexicon, "keywords"), "Keyword");
const operators = loadMap(path.join(DIRS.lexicon, "operators"), "Operator");
const literals = loadMap(path.join(DIRS.lexicon, "literals"), "Literal");
const versions = loadVersions(DIRS.versions);
const languages = readJSON(DIRS.languages);

// --- KONTROLA UNIQUE ID ---
console.log("🔑 Checking ID uniqueness...");
function checkUnique(map, type) {
  const seen = new Set();
  for (const id in map) {
    if (seen.has(id)) throw new Error(`Duplicate ${type} ID: ${id}`);
    seen.add(id);
  }
}
checkUnique(concepts, "Concept");
checkUnique(features, "Feature");
checkUnique(keywords, "Keyword");
checkUnique(operators, "Operator");
checkUnique(literals, "Literal");

// --- KONTROLA REFERENCÍ ---
console.log("🔗 Checking references...");

function checkRef(id, map, type, file) {
  if (!map[id]) throw new Error(`Invalid reference '${id}' in ${file}`);
}

// --- SLUŽEBNÍ FUNKCE MERGE ADD ---
function checkAddExistence(item, file) {
  if (!features[item]) throw new Error(`Feature not found: ${item} (referenced in ${file})`);
}

function mergeAdds(v) {
  // Rekurzivní merge extends
  let merged = { ...v };
  if (v.extends) {
    const parentId = normalizeVersionId(v.extends);
    const parent = versions[parentId];
    if (!parent) throw new Error(
      `Version not found: ${parentId}\nKnown versions: ${Object.keys(versions).join(", ")}`
    );
    merged = { ...mergeAdds(parent), ...merged };
  }

  // Zkontrolovat add (features, keywords, paradigms)
  ["features_add", "keywords_add", "paradigms_add"].forEach(field => {
    if (merged[field]) {
      for (const item of merged[field]) {
        if (field === "features_add") checkAddExistence(item, v.__file);
      }
    }
  });

  return merged;
}

// --- VYTVÁŘENÍ FRONTEND EXPORTU ---
console.log("📦 Building frontend export...");

const frontendExport = {};
for (const verId in versions) {
  const v = mergeAdds(versions[verId]);
  frontendExport[verId] = {
    id: v.id,
    language: v.language,
    version: v.version,
    first_release: v.first_release,
    features: v.features || [],
    keywords: v.keywords || [],
    paradigms: v.paradigms || [],
  };
}

// --- EXPORT FRONTEND JSON ---
const outPath = path.join("content", "versions.full.json");
fs.writeFileSync(outPath, JSON.stringify(frontendExport, null, 2));
console.log(`✅ Frontend export written: ${outPath}`);

console.log("🎉 Validation finished successfully!");
