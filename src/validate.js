import fs from "fs";
import path from "path";

// --- CONFIG ---
const CONTENT_DIR = "./content";
const FEATURES_DIR = path.join(CONTENT_DIR, "features");
const KEYWORDS_DIR = path.join(CONTENT_DIR, "lexicon/keywords");
const PARADIGMS_DIR = path.join(CONTENT_DIR, "lexicon/paradigms");
const LANGUAGES_FILE = path.join(CONTENT_DIR, "languages.json");
const VERSIONS_DIR = path.join(CONTENT_DIR, "versions");
const VERSIONS_FULL_JSON = path.join(CONTENT_DIR, "versions.full.json");

// --- HELPERS ---
function readJson(filePath) {
  return JSON.parse(fs.readFileSync(filePath, "utf8"));
}

function walkDir(dir, ext = ".json") {
  let results = [];
  if (!fs.existsSync(dir)) return results;
  const files = fs.readdirSync(dir);
  for (const file of files) {
    const full = path.join(dir, file);
    if (fs.statSync(full).isDirectory()) {
      results = results.concat(walkDir(full, ext));
    } else if (full.endsWith(ext)) {
      results.push(full);
    }
  }
  return results;
}

function loadData(directoryPath) {
  const dataMap = {};
  
  for (const filePath of walkDir(directoryPath)) {
    const jsonObject = readJson(filePath);
    dataMap[jsonObject.id] = jsonObject;
  }
  //console.log(dataMap);
  return dataMap;
}

// --- LOAD DATA ---
const languages = readJson(LANGUAGES_FILE);
const features = loadData(FEATURES_DIR);
const keywords = loadData(KEYWORDS_DIR);
const paradigms = loadData(PARADIGMS_DIR);
const versions = loadData(VERSIONS_DIR);

// --- VALIDATION FUNCTIONS ---
function checkExists(id) {
  if (id.startsWith("feature.") && !features[id]) throw new Error(`Feature not found: ${id}`);
  if (id.startsWith("keyword.") && !keywords[id]) throw new Error(`Keyword not found: ${id}`);
  if (id.startsWith("paradigm.") && !paradigms[id]) throw new Error(`Paradigm not found: ${id}`);
}

// --- MERGE ADD & INHERIT ---
function mergeAdds(versionId, stack = []) {
  if (stack.includes(versionId)) {
    throw new Error(`Cyclic inheritance detected: ${stack.join(" -> ")} -> ${versionId}`);
  }

  const v = versions[versionId];
  if (!v) {
    throw new Error(`Version not found: ${versionId}`);
  }

  // ✅ BASELINE STATE – always exists
  let result = {
    id: v.id,
    features: [],
    keywords: [],
    paradigms: []
  };

  // ✅ INHERITANCE ONLY IF IT EXISTS
  if (v.extends) {
    const parent = mergeAdds(v.extends, [...stack, versionId]);

    result.features.push(...parent.features);
    result.keywords.push(...parent.keywords);
    result.paradigms.push(...parent.paradigms);
  }

  // ✅ ADD (namespace-driven)
  (v.add || []).forEach(id => {
    checkExists(id);

    if (id.startsWith("feature.")) result.features.push(id);
    else if (id.startsWith("keyword.")) result.keywords.push(id);
    else if (id.startsWith("paradigm.")) result.paradigms.push(id);
    else {
      throw new Error(`Unknown add reference: ${id}`);
    }
  });

  // ✅ DEDUP
  result.features = [...new Set(result.features)];
  result.keywords = [...new Set(result.keywords)];
  result.paradigms = [...new Set(result.paradigms)];

  return result;
}

// --- BUILD FULL VERSIONS ---
const versionsFull = {};
for (const ID of Object.keys(versions)) {
  console.log(ID);
  versionsFull[ID] = mergeAdds(ID);
}

console.log(versionsFull);
// --- EXPORT FRONTEND JSON ---
fs.writeFileSync(VERSIONS_FULL_JSON, JSON.stringify(versionsFull, null, 2), "utf8");
console.log(`✅ Versions full export written to ${VERSIONS_FULL_JSON}`);
