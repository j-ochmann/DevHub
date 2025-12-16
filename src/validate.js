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
function mergeVersion(id, seen = new Set()) {
  if (!versions[id]) throw new Error(`Version not found: ${id}`);
  if (seen.has(id)) throw new Error(`Cyclic inheritance detected at: ${id}`);
  seen.add(id);

  const v = versions[id];
  let result = { id, features: [], keywords: [], paradigms: [] };

  // inherit first
  if (v.inherit) {
    for (const parentId of v.inherit) {
      const parentMerged = mergeVersion(parentId, seen);
      result.features.push(...parentMerged.features);
      result.keywords.push(...parentMerged.keywords);
      result.paradigms.push(...parentMerged.paradigms);
    }
  }

  // add
  if (v.add) {
    for (const item of v.add) {
      checkExists(item);
      if (item.startsWith("feature.") && !result.features.includes(item)) result.features.push(item);
      if (item.startsWith("keyword.") && !result.keywords.includes(item)) result.keywords.push(item);
      if (item.startsWith("paradigm.") && !result.paradigms.includes(item)) result.paradigms.push(item);
    }
  }

  // remove
  if (v.remove) {
    for (const item of v.remove) {
      if (item.startsWith("feature.")) result.features = result.features.filter(f => f !== item);
      if (item.startsWith("keyword.")) result.keywords = result.keywords.filter(f => f !== item);
      if (item.startsWith("paradigm.")) result.paradigms = result.paradigms.filter(f => f !== item);
    }
  }

  return result;
}

// --- BUILD FULL VERSIONS ---
const versionsFull = {};
for (const vid of Object.keys(versions)) {
  versionsFull[vid] = mergeVersion(vid);
}

// --- EXPORT FRONTEND JSON ---
fs.writeFileSync(VERSIONS_FULL_JSON, JSON.stringify(versionsFull, null, 2), "utf8");
console.log(`✅ Versions full export written to ${VERSIONS_FULL_JSON}`);
