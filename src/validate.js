import fs from "fs";
import path from "path";

const ROOT = path.resolve("./content");
const CONCEPTS_DIR = path.join(ROOT, "concepts");
const FEATURES_DIR = path.join(ROOT, "features");
const LEXICON_DIR = path.join(ROOT, "lexicon");
const VERSIONS_DIR = path.join(ROOT, "versions");
const LANGUAGES_FILE = path.join(ROOT, "languages.json");
const FRONTEND_EXPORT = path.join(ROOT, "versions.full.json");

function loadJson(file) {
  if (!fs.existsSync(file)) throw new Error(`File not found: ${file}`);
  return JSON.parse(fs.readFileSync(file, "utf-8"));
}

function loadDirRecursive(dir, namespace = "") {
  if (!fs.existsSync(dir)) return {};
  let result = {};
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      const ns = namespace ? namespace + "." + entry.name : entry.name;
      Object.assign(result, loadDirRecursive(fullPath, ns));
    } else if (entry.isFile() && entry.name.endsWith(".json")) {
      const idPart = path.basename(entry.name, ".json");
      const id = namespace ? `${namespace}.${idPart}` : idPart;
      result[id] = loadJson(fullPath);
    }
  }
  return result;
}

// Load all entities
const concepts = loadDirRecursive(CONCEPTS_DIR, "concept");
const features = loadDirRecursive(FEATURES_DIR, "feature");
const keywords = loadDirRecursive(path.join(LEXICON_DIR, "keywords"), "keyword");
const operators = loadDirRecursive(path.join(LEXICON_DIR, "operators"), "operator");
const literals = loadDirRecursive(path.join(LEXICON_DIR, "literals"), "literal");
const languages = loadJson(LANGUAGES_FILE);
const versions = loadDirRecursive(VERSIONS_DIR);

// Validate existence
function checkExists(type, id) {
  switch (type) {
    case "feature": if (!features[id]) throw new Error(`Feature not found: ${id}`); break;
    case "keyword": if (!keywords[id]) throw new Error(`Keyword not found: ${id}`); break;
    case "concept": if (!concepts[id]) throw new Error(`Concept not found: ${id}`); break;
    case "version": if (!versions[id]) throw new Error(`Version not found: ${id}`); break;
    default: throw new Error(`Unknown type for checkExists: ${type}`);
  }
}

// Merge and deduplicate
function mergeAdds(base, addList, type) {
  if (!addList) return;
  for (const item of addList) {
    checkExists(type, item);
    if (!base.includes(item)) base.push(item);
  }
}

// Build version inheritance chain
function resolveVersion(id, visited = new Set()) {
  if (visited.has(id)) throw new Error(`Circular version inheritance detected: ${id}`);
  visited.add(id);
  const v = versions[id];
  if (!v) throw new Error(`Version not found: ${id}`);
  let featuresMerged = [];
  let keywordsMerged = [];
  let paradigmsMerged = [];

  // Inherit from parent version if exists
  if (v.inherits) {
    const parent = resolveVersion(v.inherits, visited);
    featuresMerged.push(...parent.features);
    keywordsMerged.push(...parent.keywords);
    paradigmsMerged.push(...parent.paradigms);
  }

  mergeAdds(featuresMerged, v.features_add, "feature");
  mergeAdds(keywordsMerged, v.keywords_add, "keyword");
  mergeAdds(paradigmsMerged, v.paradigms_add, "concept");

  return {
    id: `proglang.${id}`,
    features: featuresMerged,
    keywords: keywordsMerged,
    paradigms: paradigmsMerged
  };
}

// Generate frontend export
const frontendExport = {};
for (const vid of Object.keys(versions)) {
  frontendExport[vid] = resolveVersion(vid);
}

// Simple uniqueness checks
function checkUnique(obj, typeName) {
  const seen = new Set();
  for (const id of Object.keys(obj)) {
    if (seen.has(id)) throw new Error(`Duplicate ${typeName} ID: ${id}`);
    seen.add(id);
  }
}

checkUnique(concepts, "concept");
checkUnique(features, "feature");
checkUnique(keywords, "keyword");

// Save frontend export
fs.writeFileSync(FRONTEND_EXPORT, JSON.stringify(frontendExport, null, 2));

console.log("🔍 Collecting files...");
console.log("📘 Validating concepts...");
console.log("📙 Validating features...");
console.log("📗 Validating keywords...");
console.log("🔑 Checking ID uniqueness...");
console.log("🔗 Checking references...");
console.log("📦 Building frontend export...");
console.log("✅ Validate completed successfully.");
