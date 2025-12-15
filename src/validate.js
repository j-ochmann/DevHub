// src/validate.js
import fs from "fs";
import path from "path";
import Ajv from "ajv";
import addFormats from "ajv-formats";

// Inicializace Ajv
const ajv = new Ajv({ strict: true, allErrors: true, allowUnionTypes: true });
addFormats(ajv);

// --- utility funkce ---
function loadJson(file) {
  const content = fs.readFileSync(file, "utf-8").trim();
  if (!content) {
    console.error(`❌ Empty JSON file: ${file}`);
    process.exit(1);
  }
  try {
    return JSON.parse(content);
  } catch (err) {
    console.error(`❌ Invalid JSON in file: ${file}`);
    console.error(err.message);
    process.exit(1);
  }
}

function loadSchema(name) {
  return loadJson(path.join("schema", name));
}

function collectJsonFiles(dir) {
  let files = [];
  for (const entry of fs.readdirSync(dir)) {
    const full = path.join(dir, entry);
    if (fs.statSync(full).isDirectory()) {
      files = files.concat(collectJsonFiles(full));
    } else if (entry.endsWith(".json")) {
      files.push(full);
    }
  }
  return files;
}

// --- validace ---
function validateAgainstSchema(files, schema) {
  const validate = ajv.compile(schema);
  for (const file of files) {
    const data = loadJson(file);
    if (!validate(data)) {
      console.error(`❌ Schema error in ${file}`);
      console.error(validate.errors);
      process.exit(1);
    }
  }
}

function collectIds(files) {
  const ids = new Map();
  for (const file of files) {
    const { id } = loadJson(file);
    if (!id) continue;
    if (ids.has(id)) {
      console.error(`❌ Duplicate ID: ${id} in ${file} and ${ids.get(id)}`);
      process.exit(1);
    }
    ids.set(id, file);
  }
  return ids;
}

function validateReferences(files, allIds) {
  for (const file of files) {
    const data = loadJson(file);
    for (const key of ["relatedConcepts", "relatedFeatures", "relatedKeywords"]) {
      if (!Array.isArray(data[key])) continue;
      for (const ref of data[key]) {
        if (!allIds.has(ref)) {
          console.error(`❌ Invalid reference '${ref}' in ${file}`);
          process.exit(1);
        }
      }
    }
  }
}

// --- spuštění validace ---
console.log("🔍 Collecting files...");
const allFiles = collectJsonFiles("content");

console.log("📘 Validating concepts...");
validateAgainstSchema(
  allFiles.filter(f => f.includes("/concepts/")),
  loadSchema("concept.schema.json")
);

console.log("📙 Validating features...");
validateAgainstSchema(
  allFiles.filter(f => f.includes("/features/")),
  loadSchema("feature.schema.json")
);

console.log("📗 Validating keywords...");
validateAgainstSchema(
  allFiles.filter(f => f.includes("/lexicon/keywords/")),
  loadSchema("keyword.schema.json")
);

console.log("🔑 Checking ID uniqueness...");
const ids = collectIds(allFiles);

console.log("🔗 Checking references...");
validateReferences(allFiles, ids);

console.log("✅ Validation passed");

