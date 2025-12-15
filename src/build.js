import fs from "fs";
import path from "path";

const CONTENT = "./content";
const DIST = "./dist/data";

function loadDir(dir) {
  return fs.readdirSync(dir)
    .filter(f => f.endsWith(".json"))
    .map(f => JSON.parse(fs.readFileSync(path.join(dir, f), "utf-8")));
}

function build() {
  const keywords = loadDir(`${CONTENT}/lexicon/keywords`);
  const concepts = loadDir(`${CONTENT}/concepts/oop`);
  const features = loadDir(`${CONTENT}/features/oop`);

  fs.mkdirSync(DIST, { recursive: true });

  fs.writeFileSync(
    `${DIST}/keywords.json`,
    JSON.stringify(keywords, null, 2)
  );

  console.log(`✔ keywords: ${keywords.length}`);
}

build();
