import fs from "fs";
import path from "path";
import http from "http";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const DATA_DIR = path.join(__dirname, "data");
const PUBLIC_DIR = path.join(__dirname, "public");

function loadLanguages() {
    const files = fs.readdirSync(DATA_DIR, { recursive: true })
        .filter(f => f.endsWith(".json"));

    const merged = {};

    for (const file of files) {
        const raw = fs.readFileSync(path.join(DATA_DIR, file), "utf8");
        const json = JSON.parse(raw);

        Object.assign(merged, json);
    }

    // ⬇️ frontend-friendly tvar
    return Object.values(merged);
}

http.createServer((req, res) => {
    if (req.url === "/api/languages") {
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify(loadLanguages(), null, 2));
        return;
    }

    // Odstranit query string
    const urlPath = req.url.split("?")[0];
    let filePath = path.join(PUBLIC_DIR, urlPath === "/" ? "index.html" : urlPath);

    // Pokud soubor existuje, pošli ho
    if (fs.existsSync(filePath) && fs.statSync(filePath).isFile()) {
        const ext = path.extname(filePath).toLowerCase();
        const mime = ext === ".html" ? "text/html" :
                     ext === ".js" ? "text/javascript" :
                     ext === ".css" ? "text/css" : "application/octet-stream";

        res.writeHead(200, { "Content-Type": mime });
        res.end(fs.readFileSync(filePath));
    } else {
        res.writeHead(404);
        res.end("Not found");
    }
}).listen(3000, () =>
    console.log("▶ http://localhost:3000")
);

