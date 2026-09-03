/**
 * Fotos fürs Web verkleinern.
 *
 *   npm run fotos
 *
 * Nimmt alles aus `fotos-original/` und legt verkleinerte WebP-Dateien in
 * `public/fotos/` ab. Handyfotos sind gern 4 MB groß — danach sind es meist
 * unter 200 KB, ohne dass man einen Unterschied sieht.
 *
 * Die Dateien müssen `01`, `02` … `13` heißen (die Nummer des Briefes).
 * Die Originale bleiben unangetastet.
 */

import { readdir, mkdir } from "node:fs/promises";
import { statSync } from "node:fs";
import path from "node:path";
import sharp from "sharp";

const QUELLE = "fotos-original";
const ZIEL = path.join("public", "fotos");
const MAX_HOEHE = 1500;
const QUALITAET = 82;

const erlaubt = new Set([".jpg", ".jpeg", ".png", ".webp", ".tif", ".tiff"]);

await mkdir(ZIEL, { recursive: true });

let dateien;
try {
  dateien = await readdir(QUELLE);
} catch {
  console.error(`Der Ordner "${QUELLE}/" fehlt. Leg ihn an und tu die Fotos hinein.`);
  process.exit(1);
}

const zuTun = dateien
  .filter((d) => erlaubt.has(path.extname(d).toLowerCase()))
  .filter((d) => /^\d{1,2}$/.test(path.basename(d, path.extname(d))))
  .sort();

if (zuTun.length === 0) {
  console.log(`Nichts zu tun. Erwartet werden Dateien wie "01.jpg" in "${QUELLE}/".`);
  process.exit(0);
}

for (const datei of zuTun) {
  const nummer = String(Number(path.basename(datei, path.extname(datei)))).padStart(2, "0");
  const ziel = path.join(ZIEL, `${nummer}.webp`);
  const vorher = statSync(path.join(QUELLE, datei)).size;

  const info = await sharp(path.join(QUELLE, datei))
    .rotate() // Ausrichtung aus den EXIF-Daten übernehmen
    .resize({ height: MAX_HOEHE, withoutEnlargement: true })
    .webp({ quality: QUALITAET })
    .toFile(ziel);

  const nachher = statSync(ziel).size;
  const kb = (n) => `${Math.round(n / 1024)} KB`;
  console.log(
    `${datei.padEnd(12)} → ${ziel}   ${info.width}×${info.height}   ${kb(vorher)} → ${kb(nachher)}`,
  );
}

console.log(`\nFertig. ${zuTun.length} Foto(s) verkleinert.`);
