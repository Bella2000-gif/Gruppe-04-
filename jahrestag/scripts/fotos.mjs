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
/**
 * Stufen aus Höhe und Qualität, die nacheinander probiert werden, bis das
 * Bild unter MAX_BYTES passt. Detailreiche Motive (Laub, Kies, Filmkorn)
 * werden bei gleicher Qualität ein Vielfaches so groß wie glatte. Dann lieber
 * die Auflösung senken als die Qualität: das Polaroid ist auf dem Bildschirm
 * ohnehin nur rund 300 Pixel breit, ein 1000 Pixel hohes Bild reicht auch auf
 * hochauflösenden Displays.
 */
const STUFEN = [
  { hoehe: 1500, qualitaet: 82 },
  { hoehe: 1250, qualitaet: 80 },
  { hoehe: 1100, qualitaet: 78 },
  { hoehe: 1000, qualitaet: 76 },
];
const MAX_BYTES = 300 * 1024;

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

  let info;
  let nachher = Infinity;
  let stufe = STUFEN[0];
  for (const s of STUFEN) {
    stufe = s;
    info = await sharp(path.join(QUELLE, datei))
      .rotate() // Ausrichtung aus den EXIF-Daten übernehmen
      .resize({ height: s.hoehe, withoutEnlargement: true })
      .webp({ quality: s.qualitaet })
      .toFile(ziel);
    nachher = statSync(ziel).size;
    if (nachher <= MAX_BYTES) break;
  }

  const kb = (n) => `${Math.round(n / 1024)} KB`;
  const verkleinert = stufe !== STUFEN[0] ? `   (Stufe ${stufe.hoehe}px/q${stufe.qualitaet})` : "";
  console.log(
    `${datei.padEnd(12)} → ${ziel}   ${info.width}×${info.height}   ` +
      `${kb(vorher)} → ${kb(nachher)}${verkleinert}`,
  );
}

console.log(`\nFertig. ${zuTun.length} Foto(s) verkleinert.`);
