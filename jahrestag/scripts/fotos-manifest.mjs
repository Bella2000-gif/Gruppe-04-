/**
 * Schreibt die Liste der vorhandenen Fotos in `src/lib/fotos-manifest.json`.
 *
 * Warum überhaupt eine Liste? Früher hat die Seite beim Aufruf im Ordner
 * `public/fotos/` nachgeschaut. Das funktioniert auf dem eigenen Rechner,
 * aber nicht bei serverlosen Hostern wie Vercel: dort liefert ein CDN den
 * `public`-Ordner aus, und die Serverfunktion sieht ihn gar nicht. Ergebnis
 * waren überall Platzhalter statt Fotos.
 *
 * Deshalb wird die Liste jetzt beim Bauen festgeschrieben — da liegen die
 * Dateien noch da, wo man sie sieht. `npm run build` ruft das automatisch auf
 * (über `prebuild`), und `npm run fotos` aktualisiert sie gleich mit.
 */

import { readdir, writeFile, open } from "node:fs/promises";
import path from "node:path";

const ORDNER = path.join("public", "fotos");
const ZIEL = path.join("src", "lib", "fotos-manifest.json");
const ENDUNGEN = ["jpg", "jpeg", "png", "webp"];

/** Fallback, falls die Abmessungen nicht lesbar sind — dann eben Hochformat. */
const STANDARDFORM = { breite: 3, hoehe: 4 };

/**
 * Liest Breite und Höhe aus dem Dateikopf — JPEG, PNG und WebP.
 * Bewusst ohne zusätzliche Bibliothek: es sind nur die ersten paar hundert
 * Bytes, und eine Abhängigkeit weniger ist eine Sache weniger, die beim
 * Bauen kaputtgehen kann.
 */
async function miss(pfad) {
  const datei = await open(pfad, "r");
  try {
    const puffer = Buffer.alloc(65536);
    const { bytesRead } = await datei.read(puffer, 0, puffer.length, 0);
    const d = puffer.subarray(0, bytesRead);

    // ── PNG: "…IHDR" gefolgt von Breite und Höhe als 32-Bit-Zahlen
    if (d.length > 24 && d.readUInt32BE(0) === 0x89504e47) {
      return { breite: d.readUInt32BE(16), hoehe: d.readUInt32BE(20) };
    }

    // ── WebP: RIFF-Container mit drei möglichen Varianten
    if (d.length > 30 && d.toString("ascii", 0, 4) === "RIFF" && d.toString("ascii", 8, 12) === "WEBP") {
      const art = d.toString("ascii", 12, 16);
      if (art === "VP8 ") {
        return { breite: d.readUInt16LE(26) & 0x3fff, hoehe: d.readUInt16LE(28) & 0x3fff };
      }
      if (art === "VP8L") {
        const bits = d.readUInt32LE(21);
        return { breite: (bits & 0x3fff) + 1, hoehe: ((bits >> 14) & 0x3fff) + 1 };
      }
      if (art === "VP8X") {
        return {
          breite: (d.readUIntLE(24, 3) & 0xffffff) + 1,
          hoehe: (d.readUIntLE(27, 3) & 0xffffff) + 1,
        };
      }
      return null;
    }

    // ── JPEG: durch die Segmente laufen, bis ein SOF-Marker kommt
    if (d.length > 4 && d.readUInt16BE(0) === 0xffd8) {
      let i = 2;
      while (i + 9 < d.length) {
        if (d[i] !== 0xff) { i++; continue; }
        const marker = d[i + 1];
        if (marker === 0xff) { i++; continue; }
        if (marker === 0x01 || (marker >= 0xd0 && marker <= 0xd9)) { i += 2; continue; }
        const istSof =
          marker >= 0xc0 && marker <= 0xcf && marker !== 0xc4 && marker !== 0xc8 && marker !== 0xcc;
        if (istSof) return { hoehe: d.readUInt16BE(i + 5), breite: d.readUInt16BE(i + 7) };
        i += 2 + d.readUInt16BE(i + 2);
      }
    }

    return null;
  } finally {
    await datei.close();
  }
}

export async function schreibeManifest({ still = false } = {}) {
  let dateien = [];
  try {
    dateien = await readdir(ORDNER);
  } catch {
    // Kein Ordner: dann eben keine Fotos, die Seite zeigt Platzhalter.
  }

  const manifest = {};
  for (let id = 1; id <= 13; id++) {
    const name = String(id).padStart(2, "0");
    const treffer = ENDUNGEN.map((e) => `${name}.${e}`).find((d) => dateien.includes(d));
    if (!treffer) continue;
    const masse = (await miss(path.join(ORDNER, treffer))) ?? STANDARDFORM;
    manifest[id] = { quelle: `/fotos/${treffer}`, ...masse };
  }

  await writeFile(ZIEL, JSON.stringify(manifest, null, 2) + "\n");
  if (!still) {
    console.log(`${ZIEL}: ${Object.keys(manifest).length} von 13 Fotos eingetragen`);
  }
  return manifest;
}

// Direkt aufgerufen (nicht importiert)? Dann einfach schreiben.
if (import.meta.url === `file://${process.argv[1]}`) {
  await schreibeManifest();
}
