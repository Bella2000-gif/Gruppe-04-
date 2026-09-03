import fs from "node:fs";
import path from "node:path";

/**
 * Sucht heraus, ob für einen Brief ein Foto hinterlegt ist — und wie groß es ist.
 *
 * Bella legt einfach `public/fotos/01.jpg` (oder .jpeg/.png/.webp) ab und das
 * Bild taucht auf, ohne dass am Code etwas geändert werden muss.
 *
 * Zwei Gründe, warum das auf dem Server passiert und nicht im Browser:
 *   1. Der Browser probiert dann nicht vier Adressen durch und schreibt dabei
 *      404er in die Konsole.
 *   2. Wir kennen die Abmessungen, bevor das Bild geladen ist. Dadurch wird
 *      das Polaroid in der richtigen Form gezeichnet — es springt beim Laden
 *      nicht, und hoch- wie querformatige Bilder werden nirgends beschnitten.
 */

export interface Foto {
  quelle: string;
  breite: number;
  hoehe: number;
}

const ORDNER = path.join(process.cwd(), "public", "fotos");
const ENDUNGEN = ["jpg", "jpeg", "png", "webp"] as const;

/** Fallback, falls die Abmessungen nicht lesbar sind — dann eben Hochformat. */
const STANDARDFORM = { breite: 3, hoehe: 4 };

export function fotoFuer(id: number): Foto | null {
  const name = String(id).padStart(2, "0");
  for (const endung of ENDUNGEN) {
    const pfad = path.join(ORDNER, `${name}.${endung}`);
    try {
      if (!fs.existsSync(pfad)) continue;
      const masse = miss(pfad) ?? STANDARDFORM;
      return { quelle: `/fotos/${name}.${endung}`, ...masse };
    } catch {
      // Kein lesbarer Ordner (z. B. read-only Deployment) — dann eben ohne Foto.
      return null;
    }
  }
  return null;
}

/**
 * Liest Breite und Höhe direkt aus dem Dateikopf — für JPEG, PNG und WebP.
 * Bewusst ohne zusätzliche Bibliothek: es sind nur die ersten paar hundert
 * Bytes, und eine Abhängigkeit weniger ist eine Sache weniger, die in fünf
 * Jahren kaputt sein kann.
 */
function miss(pfad: string): { breite: number; hoehe: number } | null {
  const fd = fs.openSync(pfad, "r");
  try {
    const kopf = Buffer.alloc(65536);
    const gelesen = fs.readSync(fd, kopf, 0, kopf.length, 0);
    const d = kopf.subarray(0, gelesen);

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
        if (d[i] !== 0xff) {
          i++;
          continue;
        }
        const marker = d[i + 1];
        // Füllbytes und Marker ohne Nutzlast überspringen
        if (marker === 0xff) {
          i++;
          continue;
        }
        if (marker === 0x01 || (marker >= 0xd0 && marker <= 0xd9)) {
          i += 2;
          continue;
        }
        // SOF0…SOF15, ohne die Marker, die keine Bildmaße tragen
        const istSof =
          marker >= 0xc0 && marker <= 0xcf && marker !== 0xc4 && marker !== 0xc8 && marker !== 0xcc;
        if (istSof) {
          return { hoehe: d.readUInt16BE(i + 5), breite: d.readUInt16BE(i + 7) };
        }
        i += 2 + d.readUInt16BE(i + 2);
      }
    }

    return null;
  } finally {
    fs.closeSync(fd);
  }
}
