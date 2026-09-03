import fs from "node:fs";
import path from "node:path";

/**
 * Sucht heraus, ob für einen Brief ein Foto hinterlegt ist.
 *
 * Bella legt einfach `public/fotos/01.jpg` (oder .jpeg/.png/.webp) ab und
 * das Bild taucht auf — ohne dass am Code etwas geändert werden muss.
 * Die Prüfung passiert absichtlich auf dem Server: so probiert der Browser
 * nicht vier Adressen durch und schreibt dabei 404er in die Konsole.
 */

const ORDNER = path.join(process.cwd(), "public", "fotos");
const ENDUNGEN = ["jpg", "jpeg", "png", "webp"] as const;

export function fotoFuer(id: number): string | null {
  const name = String(id).padStart(2, "0");
  for (const endung of ENDUNGEN) {
    try {
      if (fs.existsSync(path.join(ORDNER, `${name}.${endung}`))) {
        return `/fotos/${name}.${endung}`;
      }
    } catch {
      // Kein lesbarer Ordner (z. B. read-only Deployment) — dann eben ohne Foto.
      return null;
    }
  }
  return null;
}
