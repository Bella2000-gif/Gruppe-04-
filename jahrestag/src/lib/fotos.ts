import manifest from "./fotos-manifest.json";

/**
 * Welches Foto gehört zu welchem Brief?
 *
 * Die Antwort steht in `fotos-manifest.json`, das beim Bauen geschrieben
 * wird (siehe `scripts/fotos-manifest.mjs`). Früher hat diese Datei beim
 * Aufruf im Ordner `public/fotos/` nachgeschaut — das funktioniert auf dem
 * eigenen Rechner, aber nicht bei serverlosen Hostern wie Vercel: dort
 * liefert ein CDN den `public`-Ordner aus, und die Serverfunktion sieht ihn
 * gar nicht. Dann gab es überall Platzhalter statt Fotos.
 *
 * Die Abmessungen stehen mit in der Liste. Dadurch wird das Polaroid in der
 * richtigen Form gezeichnet, bevor das Bild geladen ist: hoch- wie
 * querformatige Bilder werden nirgends beschnitten, und beim Laden springt
 * nichts.
 *
 * Ein Foto getauscht oder ergänzt? `npm run fotos` aktualisiert die Liste
 * mit; `npm run build` ebenfalls, von allein.
 */

export interface Foto {
  quelle: string;
  breite: number;
  hoehe: number;
}

const FOTOS: Record<string, Foto> = manifest;

export function fotoFuer(id: number): Foto | null {
  return FOTOS[String(id)] ?? null;
}
