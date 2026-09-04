import { sqliteSpeicher } from "./sqlite";
import { postgresSpeicher } from "./postgres";
import type { BriefStatus, Speicher } from "./typen";

export type { BriefStatus, Speicher };

/**
 * Wählt den Speicher anhand der Umgebung — es gibt nichts umzustellen:
 *
 *   DATABASE_URL gesetzt (postgres://…)  →  Postgres   (Vercel, Netlify …)
 *   sonst                                →  SQLite     (eigener Server, lokal)
 *
 * Wichtig: Die Freischaltung der Briefe hängt an keinem der beiden, sondern
 * nur am Datum. Selbst wenn die Datenbank verloren geht, funktioniert das
 * Geschenk weiter — es fehlen dann nur die Häkchen und Notizen.
 */

declare global {
  var __briefeSpeicher: Speicher | undefined;
}

export function speicher(): Speicher {
  if (globalThis.__briefeSpeicher) return globalThis.__briefeSpeicher;

  const url = process.env.DATABASE_URL?.trim();
  const gewaehlt =
    url && /^postgres(ql)?:\/\//i.test(url) ? postgresSpeicher(url) : sqliteSpeicher();

  if (process.env.NODE_ENV !== "test") {
    console.log(`[db] Speicher: ${gewaehlt.art}`);
  }
  globalThis.__briefeSpeicher = gewaehlt;
  return gewaehlt;
}

// Bequeme Kurzformen, damit der Rest der App den Speicher nicht kennen muss.
export const alleStatus = () => speicher().alleStatus();
export const status = (id: number) => speicher().status(id);
export const markiereGeoeffnet = (id: number, wann?: string) =>
  speicher().markiereGeoeffnet(id, wann);
export const setzeErledigt = (id: number, erledigt: boolean) =>
  speicher().setzeErledigt(id, erledigt);
export const speichereNotiz = (id: number, notiz: string) => speicher().speichereNotiz(id, notiz);
