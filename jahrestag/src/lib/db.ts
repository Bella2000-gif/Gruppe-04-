import Database from "better-sqlite3";
import fs from "node:fs";
import path from "node:path";

/**
 * Speicher für alles, was während des Jahres entsteht:
 * wann Marco einen Brief geöffnet hat, ob das Date stattgefunden hat und
 * was er dazu geschrieben hat.
 *
 * Wichtig: Die Freischaltung der Briefe hängt NICHT an dieser Datenbank,
 * sondern nur am Datum. Selbst wenn die Datenbank verloren geht, funktioniert
 * das Geschenk weiter — es fehlen dann nur die Notizen.
 */

export interface BriefStatus {
  letterId: number;
  geoeffnetAm: string | null;
  erledigtAm: string | null;
  notiz: string | null;
  notizAktualisiert: string | null;
}

const STANDARD_PFAD = path.join(process.cwd(), "data", "briefe.db");

function verbinde(): Database.Database {
  const pfad = process.env.DATABASE_PATH ?? STANDARD_PFAD;
  try {
    if (pfad !== ":memory:") {
      fs.mkdirSync(path.dirname(pfad), { recursive: true });
    }
    return new Database(pfad);
  } catch (fehler) {
    // Read-only-Deployments (z. B. serverlose Hosts) sollen nicht abstürzen.
    console.warn(
      `[db] Konnte ${pfad} nicht öffnen, weiche auf den Arbeitsspeicher aus.`,
      fehler,
    );
    return new Database(":memory:");
  }
}

declare global {
  var __briefeDb: Database.Database | undefined;
}

function db(): Database.Database {
  if (!globalThis.__briefeDb) {
    const d = verbinde();
    d.pragma("journal_mode = WAL");
    d.exec(`
      CREATE TABLE IF NOT EXISTS brief_status (
        letter_id           INTEGER PRIMARY KEY,
        geoeffnet_am        TEXT,
        erledigt_am         TEXT,
        notiz               TEXT,
        notiz_aktualisiert  TEXT
      );
    `);
    globalThis.__briefeDb = d;
  }
  return globalThis.__briefeDb;
}

function zeile(r: Record<string, unknown> | undefined, letterId: number): BriefStatus {
  return {
    letterId,
    geoeffnetAm: (r?.geoeffnet_am as string | null) ?? null,
    erledigtAm: (r?.erledigt_am as string | null) ?? null,
    notiz: (r?.notiz as string | null) ?? null,
    notizAktualisiert: (r?.notiz_aktualisiert as string | null) ?? null,
  };
}

export function alleStatus(): Map<number, BriefStatus> {
  const rows = db().prepare("SELECT * FROM brief_status").all() as Record<string, unknown>[];
  return new Map(rows.map((r) => [r.letter_id as number, zeile(r, r.letter_id as number)]));
}

export function status(letterId: number): BriefStatus {
  const r = db()
    .prepare("SELECT * FROM brief_status WHERE letter_id = ?")
    .get(letterId) as Record<string, unknown> | undefined;
  return zeile(r, letterId);
}

/**
 * Markiert einen Brief als geöffnet. Idempotent: der erste Zeitstempel
 * bleibt für immer stehen, damit „geöffnet am“ ehrlich bleibt.
 */
export function markiereGeoeffnet(letterId: number, wann = new Date().toISOString()): BriefStatus {
  db()
    .prepare(
      `INSERT INTO brief_status (letter_id, geoeffnet_am)
       VALUES (?, ?)
       ON CONFLICT(letter_id) DO UPDATE
         SET geoeffnet_am = COALESCE(brief_status.geoeffnet_am, excluded.geoeffnet_am)`,
    )
    .run(letterId, wann);
  return status(letterId);
}

export function setzeErledigt(letterId: number, erledigt: boolean): BriefStatus {
  db()
    .prepare(
      `INSERT INTO brief_status (letter_id, erledigt_am)
       VALUES (?, ?)
       ON CONFLICT(letter_id) DO UPDATE SET erledigt_am = excluded.erledigt_am`,
    )
    .run(letterId, erledigt ? new Date().toISOString() : null);
  return status(letterId);
}

export function speichereNotiz(letterId: number, notiz: string): BriefStatus {
  const sauber = notiz.slice(0, 4000);
  db()
    .prepare(
      `INSERT INTO brief_status (letter_id, notiz, notiz_aktualisiert)
       VALUES (?, ?, ?)
       ON CONFLICT(letter_id) DO UPDATE
         SET notiz = excluded.notiz, notiz_aktualisiert = excluded.notiz_aktualisiert`,
    )
    .run(letterId, sauber.trim() === "" ? null : sauber, new Date().toISOString());
  return status(letterId);
}
