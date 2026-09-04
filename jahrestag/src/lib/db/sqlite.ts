import fs from "node:fs";
import path from "node:path";
import type BetterSqlite3 from "better-sqlite3";
import { ausZeile, saubereNotiz, type BriefStatus, type Speicher } from "./typen";

/**
 * Speicher für den Betrieb auf einem Server mit eigener Festplatte —
 * ein Raspberry Pi zu Hause, ein kleiner Server, oder einfach der eigene
 * Rechner. Eine Datei, kein Dienst, nichts einzurichten.
 *
 * Für serverlose Anbieter (Vercel & Co.) taugt das nicht: dort ist die
 * Festplatte flüchtig. Dafür gibt es `postgres.ts`.
 */

const STANDARD_PFAD = path.join(process.cwd(), "data", "briefe.db");

export function sqliteSpeicher(pfad = process.env.DATABASE_PATH ?? STANDARD_PFAD): Speicher {
  let db: BetterSqlite3.Database | null = null;

  function verbindung(): BetterSqlite3.Database {
    if (db) return db;
    // Erst hier laden, damit der Postgres-Betrieb das native Modul gar nicht braucht.
    let Database: typeof BetterSqlite3;
    try {
      // eslint-disable-next-line @typescript-eslint/no-require-imports
      Database = require("better-sqlite3") as typeof BetterSqlite3;
    } catch (fehler) {
      throw new Error(
        "Das Paket „better-sqlite3“ lässt sich nicht laden. Es wird für die " +
          "Speicherung in einer Datei gebraucht. Entweder `npm install` nachholen — " +
          "oder, bei einem serverlosen Hoster wie Vercel, stattdessen DATABASE_URL " +
          "auf eine Postgres-Datenbank setzen.",
        { cause: fehler },
      );
    }

    let d: BetterSqlite3.Database;
    try {
      if (pfad !== ":memory:") fs.mkdirSync(path.dirname(pfad), { recursive: true });
      d = new Database(pfad);
    } catch (fehler) {
      // Schreibgeschütztes Dateisystem: lieber im Arbeitsspeicher weiterlaufen
      // als abstürzen. Die Briefe gehen trotzdem auf, nur die Notizen sind weg.
      console.warn(`[db] Konnte ${pfad} nicht öffnen, weiche auf den Arbeitsspeicher aus.`, fehler);
      d = new Database(":memory:");
    }

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
    db = d;
    return d;
  }

  async function status(letterId: number): Promise<BriefStatus> {
    const r = verbindung()
      .prepare("SELECT * FROM brief_status WHERE letter_id = ?")
      .get(letterId) as Record<string, unknown> | undefined;
    return ausZeile(r, letterId);
  }

  return {
    art: "sqlite",

    async alleStatus() {
      const rows = verbindung()
        .prepare("SELECT * FROM brief_status")
        .all() as Record<string, unknown>[];
      return new Map(rows.map((r) => [r.letter_id as number, ausZeile(r, r.letter_id as number)]));
    },

    status,

    async markiereGeoeffnet(letterId, wann = new Date().toISOString()) {
      verbindung()
        .prepare(
          `INSERT INTO brief_status (letter_id, geoeffnet_am)
           VALUES (?, ?)
           ON CONFLICT(letter_id) DO UPDATE
             SET geoeffnet_am = COALESCE(brief_status.geoeffnet_am, excluded.geoeffnet_am)`,
        )
        .run(letterId, wann);
      return status(letterId);
    },

    async setzeErledigt(letterId, erledigt) {
      verbindung()
        .prepare(
          `INSERT INTO brief_status (letter_id, erledigt_am)
           VALUES (?, ?)
           ON CONFLICT(letter_id) DO UPDATE SET erledigt_am = excluded.erledigt_am`,
        )
        .run(letterId, erledigt ? new Date().toISOString() : null);
      return status(letterId);
    },

    async speichereNotiz(letterId, notiz) {
      verbindung()
        .prepare(
          `INSERT INTO brief_status (letter_id, notiz, notiz_aktualisiert)
           VALUES (?, ?, ?)
           ON CONFLICT(letter_id) DO UPDATE
             SET notiz = excluded.notiz, notiz_aktualisiert = excluded.notiz_aktualisiert`,
        )
        .run(letterId, saubereNotiz(notiz), new Date().toISOString());
      return status(letterId);
    },
  };
}
