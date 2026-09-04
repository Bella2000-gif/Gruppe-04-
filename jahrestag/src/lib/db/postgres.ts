import type { Pool as PgPool } from "pg";
import { ausZeile, saubereNotiz, type BriefStatus, type Speicher } from "./typen";

/**
 * Speicher für Anbieter ohne dauerhafte Festplatte — Vercel, Netlify und
 * alles andere Serverlose. Die Datenbank liegt woanders (Neon, Supabase,
 * Railway …) und wird über `DATABASE_URL` angebunden.
 *
 * Die Tabelle wird beim ersten Zugriff angelegt. Es gibt also keinen
 * Migrationsschritt, den man beim Aufsetzen vergessen könnte.
 */

export function postgresSpeicher(url: string): Speicher {
  let pool: PgPool | null = null;
  let bereit: Promise<void> | null = null;

  function verbindung(): PgPool {
    if (pool) return pool;
    // Erst hier laden, damit der SQLite-Betrieb den Treiber nicht braucht.
    // eslint-disable-next-line @typescript-eslint/no-require-imports
    const { Pool } = require("pg") as typeof import("pg");
    pool = new Pool({
      connectionString: url,
      // Gehostete Datenbanken verlangen TLS, bringen aber oft ein Zertifikat
      // mit, das die Standard-Kette nicht kennt. Lokal ohne TLS.
      ssl: braucht_tls(url) ? { rejectUnauthorized: false } : undefined,
      // Zwei Menschen, ein Brief pro Monat: mehr als eine Handvoll
      // Verbindungen braucht das nie, und serverlose Umgebungen danken es.
      max: 3,
      idleTimeoutMillis: 10_000,
      connectionTimeoutMillis: 10_000,
    });
    return pool;
  }

  async function sicherstellen(): Promise<void> {
    bereit ??= verbindung()
      .query(
        `CREATE TABLE IF NOT EXISTS brief_status (
           letter_id           INTEGER PRIMARY KEY,
           geoeffnet_am        TEXT,
           erledigt_am         TEXT,
           notiz               TEXT,
           notiz_aktualisiert  TEXT
         )`,
      )
      .then(() => undefined)
      .catch((fehler) => {
        // Beim nächsten Versuch neu probieren, statt dauerhaft kaputt zu sein.
        bereit = null;
        throw fehler;
      });
    return bereit;
  }

  async function frage(sql: string, werte: unknown[] = []) {
    await sicherstellen();
    const { rows } = await verbindung().query(sql, werte);
    return rows as Record<string, unknown>[];
  }

  async function status(letterId: number): Promise<BriefStatus> {
    const rows = await frage("SELECT * FROM brief_status WHERE letter_id = $1", [letterId]);
    return ausZeile(rows[0], letterId);
  }

  return {
    art: "postgres",

    async alleStatus() {
      const rows = await frage("SELECT * FROM brief_status");
      return new Map(rows.map((r) => [r.letter_id as number, ausZeile(r, r.letter_id as number)]));
    },

    status,

    async markiereGeoeffnet(letterId, wann = new Date().toISOString()) {
      const rows = await frage(
        `INSERT INTO brief_status (letter_id, geoeffnet_am)
         VALUES ($1, $2)
         ON CONFLICT (letter_id) DO UPDATE
           SET geoeffnet_am = COALESCE(brief_status.geoeffnet_am, EXCLUDED.geoeffnet_am)
         RETURNING *`,
        [letterId, wann],
      );
      return ausZeile(rows[0], letterId);
    },

    async setzeErledigt(letterId, erledigt) {
      const rows = await frage(
        `INSERT INTO brief_status (letter_id, erledigt_am)
         VALUES ($1, $2)
         ON CONFLICT (letter_id) DO UPDATE SET erledigt_am = EXCLUDED.erledigt_am
         RETURNING *`,
        [letterId, erledigt ? new Date().toISOString() : null],
      );
      return ausZeile(rows[0], letterId);
    },

    async speichereNotiz(letterId, notiz) {
      const rows = await frage(
        `INSERT INTO brief_status (letter_id, notiz, notiz_aktualisiert)
         VALUES ($1, $2, $3)
         ON CONFLICT (letter_id) DO UPDATE
           SET notiz = EXCLUDED.notiz, notiz_aktualisiert = EXCLUDED.notiz_aktualisiert
         RETURNING *`,
        [letterId, saubereNotiz(notiz), new Date().toISOString()],
      );
      return ausZeile(rows[0], letterId);
    },
  };
}

/** Lokale Datenbanken laufen ohne TLS, gehostete praktisch immer damit. */
function braucht_tls(url: string): boolean {
  try {
    const u = new URL(url);
    if (u.searchParams.get("sslmode") === "disable") return false;
    return !["localhost", "127.0.0.1", "::1"].includes(u.hostname);
  } catch {
    return true;
  }
}
