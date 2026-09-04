/**
 * Was während des Jahres entsteht: wann Marco einen Brief geöffnet hat,
 * ob das Date stattgefunden hat und was er dazu geschrieben hat.
 */
export interface BriefStatus {
  letterId: number;
  geoeffnetAm: string | null;
  erledigtAm: string | null;
  notiz: string | null;
  notizAktualisiert: string | null;
}

/**
 * Die Schnittstelle, die beide Speicher erfüllen — SQLite und Postgres.
 *
 * Zeitstempel werden in beiden als ISO-Zeichenkette abgelegt, nicht als
 * Datumstyp der Datenbank. Das hält die beiden Fassungen wirklich
 * austauschbar und erspart Überraschungen mit Zeitzonen.
 */
export interface Speicher {
  readonly art: "sqlite" | "postgres";
  alleStatus(): Promise<Map<number, BriefStatus>>;
  status(letterId: number): Promise<BriefStatus>;
  /** Idempotent: der erste Zeitstempel bleibt stehen, damit „geöffnet am“ ehrlich bleibt. */
  markiereGeoeffnet(letterId: number, wann?: string): Promise<BriefStatus>;
  setzeErledigt(letterId: number, erledigt: boolean): Promise<BriefStatus>;
  speichereNotiz(letterId: number, notiz: string): Promise<BriefStatus>;
}

/** Baut aus einer Datenbankzeile den Status — für beide Speicher gleich. */
export function ausZeile(
  r: Record<string, unknown> | undefined,
  letterId: number,
): BriefStatus {
  return {
    letterId,
    geoeffnetAm: (r?.geoeffnet_am as string | null) ?? null,
    erledigtAm: (r?.erledigt_am as string | null) ?? null,
    notiz: (r?.notiz as string | null) ?? null,
    notizAktualisiert: (r?.notiz_aktualisiert as string | null) ?? null,
  };
}

/** Notizen werden gekappt, damit niemand die Datenbank vollschreibt. */
export const NOTIZ_MAX = 4000;

export function saubereNotiz(notiz: string): string | null {
  const gekappt = notiz.slice(0, NOTIZ_MAX);
  return gekappt.trim() === "" ? null : gekappt;
}
