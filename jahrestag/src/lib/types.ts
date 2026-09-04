/**
 * Datenmodell für die Briefe.
 * Der eigentliche Inhalt steht in `src/lib/letters.ts` — dort darfst du alles
 * umschreiben, ohne den Rest der App anzufassen.
 */

export type Season = "herbst" | "winter" | "fruehling" | "sommer";

/** Motiv der Briefmarke — siehe `src/components/Stamp.tsx` */
export type StampMotif =
  | "blatt"
  | "mond"
  | "tanne"
  | "schlittschuh"
  | "tasse"
  | "tulpe"
  | "pasta"
  | "fahrrad"
  | "sonne"
  | "welle"
  | "sternschnuppe"
  | "traube"
  | "herz"
  | "kino"
  | "kaffee"
  | "berg"
  | "korb"
  | "pinsel"
  | "tier"
  | "album";

export interface DatePlan {
  /** Die Aktivität, schlicht benannt */
  titel: string;
  /** Ein Satz, der Lust darauf macht */
  kurz: string;
  /**
   * Ein bis zwei kurze Absätze über das Date — warm erzählt, aber bei der
   * Sache. Bewusst keine nummerierten Regeln und keine ausgedachten Spiele:
   * es soll klingen wie etwas, das man vorschlägt, nicht wie eine Anleitung.
   */
  text: string[];
  /** Was ihr dafür braucht — nur das, was wirklich gebraucht wird */
  brauchtIhr: string[];
  /** Ungefähre Dauer, z. B. "ein halber Nachmittag" */
  dauer: string;
  /** Optionaler Satz zum Wetter. Bleibt bei der Aktivität. */
  wetter?: string;
}

export interface Letter {
  /** 1 … 13, gleichzeitig die Reihenfolge */
  id: number;
  /** Datum der Freischaltung, immer der 10. — Format YYYY-MM-DD */
  unlock: string;
  /** z. B. "Oktober 2026" */
  monat: string;
  /** Kurzform für die Briefmarke, z. B. "OKT 26" */
  stempel: string;
  /** Überschrift des Briefes */
  titel: string;
  /** Ein Satz, der auch schon vor dem Öffnen auf dem Umschlag stehen darf */
  motto: string;
  season: Season;
  stamp: StampMotif;
  /** "Mein liebster Marcolino Popolino," */
  anrede: string;
  /** Die Absätze des Briefes */
  absaetze: string[];
  /** Grußformel, z. B. "Ich liebe dich," */
  gruss: string;
  /** Unterschrift */
  signatur: string;
  /** Optionales P.S. */
  ps?: string;
  date: DatePlan;
}

/** Was der Client über einen noch verschlossenen Brief erfahren darf. */
export interface SealedLetter {
  id: number;
  unlock: string;
  monat: string;
  stempel: string;
  season: Season;
  stamp: StampMotif;
  status: "verschlossen" | "bereit" | "geoeffnet";
  /** ISO-Zeitstempel, wann Marco ihn geöffnet hat */
  geoeffnetAm: string | null;
  /** Nur bei geöffneten Briefen gesetzt */
  titel?: string;
  motto?: string;
}
