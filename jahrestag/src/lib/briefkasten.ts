import { LETTERS, getLetter } from "./letters";
import { alleStatus, status as statusFuer, type BriefStatus } from "./db";
import { berlinerMitternacht, istFreigeschaltet } from "./zeit";
import type { Letter, SealedLetter } from "./types";
import type { Rolle } from "./auth";

/**
 * Die eine Stelle, an der entschieden wird, wer was sehen darf.
 * Alles andere in der App vertraut darauf — insbesondere geht der Text eines
 * verschlossenen Briefes hier gar nicht erst raus.
 */

export interface BriefUebersicht extends SealedLetter {
  unlockMs: number;
  erledigtAm: string | null;
  hatNotiz: boolean;
  /** Nur für Bellas Vorschau: sie darf zugeschlossene Briefe sehen. */
  vorschau: boolean;
}

/**
 * Bella kann sich in der Vorschau in die Zukunft stellen, um zu prüfen, wie
 * das Geschenk im März aussieht. Für Marco wird der Parameter ignoriert.
 */
export function jetztFuer(rolle: Rolle | null, zeitreise?: string | null): Date {
  if (rolle === "bella" && zeitreise && /^\d{4}-\d{2}-\d{2}$/.test(zeitreise)) {
    return new Date(berlinerMitternacht(zeitreise) + 12 * 3_600_000);
  }
  return new Date();
}

function statusVon(l: Letter, st: BriefStatus | undefined, jetzt: Date, rolle: Rolle | null) {
  const frei = istFreigeschaltet(l.unlock, jetzt);
  const geoeffnet = Boolean(st?.geoeffnetAm);
  if (geoeffnet) return "geoeffnet" as const;
  if (frei) return "bereit" as const;
  // Bella darf in der Vorschau hineinschauen, ohne dass es als „geöffnet“ zählt.
  return rolle === "bella" ? ("bereit" as const) : ("verschlossen" as const);
}

export async function uebersicht(
  rolle: Rolle | null,
  jetzt = new Date(),
): Promise<BriefUebersicht[]> {
  const stati = await alleStatus();
  return LETTERS.map((l) => {
    const st = stati.get(l.id);
    const echtFrei = istFreigeschaltet(l.unlock, jetzt);
    const s = statusVon(l, st, jetzt, rolle);
    const darfInhalt = s === "geoeffnet" || (rolle === "bella" && s === "bereit");
    return {
      id: l.id,
      unlock: l.unlock,
      unlockMs: berlinerMitternacht(l.unlock),
      monat: l.monat,
      stempel: l.stempel,
      season: l.season,
      stamp: l.stamp,
      status: s,
      geoeffnetAm: st?.geoeffnetAm ?? null,
      erledigtAm: st?.erledigtAm ?? null,
      hatNotiz: Boolean(st?.notiz),
      vorschau: rolle === "bella" && !echtFrei,
      ...(darfInhalt ? { titel: l.titel, motto: l.motto } : {}),
    };
  });
}

export type BriefZugriff =
  | { erlaubt: true; brief: Letter; status: BriefStatus; vorschau: boolean }
  | { erlaubt: false; grund: "unbekannt" | "verschlossen"; unlock?: string };

/**
 * Der Torwächter. Gibt den Brieftext nur heraus, wenn das Datum erreicht ist
 * (oder Bella in der Vorschau ist).
 */
export async function briefZugriff(
  id: number,
  rolle: Rolle | null,
  jetzt = new Date(),
): Promise<BriefZugriff> {
  const brief = getLetter(id);
  if (!brief) return { erlaubt: false, grund: "unbekannt" };

  const frei = istFreigeschaltet(brief.unlock, jetzt);
  const st = await statusFuer(id);

  if (frei || Boolean(st.geoeffnetAm)) {
    return { erlaubt: true, brief, status: st, vorschau: false };
  }
  if (rolle === "bella") {
    return { erlaubt: true, brief, status: st, vorschau: true };
  }
  return { erlaubt: false, grund: "verschlossen", unlock: brief.unlock };
}

/** Der nächste Brief, der noch nicht offen ist — für den Countdown auf der Startseite. */
export function naechsterBrief(jetzt = new Date()): { id: number; unlockMs: number; monat: string } | null {
  const kandidat = LETTERS.find((l) => !istFreigeschaltet(l.unlock, jetzt));
  if (!kandidat) return null;
  return {
    id: kandidat.id,
    unlockMs: berlinerMitternacht(kandidat.unlock),
    monat: kandidat.monat,
  };
}

export async function fortschritt(): Promise<{
  geoeffnet: number;
  erledigt: number;
  gesamt: number;
}> {
  const stati = await alleStatus();
  let geoeffnet = 0;
  let erledigt = 0;
  for (const s of stati.values()) {
    if (s.geoeffnetAm) geoeffnet++;
    if (s.erledigtAm) erledigt++;
  }
  return { geoeffnet, erledigt, gesamt: LETTERS.length };
}
