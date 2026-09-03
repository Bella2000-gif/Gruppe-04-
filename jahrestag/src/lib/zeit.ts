/**
 * Alles rund um Zeit passiert bewusst in `Europe/Berlin` — egal, in welcher
 * Zeitzone der Server steht. Sonst würde ein Brief in Deutschland um 2 Uhr
 * nachts aufgehen oder erst mittags.
 */

export const ZEITZONE = "Europe/Berlin";

const TEILE = new Intl.DateTimeFormat("en-CA", {
  timeZone: ZEITZONE,
  year: "numeric",
  month: "2-digit",
  day: "2-digit",
  hour: "2-digit",
  minute: "2-digit",
  second: "2-digit",
  hour12: false,
});

/** Der Versatz von Berlin zu UTC (in ms) zu einem bestimmten Zeitpunkt. */
function versatz(utcMs: number): number {
  const p = Object.fromEntries(
    TEILE.formatToParts(new Date(utcMs))
      .filter((x) => x.type !== "literal")
      .map((x) => [x.type, x.value]),
  ) as Record<string, string>;
  const alsUtc = Date.UTC(
    Number(p.year),
    Number(p.month) - 1,
    Number(p.day),
    Number(p.hour) === 24 ? 0 : Number(p.hour),
    Number(p.minute),
    Number(p.second),
  );
  return alsUtc - utcMs;
}

/**
 * Mitternacht (Berliner Zeit) eines Datums als UTC-Zeitstempel.
 * Zwei Durchläufe, damit auch die Zeitumstellung sauber getroffen wird.
 */
export function berlinerMitternacht(datum: string): number {
  const [j, m, t] = datum.split("-").map(Number);
  const naiv = Date.UTC(j, m - 1, t, 0, 0, 0);
  let ms = naiv - versatz(naiv);
  ms = naiv - versatz(ms);
  return ms;
}

/** Das heutige Datum in Berlin als "YYYY-MM-DD". */
export function heuteInBerlin(jetzt: Date = new Date()): string {
  return new Intl.DateTimeFormat("en-CA", {
    timeZone: ZEITZONE,
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
  }).format(jetzt);
}

/** Ist der Brief zu diesem Zeitpunkt schon freigeschaltet? */
export function istFreigeschaltet(unlock: string, jetzt: Date = new Date()): boolean {
  return jetzt.getTime() >= berlinerMitternacht(unlock);
}

export interface Restzeit {
  tage: number;
  stunden: number;
  minuten: number;
  sekunden: number;
  vorbei: boolean;
}

export function restzeitBis(zielMs: number, jetztMs: number): Restzeit {
  let diff = Math.max(0, zielMs - jetztMs);
  const vorbei = diff === 0;
  const tage = Math.floor(diff / 86_400_000);
  diff -= tage * 86_400_000;
  const stunden = Math.floor(diff / 3_600_000);
  diff -= stunden * 3_600_000;
  const minuten = Math.floor(diff / 60_000);
  diff -= minuten * 60_000;
  return { tage, stunden, minuten, sekunden: Math.floor(diff / 1000), vorbei };
}

const LANG = new Intl.DateTimeFormat("de-DE", {
  timeZone: ZEITZONE,
  day: "numeric",
  month: "long",
  year: "numeric",
});

export function datumLang(datum: string): string {
  return LANG.format(new Date(berlinerMitternacht(datum)));
}

export function zeitpunktLang(iso: string): string {
  return new Intl.DateTimeFormat("de-DE", {
    timeZone: ZEITZONE,
    day: "numeric",
    month: "long",
    year: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  }).format(new Date(iso));
}
