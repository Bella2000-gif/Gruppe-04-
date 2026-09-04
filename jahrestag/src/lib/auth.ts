import { createHmac, timingSafeEqual } from "node:crypto";
import { cookies } from "next/headers";

/**
 * Bewusst winzige Anmeldung: zwei Codewörter, ein signiertes Cookie.
 * Es gibt keine Nutzerkonten, keine E-Mails, keine Sitzungen in der
 * Datenbank — die Seite soll privat sein, nicht ein Sicherheitsprodukt.
 *
 * Sobald sie aber unter einer öffentlich erreichbaren Adresse liegt, ist
 * dieses Codewort das Einzige, was zwischen Fremden und euren Briefen steht.
 * Deshalb weigert sich die App im Produktivbetrieb, mit den Standardwerten
 * aus dem Quelltext zu laufen (siehe `konfigProblem`).
 */

export type Rolle = "marco" | "bella";

export const COOKIE_NAME = "briefkasten";
const GUELTIG_TAGE = 400;

const STANDARD_GEHEIMNIS = "unsicheres-entwicklungs-geheimnis-bitte-SESSION_SECRET-setzen";
const STANDARD_MARCO = "popolino";
const STANDARD_BELLA = "bella";

/**
 * Werte, die im Quelltext oder in `.env.example` stehen und damit jeder
 * kennt, der das Repository sieht. Sie dürfen niemals eine öffentlich
 * erreichbare Seite schützen.
 */
const OEFFENTLICH_BEKANNT = new Set([
  STANDARD_GEHEIMNIS,
  STANDARD_MARCO,
  STANDARD_BELLA,
  "bitte-hier-ein-eigenes-wort-fuer-marco-eintragen",
  "bitte-hier-ein-eigenes-wort-fuer-bella-eintragen",
  "bitte-hier-eine-lange-zufallszeichenkette-eintragen",
]);

const MIN_CODE_LAENGE = 10;
const MIN_GEHEIMNIS_LAENGE = 24;

const istProduktiv = () => process.env.NODE_ENV === "production";

function geheimnis(): string {
  return process.env.SESSION_SECRET ?? STANDARD_GEHEIMNIS;
}

function codeFuer(rolle: Rolle): string {
  return rolle === "bella"
    ? (process.env.BELLA_CODE ?? STANDARD_BELLA)
    : (process.env.MARCO_CODE ?? STANDARD_MARCO);
}

/**
 * Prüft, ob die Seite so, wie sie gerade eingerichtet ist, öffentlich stehen
 * darf. Gibt eine Beschreibung des Problems zurück, oder null wenn alles gut
 * ist. In der Entwicklung (`npm run dev`) ist alles erlaubt — da schadet ein
 * Standardwert niemandem.
 *
 * Geprüft wird die Güte des Wertes, nicht nur, ob er vom Standard abweicht:
 * ein Codewort, das jeder erraten kann, ist genauso wertlos wie gar keins.
 */
export function konfigProblem(): string | null {
  if (!istProduktiv()) return null;

  const maengel: string[] = [];

  const s = process.env.SESSION_SECRET ?? "";
  if (s === "" || OEFFENTLICH_BEKANNT.has(s)) {
    maengel.push("SESSION_SECRET fehlt");
  } else if (s.length < MIN_GEHEIMNIS_LAENGE) {
    maengel.push(`SESSION_SECRET ist zu kurz (mindestens ${MIN_GEHEIMNIS_LAENGE} Zeichen)`);
  }

  for (const [name, wert] of [
    ["MARCO_CODE", process.env.MARCO_CODE ?? ""],
    ["BELLA_CODE", process.env.BELLA_CODE ?? ""],
  ] as const) {
    if (wert === "" || OEFFENTLICH_BEKANNT.has(wert.trim().toLowerCase())) {
      maengel.push(`${name} fehlt oder steht noch auf dem Wert aus dem Quelltext`);
    } else if (wert.trim().length < MIN_CODE_LAENGE) {
      maengel.push(`${name} ist zu kurz (mindestens ${MIN_CODE_LAENGE} Zeichen)`);
    }
  }

  if (process.env.MARCO_CODE && process.env.MARCO_CODE === process.env.BELLA_CODE) {
    maengel.push("MARCO_CODE und BELLA_CODE sind identisch");
  }

  if (maengel.length === 0) return null;

  return (
    `${maengel.join("; ")}. Solange das so ist, lässt die Seite niemanden herein — ` +
    `sonst könnte jeder eure Briefe lesen. Trag die Werte bei deinem Hoster ein ` +
    `und starte neu.`
  );
}

function gleich(a: string, b: string): boolean {
  const ab = Buffer.from(a);
  const bb = Buffer.from(b);
  // Ohne gleiche Länge wirft timingSafeEqual — trotzdem beide Puffer bilden,
  // damit die Laufzeit nicht schon die Länge verrät.
  if (ab.length !== bb.length) return false;
  return timingSafeEqual(ab, bb);
}

/** Prüft ein eingegebenes Codewort und gibt die passende Rolle zurück. */
export function pruefeCode(eingabe: string): Rolle | null {
  // Im Produktivbetrieb mit kaputter Konfiguration lässt niemand rein.
  if (konfigProblem()) return null;

  const norm = eingabe.trim().toLowerCase();
  if (norm === "") return null;
  if (gleich(norm, codeFuer("marco").trim().toLowerCase())) return "marco";
  if (gleich(norm, codeFuer("bella").trim().toLowerCase())) return "bella";
  return null;
}

function signiere(nutzlast: string): string {
  return createHmac("sha256", geheimnis()).update(nutzlast).digest("base64url");
}

export function baueToken(rolle: Rolle): string {
  const nutzlast = `${rolle}.${Date.now()}`;
  return `${nutzlast}.${signiere(nutzlast)}`;
}

export function leseToken(token: string | undefined): Rolle | null {
  if (!token) return null;
  const teile = token.split(".");
  if (teile.length !== 3) return null;
  const [rolle, zeit, sig] = teile;
  if (rolle !== "marco" && rolle !== "bella") return null;
  if (!gleich(sig, signiere(`${rolle}.${zeit}`))) return null;
  const alter = Date.now() - Number(zeit);
  if (!Number.isFinite(alter) || alter < 0 || alter > GUELTIG_TAGE * 86_400_000) return null;
  return rolle;
}

/** Die Rolle des aktuellen Besuchers, oder null wenn nicht angemeldet. */
export async function aktuelleRolle(): Promise<Rolle | null> {
  const jar = await cookies();
  return leseToken(jar.get(COOKIE_NAME)?.value);
}

export const COOKIE_OPTIONEN = {
  httpOnly: true,
  sameSite: "lax",
  path: "/",
  maxAge: GUELTIG_TAGE * 86_400,
  secure: istProduktiv(),
} as const;

/* ───────────────────────── Versuche begrenzen ─────────────────────────
   Ein einfacher Zähler im Arbeitsspeicher. Bei serverlosen Anbietern gilt er
   je Instanz und ist damit kein Bollwerk — zusammen mit der halben Sekunde
   Verzögerung pro Fehlversuch macht er das Durchprobieren aber unattraktiv
   genug. Die eigentliche Sicherheit ist und bleibt ein ordentliches
   Codewort, kein einzelnes Wort.
   -------------------------------------------------------------------- */

const FENSTER_MS = 10 * 60_000;
const MAX_VERSUCHE = 8;
const SPERRE_MS = 15 * 60_000;

type Zaehler = { versuche: number; ersterVersuch: number; gesperrtBis: number };
const zaehler = new Map<string, Zaehler>();

export function darfEsVersuchen(schluessel: string): { erlaubt: boolean; wartenMs: number } {
  const jetzt = Date.now();
  const z = zaehler.get(schluessel);

  if (!z) return { erlaubt: true, wartenMs: 0 };
  if (z.gesperrtBis > jetzt) return { erlaubt: false, wartenMs: z.gesperrtBis - jetzt };
  if (jetzt - z.ersterVersuch > FENSTER_MS) {
    zaehler.delete(schluessel);
    return { erlaubt: true, wartenMs: 0 };
  }
  return { erlaubt: true, wartenMs: 0 };
}

export function fehlversuchNotieren(schluessel: string): void {
  const jetzt = Date.now();
  const z = zaehler.get(schluessel);

  if (!z || jetzt - z.ersterVersuch > FENSTER_MS) {
    zaehler.set(schluessel, { versuche: 1, ersterVersuch: jetzt, gesperrtBis: 0 });
    return;
  }

  z.versuche += 1;
  if (z.versuche >= MAX_VERSUCHE) {
    z.gesperrtBis = jetzt + SPERRE_MS;
    z.versuche = 0;
    z.ersterVersuch = jetzt;
  }

  // Die Tabelle darf nicht unbegrenzt wachsen.
  if (zaehler.size > 500) {
    for (const [k, v] of zaehler) {
      if (v.gesperrtBis < jetzt && jetzt - v.ersterVersuch > FENSTER_MS) zaehler.delete(k);
    }
  }
}

export function erfolgNotieren(schluessel: string): void {
  zaehler.delete(schluessel);
}
