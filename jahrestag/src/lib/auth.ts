import { createHmac, timingSafeEqual } from "node:crypto";
import { cookies } from "next/headers";

/**
 * Bewusst winzige Anmeldung: zwei Codewörter, ein signiertes Cookie.
 * Es gibt keine Nutzerkonten, keine E-Mails, keine Datenbank-Sessions —
 * die Seite soll privat sein, nicht ein Sicherheitsprodukt.
 */

export type Rolle = "marco" | "bella";

export const COOKIE_NAME = "briefkasten";
const GUELTIG_TAGE = 400;

function geheimnis(): string {
  return (
    process.env.SESSION_SECRET ??
    // Fallback nur für die lokale Entwicklung: dann bleibt man eben nur so
    // lange angemeldet, bis der Server neu startet.
    "unsicheres-entwicklungs-geheimnis-bitte-SESSION_SECRET-setzen"
  );
}

function codeFuer(rolle: Rolle): string {
  return rolle === "bella"
    ? (process.env.BELLA_CODE ?? "bella")
    : (process.env.MARCO_CODE ?? "popolino");
}

function gleich(a: string, b: string): boolean {
  const ab = Buffer.from(a);
  const bb = Buffer.from(b);
  if (ab.length !== bb.length) return false;
  return timingSafeEqual(ab, bb);
}

/** Prüft ein eingegebenes Codewort und gibt die passende Rolle zurück. */
export function pruefeCode(eingabe: string): Rolle | null {
  const norm = eingabe.trim().toLowerCase();
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
  secure: process.env.NODE_ENV === "production",
} as const;
