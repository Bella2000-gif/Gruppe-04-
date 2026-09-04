import { NextResponse } from "next/server";
import {
  COOKIE_NAME,
  COOKIE_OPTIONEN,
  baueToken,
  darfEsVersuchen,
  erfolgNotieren,
  fehlversuchNotieren,
  konfigProblem,
  pruefeCode,
} from "@/lib/auth";

/** Grobe Zuordnung zum Anrufer — hinter einem Hoster steht die echte IP im Header. */
function absender(request: Request): string {
  const weitergeleitet = request.headers.get("x-forwarded-for");
  if (weitergeleitet) return weitergeleitet.split(",")[0].trim();
  return request.headers.get("x-real-ip") ?? "unbekannt";
}

/** Anmelden mit dem Codewort. */
export async function POST(request: Request) {
  // Läuft die Seite öffentlich, aber noch mit den Standardwerten aus dem
  // Quelltext? Dann lieber gar niemanden einlassen und deutlich sagen, warum.
  const problem = konfigProblem();
  if (problem) {
    console.error(`[auth] ${problem}`);
    return NextResponse.json(
      { fehler: "Die Seite ist noch nicht fertig eingerichtet.", hinweis: problem },
      { status: 503 },
    );
  }

  const schluessel = absender(request);
  const { erlaubt, wartenMs } = darfEsVersuchen(schluessel);
  if (!erlaubt) {
    const minuten = Math.ceil(wartenMs / 60_000);
    return NextResponse.json(
      { fehler: `Zu viele Versuche. Probier es in ${minuten} Minuten nochmal.` },
      { status: 429, headers: { "retry-after": String(Math.ceil(wartenMs / 1000)) } },
    );
  }

  let code = "";
  try {
    const body = (await request.json()) as { code?: unknown };
    code = typeof body.code === "string" ? body.code : "";
  } catch {
    return NextResponse.json({ fehler: "Ungültige Anfrage." }, { status: 400 });
  }

  const rolle = pruefeCode(code);
  if (!rolle) {
    fehlversuchNotieren(schluessel);
    // Kleine Verzögerung, damit Durchprobieren unattraktiv wird.
    await new Promise((r) => setTimeout(r, 600));
    return NextResponse.json({ fehler: "Das war leider nicht das Wort." }, { status: 401 });
  }

  erfolgNotieren(schluessel);
  const antwort = NextResponse.json({ rolle });
  antwort.cookies.set(COOKIE_NAME, baueToken(rolle), COOKIE_OPTIONEN);
  return antwort;
}

/** Abmelden. */
export async function DELETE() {
  const antwort = NextResponse.json({ ok: true });
  antwort.cookies.set(COOKIE_NAME, "", { ...COOKIE_OPTIONEN, maxAge: 0 });
  return antwort;
}
