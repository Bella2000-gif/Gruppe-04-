import { NextResponse } from "next/server";
import { COOKIE_NAME, COOKIE_OPTIONEN, baueToken, pruefeCode } from "@/lib/auth";

/** Anmelden mit dem Codewort. */
export async function POST(request: Request) {
  let code = "";
  try {
    const body = (await request.json()) as { code?: unknown };
    code = typeof body.code === "string" ? body.code : "";
  } catch {
    return NextResponse.json({ fehler: "Ungültige Anfrage." }, { status: 400 });
  }

  const rolle = pruefeCode(code);
  if (!rolle) {
    // Kleine Verzögerung, damit Durchprobieren unattraktiv wird.
    await new Promise((r) => setTimeout(r, 600));
    return NextResponse.json({ fehler: "Das war leider nicht das Wort." }, { status: 401 });
  }

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
