import { NextResponse } from "next/server";
import { aktuelleRolle } from "@/lib/auth";
import { briefZugriff } from "@/lib/briefkasten";
import { setzeErledigt } from "@/lib/db";

/** Hakt ein Date als „haben wir gemacht“ ab. */
export async function POST(request: Request, ctx: { params: Promise<{ id: string }> }) {
  const rolle = await aktuelleRolle();
  if (!rolle) return NextResponse.json({ fehler: "Nicht angemeldet." }, { status: 401 });

  const id = Number((await ctx.params).id);
  const zugriff = briefZugriff(id, rolle);
  if (!zugriff.erlaubt) {
    return NextResponse.json({ fehler: "Dieser Brief ist noch zu." }, { status: 423 });
  }

  let erledigt = true;
  try {
    const body = (await request.json()) as { erledigt?: unknown };
    erledigt = Boolean(body.erledigt);
  } catch {
    return NextResponse.json({ fehler: "Ungültige Anfrage." }, { status: 400 });
  }

  const status = setzeErledigt(id, erledigt);
  return NextResponse.json({ ok: true, status });
}
