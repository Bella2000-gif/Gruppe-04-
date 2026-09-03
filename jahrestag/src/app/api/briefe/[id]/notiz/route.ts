import { NextResponse } from "next/server";
import { aktuelleRolle } from "@/lib/auth";
import { briefZugriff } from "@/lib/briefkasten";
import { speichereNotiz } from "@/lib/db";

/** Marcos Antwort auf einen Brief — wird im Reisetagebuch gesammelt. */
export async function PUT(request: Request, ctx: { params: Promise<{ id: string }> }) {
  const rolle = await aktuelleRolle();
  if (!rolle) return NextResponse.json({ fehler: "Nicht angemeldet." }, { status: 401 });

  const id = Number((await ctx.params).id);
  const zugriff = briefZugriff(id, rolle);
  if (!zugriff.erlaubt) {
    return NextResponse.json({ fehler: "Dieser Brief ist noch zu." }, { status: 423 });
  }

  let notiz = "";
  try {
    const body = (await request.json()) as { notiz?: unknown };
    notiz = typeof body.notiz === "string" ? body.notiz : "";
  } catch {
    return NextResponse.json({ fehler: "Ungültige Anfrage." }, { status: 400 });
  }

  const status = speichereNotiz(id, notiz);
  return NextResponse.json({ ok: true, status });
}
