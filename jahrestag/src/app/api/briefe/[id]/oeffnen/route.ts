import { NextResponse } from "next/server";
import { aktuelleRolle } from "@/lib/auth";
import { briefZugriff } from "@/lib/briefkasten";
import { markiereGeoeffnet } from "@/lib/db";

/**
 * Hält fest, dass ein Brief geöffnet wurde.
 * Der Torwächter läuft auch hier nochmal — ein direkter Aufruf dieser Route
 * kann einen Brief nicht vorzeitig aufsperren.
 */
export async function POST(_: Request, ctx: { params: Promise<{ id: string }> }) {
  const rolle = await aktuelleRolle();
  if (!rolle) return NextResponse.json({ fehler: "Nicht angemeldet." }, { status: 401 });

  const id = Number((await ctx.params).id);
  const zugriff = briefZugriff(id, rolle);
  if (!zugriff.erlaubt) {
    return NextResponse.json(
      { fehler: "Dieser Brief ist noch zu.", unlock: zugriff.unlock },
      { status: zugriff.grund === "unbekannt" ? 404 : 423 },
    );
  }

  // Bellas Vorschau zählt ausdrücklich nicht als „geöffnet“.
  if (zugriff.vorschau || rolle === "bella") {
    return NextResponse.json({ ok: true, notiert: false });
  }

  const status = markiereGeoeffnet(id);
  return NextResponse.json({ ok: true, notiert: true, geoeffnetAm: status.geoeffnetAm });
}
