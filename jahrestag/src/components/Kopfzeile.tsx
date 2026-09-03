"use client";

import { useRouter } from "next/navigation";
import { useSyncExternalStore } from "react";
import type { Rolle } from "@/lib/auth";

/**
 * Schmale Kopfzeile: Monogramm, Thema-Umschalter, Abmelden.
 * Für Bella zusätzlich die Zeitreise — damit sie im Oktober schon sehen kann,
 * wie das Geschenk im März aussieht, ohne dabei Briefe als „geöffnet“ zu
 * markieren.
 */

type Thema = "system" | "hell" | "dunkel";
const REIHE: Thema[] = ["system", "hell", "dunkel"];

/**
 * Das Thema lebt im Browser (localStorage + ein Attribut am <html>), nicht in
 * React. Deshalb wird es über einen externen Speicher gelesen — so bleibt die
 * Komponente frei von Effekten, die beim Laden nachträglich den Zustand
 * umschreiben.
 */
let zwischenspeicher: Thema | null = null;
const hoerer = new Set<() => void>();

function liesThema(): Thema {
  if (zwischenspeicher === null) {
    const g = window.localStorage.getItem("thema");
    zwischenspeicher = g === "hell" || g === "dunkel" ? g : "system";
  }
  return zwischenspeicher;
}

const liesThemaAufDemServer = (): Thema => "system";

function abonniereThema(melde: () => void): () => void {
  hoerer.add(melde);
  return () => {
    hoerer.delete(melde);
  };
}

function setzeThema(t: Thema) {
  zwischenspeicher = t;
  if (t === "system") {
    delete document.documentElement.dataset.thema;
    window.localStorage.removeItem("thema");
  } else {
    document.documentElement.dataset.thema = t;
    window.localStorage.setItem("thema", t);
  }
  for (const h of hoerer) h();
}

export function Kopfzeile({ rolle, zeitreise }: { rolle: Rolle; zeitreise: string | null }) {
  const router = useRouter();
  const thema = useSyncExternalStore(abonniereThema, liesThema, liesThemaAufDemServer);

  async function abmelden() {
    await fetch("/api/sitzung", { method: "DELETE" });
    router.replace("/anmelden");
    router.refresh();
  }

  return (
    <header className="sticky top-0 z-40 border-b border-linie/60 bg-papier/80 backdrop-blur-md">
      <div className="mx-auto flex w-full max-w-6xl items-center gap-3 px-5 py-2.5 sm:px-8">
        <span className="font-display text-sm font-semibold tracking-tight">B&nbsp;&amp;&nbsp;M</span>
        <span className="kapitaelchen hidden text-[0.55rem] sm:inline">Dreizehn Briefe</span>

        <div className="flex-1" />

        {rolle === "bella" && (
          <label className="flex items-center gap-2">
            <span className="kapitaelchen hidden text-[0.55rem] sm:inline">Zeitreise</span>
            <input
              type="date"
              defaultValue={zeitreise ?? undefined}
              min="2026-09-01"
              max="2028-01-01"
              onChange={(e) => {
                const v = e.target.value;
                router.push(v ? `/?zeit=${v}` : "/");
                router.refresh();
              }}
              className="rounded-sm border border-linie bg-karte px-2 py-1 font-stempel text-[0.68rem] text-leise outline-none focus:border-blau"
            />
          </label>
        )}

        <button
          type="button"
          onClick={() => setzeThema(REIHE[(REIHE.indexOf(thema) + 1) % REIHE.length])}
          className="cursor-pointer rounded-sm px-2 py-1 font-stempel text-[0.6rem] uppercase tracking-[0.18em] text-leise transition hover:text-tinte"
          aria-label={`Darstellung: ${thema}. Weiterschalten.`}
          title={`Darstellung: ${thema}`}
        >
          {thema === "system" ? "auto" : thema}
        </button>

        <button
          type="button"
          onClick={abmelden}
          className="cursor-pointer rounded-sm px-2 py-1 font-stempel text-[0.6rem] uppercase tracking-[0.18em] text-leise transition hover:text-rot"
        >
          zu
        </button>
      </div>
    </header>
  );
}
