"use client";

import { useSyncExternalStore } from "react";
import { restzeitBis } from "@/lib/zeit";

/**
 * Zählt auf Mitternacht Berliner Zeit herunter. Der Zielzeitpunkt wird auf
 * dem Server berechnet und als Zahl übergeben — so ist es egal, wie die Uhr
 * oder die Zeitzone auf Marcos Handy eingestellt ist.
 *
 * Die Uhr liegt bewusst in einem winzigen Speicher außerhalb von React:
 * so laufen alle Countdowns auf der Seite über einen einzigen Intervall,
 * und beim ersten Rendern auf dem Server steht noch gar keine Zeit fest
 * (sonst würde beim Laden kurz eine falsche Zahl aufblitzen).
 */

let jetztMs = Date.now();
const hoerer = new Set<() => void>();
let takt: ReturnType<typeof setInterval> | null = null;

function abonniere(melde: () => void): () => void {
  jetztMs = Date.now();
  hoerer.add(melde);
  takt ??= setInterval(() => {
    jetztMs = Date.now();
    for (const h of hoerer) h();
  }, 1000);

  return () => {
    hoerer.delete(melde);
    if (hoerer.size === 0 && takt) {
      clearInterval(takt);
      takt = null;
    }
  };
}

const liesUhr = () => jetztMs;
const liesUhrAufDemServer = () => null;

export function Countdown({ zielMs, kompakt = false }: { zielMs: number; kompakt?: boolean }) {
  const jetzt = useSyncExternalStore(abonniere, liesUhr, liesUhrAufDemServer);

  // Vor der Hydration nichts Konkretes zeigen.
  if (jetzt === null) {
    return (
      <span className="font-stempel tabular-nums opacity-40" aria-hidden="true">
        ··· ··· ···
      </span>
    );
  }

  const rest = restzeitBis(zielMs, jetzt);

  if (kompakt) {
    if (rest.vorbei) return <span className="font-stempel text-rot">jetzt</span>;
    if (rest.tage > 0) {
      return (
        <span className="font-stempel tabular-nums">
          noch {rest.tage} {rest.tage === 1 ? "Tag" : "Tage"}
        </span>
      );
    }
    return (
      <span className="font-stempel tabular-nums">
        {String(rest.stunden).padStart(2, "0")}:{String(rest.minuten).padStart(2, "0")}:
        {String(rest.sekunden).padStart(2, "0")}
      </span>
    );
  }

  const felder: [number, string][] = [
    [rest.tage, rest.tage === 1 ? "Tag" : "Tage"],
    [rest.stunden, "Std"],
    [rest.minuten, "Min"],
    [rest.sekunden, "Sek"],
  ];

  return (
    <div className="flex items-end gap-3 sm:gap-5">
      {felder.map(([wert, label]) => (
        <div key={label} className="flex flex-col items-center">
          <span className="font-display text-3xl font-semibold tabular-nums sm:text-4xl">
            {String(wert).padStart(2, "0")}
          </span>
          <span className="kapitaelchen mt-0.5">{label}</span>
        </div>
      ))}
    </div>
  );
}
