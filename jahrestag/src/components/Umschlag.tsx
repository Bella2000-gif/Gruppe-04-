"use client";

import { useRouter } from "next/navigation";
import { useCallback, useRef, useState } from "react";
import { Marke, Poststempel } from "./Marke";
import { Motiv } from "./Motive";
import { Siegel } from "./Siegel";
import { Countdown } from "./Countdown";
import type { BriefUebersicht } from "@/lib/briefkasten";

/**
 * Ein Umschlag im Briefkasten.
 *
 * Der Ablauf beim Öffnen hat drei Schläge, die aufeinander warten:
 *   1. das Siegel bricht (0 ms)
 *   2. die Klappe klappt in echtem 3D nach hinten (180 ms)
 *   3. der Brief schiebt sich heraus (620 ms) — danach der Seitenwechsel
 *
 * Die Tiefensortierung läuft über echte translateZ-Werte, nicht über
 * z-index: innerhalb von `transform-style: preserve-3d` wird z-index
 * ignoriert. Deshalb schiebt sich der Brief per Z-Wert an der Klappe vorbei.
 */

const MONAT_KURZ = [
  "Jan", "Feb", "Mär", "Apr", "Mai", "Jun",
  "Jul", "Aug", "Sep", "Okt", "Nov", "Dez",
];

export function Umschlag({ brief, index }: { brief: BriefUebersicht; index: number }) {
  const router = useRouter();
  const [offen, setOffen] = useState(false);
  const [gebrochen, setGebrochen] = useState(false);
  const [wackelt, setWackelt] = useState(false);
  const laeuft = useRef(false);

  const verschlossen = brief.status === "verschlossen";
  const bereit = brief.status === "bereit";
  const geoeffnet = brief.status === "geoeffnet";

  const [, monatNr] = brief.unlock.split("-").map(Number);
  const jahr = brief.unlock.slice(2, 4);

  const oeffnen = useCallback(async () => {
    if (laeuft.current) return;

    if (verschlossen) {
      setWackelt(true);
      setTimeout(() => setWackelt(false), 520);
      if (typeof navigator !== "undefined" && "vibrate" in navigator) navigator.vibrate?.(12);
      return;
    }

    // Schon geöffnet: direkt hinspringen, ohne die Show nochmal abzuspielen.
    if (geoeffnet) {
      router.push(`/brief/${brief.id}`);
      return;
    }

    laeuft.current = true;
    const sparsam = window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    if (typeof navigator !== "undefined" && "vibrate" in navigator) navigator.vibrate?.([8, 40, 14]);

    // Serverseitig festhalten, dass dieser Brief jetzt geöffnet wurde.
    // Wenn das schiefgeht, wird trotzdem weitergeleitet — die Seite selbst
    // prüft die Freischaltung ohnehin nochmal.
    const gespeichert = fetch(`/api/briefe/${brief.id}/oeffnen`, { method: "POST" }).catch(
      () => undefined,
    );

    if (sparsam) {
      await gespeichert;
      router.push(`/brief/${brief.id}?frisch=1`);
      return;
    }

    setGebrochen(true);
    setTimeout(() => setOffen(true), 180);
    setTimeout(async () => {
      await gespeichert;
      router.push(`/brief/${brief.id}?frisch=1`);
    }, 1500);
  }, [brief.id, geoeffnet, router, verschlossen]);

  const status = verschlossen ? "verschlossen" : geoeffnet ? "geoeffnet" : "bereit";

  const beschriftung = verschlossen
    ? `Brief ${brief.id}, ${brief.monat} — noch verschlossen`
    : geoeffnet
      ? `Brief ${brief.id}, ${brief.monat}: ${brief.titel ?? ""} — nochmal lesen`
      : `Brief ${brief.id}, ${brief.monat} — jetzt öffnen`;

  return (
    <div
      className="szene aufsteigen"
      style={{ animationDelay: `${Math.min(index, 12) * 55}ms` }}
    >
      <button
        type="button"
        onClick={oeffnen}
        aria-label={beschriftung}
        className={`group block w-full cursor-pointer appearance-none border-0 bg-transparent p-0 text-left ${
          wackelt ? "wackelt" : ""
        }`}
      >
        <div className="umschlag" data-offen={offen} data-status={status}>
          {/* Rückseite des Umschlags */}
          <div
            className="umschlag-flaeche u-ruecken korn overflow-hidden"
            style={{
              background: "var(--papier-tief)",
              boxShadow: "var(--schatten-karte)",
            }}
          />

          {/* Der Brief, der sich beim Öffnen herausschiebt */}
          <div
            className="umschlag-flaeche u-brief korn flex flex-col justify-center px-[9%] py-[7%]"
            style={{
              background: "var(--karte)",
              transform: offen ? "translateZ(64px) translateY(-42%)" : "translateZ(1px)",
              transition: "transform .95s cubic-bezier(.22,.61,.36,1) .44s",
              boxShadow: offen ? "var(--schatten-hoch)" : "none",
            }}
          >
            <span className="kapitaelchen">{brief.monat}</span>
            {brief.titel ? (
              <span className="hand mt-1 text-leise">{brief.titel}</span>
            ) : (
              // Vor dem Öffnen bleibt der Titel geheim — statt eines
              // Platzhalters steht hier das Motiv des Monats.
              <Motiv art={brief.stamp} className="mt-2 w-[22%] text-rot opacity-60" />
            )}
          </div>

          {/* Vorderseite mit Adresse, Marke und Stempel */}
          <div
            className="umschlag-flaeche u-front korn overflow-hidden"
            style={{
              background:
                "linear-gradient(168deg, var(--karte), color-mix(in oklab, var(--papier-tief) 55%, var(--karte)))",
              boxShadow: "inset 0 0 0 1px color-mix(in oklab, var(--linie) 70%, transparent)",
            }}
          >
            {/* Luftpost-Bordüre unten */}
            <div className="luftpost-rand absolute inset-x-0 bottom-0 h-[5px] opacity-70" />

            {/* Briefmarke und Entwertungsstempel.
                Beide sitzen bewusst unterhalb der V-Kerbe: die Vorderseite
                ist oben ausgeschnitten (dort schaut die Klappe heraus), und
                alles, was in diese Kerbe ragt, würde abgeschnitten. */}
            <Marke art={brief.stamp} className="absolute right-[4%] top-[18%] block w-[13%]" />
            <Poststempel
              text={`${MONAT_KURZ[monatNr - 1]} ${jahr}`}
              className="absolute right-[12%] top-[29%] w-[15%]"
            />

            {/* Adressfeld. Verschlossen fällt alles weg, was Platz kostet,
                damit die Banderole darüber frei liegen kann. */}
            <div className="absolute bottom-[9%] left-[7%] w-[64%]">
              {!verschlossen && <span className="kapitaelchen text-[0.55rem]">An</span>}
              <p className="hand -mt-0.5 text-[0.95rem] leading-tight text-tinte sm:text-[1.1rem]">
                Marcolino Popolino
              </p>
              {!verschlossen && (
                <div className="mt-1.5 space-y-[5px]">
                  <span className="block h-px w-[84%] bg-linie" />
                  <span className="block h-px w-[62%] bg-linie" />
                </div>
              )}
            </div>

            {/* Laufende Nummer unten rechts. Oben ginge nicht: dort liegt
                die Klappe darüber und würde sie zur Hälfte verdecken. */}
            <span className="kapitaelchen absolute bottom-[8%] right-[5%] whitespace-nowrap text-[0.52rem] opacity-70">
              Nr. {String(brief.id).padStart(2, "0")} / 13
            </span>
          </div>

          {/* Die Klappe */}
          <div
            className="umschlag-flaeche u-klappe korn"
            style={{
              background:
                "linear-gradient(180deg, color-mix(in oklab, var(--papier-tief) 42%, var(--karte)), var(--karte))",
              boxShadow: "inset 0 1px 0 color-mix(in oklab, #fff 30%, transparent)",
            }}
          />

          {/* Siegel — bewusst Geschwister der Klappe, nicht Kind:
              sonst würde es beim Aufklappen auf den Rücken fallen. */}
          <div
            className="absolute left-1/2 top-[42%] w-[18%] -translate-x-1/2 -translate-y-1/2"
            style={{ transform: "translate(-50%,-50%) translateZ(4px)" }}
          >
            <Siegel gebrochen={gebrochen} initialen={geoeffnet ? "7" : "B&M"} />
          </div>

          {/* Noch verschlossen: eine Banderole quer über dem Umschlag.
              Sie sitzt bewusst zwischen Siegel und Adressfeld, damit sie
              nichts überdeckt, was man lesen können soll. */}
          {verschlossen && (
            <>
              <div
                className="umschlag-flaeche pointer-events-none"
                style={{
                  transform: "translateZ(5px)",
                  background: "color-mix(in oklab, var(--papier) 38%, transparent)",
                }}
              />
              <div
                className="banderole pointer-events-none absolute inset-x-0 top-[43%] flex flex-col items-center py-[3.2%]"
                style={{ transform: "translateZ(6px)" }}
              >
                <span className="kapitaelchen text-[0.5rem] leading-tight">öffnet sich am</span>
                <span className="font-display text-[0.95rem] font-semibold leading-snug">
                  10. {brief.monat}
                </span>
                <Countdown zielMs={brief.unlockMs} kompakt />
              </div>
            </>
          )}
        </div>
      </button>

      {/* Bildunterschrift unter dem Umschlag */}
      <div className="mt-3 flex items-baseline justify-between gap-2 px-1">
        <p className="min-w-0 truncate font-display text-[1.05rem] font-semibold">
          {geoeffnet || brief.titel ? brief.titel : "Noch ein Geheimnis"}
        </p>
        <span className="kapitaelchen shrink-0 text-[0.58rem]">
          {geoeffnet ? "gelesen" : bereit ? "bereit" : brief.monat.split(" ")[0]}
        </span>
      </div>
      {brief.vorschau && (
        <p className="kapitaelchen mt-0.5 px-1 text-[0.55rem] text-rot">Vorschau</p>
      )}
    </div>
  );
}
