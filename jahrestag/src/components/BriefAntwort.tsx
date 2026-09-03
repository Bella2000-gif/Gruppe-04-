"use client";

import { useEffect, useRef, useState } from "react";

/**
 * Aus dem Geschenk wird über das Jahr ein gemeinsames Tagebuch:
 * Marco kann abhaken, dass ein Date stattgefunden hat, und dazuschreiben,
 * wie es war. Gespeichert wird automatisch, kurz nachdem er aufhört zu tippen.
 */
export function BriefAntwort({
  briefId,
  erledigtAmInitial,
  notizInitial,
  nurLesen = false,
}: {
  briefId: number;
  erledigtAmInitial: string | null;
  notizInitial: string | null;
  nurLesen?: boolean;
}) {
  const [erledigt, setErledigt] = useState(Boolean(erledigtAmInitial));
  const [notiz, setNotiz] = useState(notizInitial ?? "");
  const [zustand, setZustand] = useState<"ruhig" | "speichert" | "gesichert" | "fehler">("ruhig");
  const ersterLauf = useRef(true);
  const feld = useRef<HTMLTextAreaElement>(null);

  // Textfeld wächst mit dem Text mit.
  useEffect(() => {
    const el = feld.current;
    if (!el) return;
    el.style.height = "auto";
    el.style.height = `${el.scrollHeight}px`;
  }, [notiz]);

  // Automatisch speichern, 900 ms nach dem letzten Tastendruck.
  useEffect(() => {
    if (nurLesen) return;
    if (ersterLauf.current) {
      ersterLauf.current = false;
      return;
    }
    setZustand("speichert");
    const t = setTimeout(async () => {
      const a = await fetch(`/api/briefe/${briefId}/notiz`, {
        method: "PUT",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ notiz }),
      }).catch(() => null);
      setZustand(a?.ok ? "gesichert" : "fehler");
    }, 900);
    return () => clearTimeout(t);
  }, [notiz, briefId, nurLesen]);

  async function erledigtWechseln() {
    if (nurLesen) return;
    const neu = !erledigt;
    setErledigt(neu);
    const a = await fetch(`/api/briefe/${briefId}/erledigt`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ erledigt: neu }),
    }).catch(() => null);
    if (!a?.ok) setErledigt(!neu);
  }

  return (
    <div className="korn relative rounded-sm border border-linie bg-karte p-5 shadow-karte sm:p-7">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <span className="kapitaelchen">Unser Tagebuch</span>
        <span
          className="font-stempel text-[0.6rem] text-fluestern"
          role="status"
          aria-live="polite"
        >
          {zustand === "speichert" && "speichert …"}
          {zustand === "gesichert" && "gesichert ✓"}
          {zustand === "fehler" && "nicht gespeichert"}
        </span>
      </div>

      <button
        type="button"
        onClick={erledigtWechseln}
        disabled={nurLesen}
        aria-pressed={erledigt}
        className="mt-4 flex w-full cursor-pointer items-center gap-3 rounded-sm border border-linie/70 bg-papier px-4 py-3 text-left transition hover:border-rot/50 disabled:cursor-default"
      >
        <span
          aria-hidden="true"
          className={`grid h-6 w-6 shrink-0 place-items-center rounded-full border-2 transition ${
            erledigt ? "border-rot bg-rot text-papier" : "border-linie text-transparent"
          }`}
        >
          <svg viewBox="0 0 16 16" className="w-3.5" fill="none" stroke="currentColor" strokeWidth="2.6" strokeLinecap="round" strokeLinejoin="round">
            <path d="m3 8.5 3.2 3.2L13 4.8" />
          </svg>
        </span>
        <span className="font-display text-[1.05rem] font-semibold">
          {erledigt ? "Haben wir gemacht." : "Date als erledigt markieren"}
        </span>
      </button>

      <label htmlFor={`notiz-${briefId}`} className="kapitaelchen mt-6 block">
        Wie war es?
      </label>
      <textarea
        id={`notiz-${briefId}`}
        ref={feld}
        value={notiz}
        readOnly={nurLesen}
        onChange={(e) => setNotiz(e.target.value)}
        rows={3}
        placeholder="Ein paar Sätze, damit wir uns in zehn Jahren noch daran erinnern …"
        className="liniert mt-2 w-full resize-none rounded-sm border border-linie/70 bg-papier px-4 py-2 font-hand text-[1.4rem] leading-[1.95rem] text-tinte outline-none transition placeholder:text-fluestern/70 placeholder:text-[1.1rem] focus:border-blau"
      />
    </div>
  );
}
