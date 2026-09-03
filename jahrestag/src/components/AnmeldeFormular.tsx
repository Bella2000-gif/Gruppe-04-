"use client";

import { useRouter } from "next/navigation";
import { useState } from "react";

export function AnmeldeFormular() {
  const router = useRouter();
  const [code, setCode] = useState("");
  const [fehler, setFehler] = useState<string | null>(null);
  const [laedt, setLaedt] = useState(false);

  async function absenden(e: React.FormEvent) {
    e.preventDefault();
    if (laedt || code.trim() === "") return;
    setLaedt(true);
    setFehler(null);

    const antwort = await fetch("/api/sitzung", {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ code }),
    }).catch(() => null);

    if (antwort?.ok) {
      router.replace("/");
      router.refresh();
      return;
    }

    const daten = (await antwort?.json().catch(() => null)) as { fehler?: string } | null;
    setFehler(daten?.fehler ?? "Das hat gerade nicht geklappt.");
    setLaedt(false);
  }

  return (
    <form onSubmit={absenden} className="w-full">
      <label htmlFor="code" className="kapitaelchen mb-2 block text-center">
        Das Wort, das nur ihr zwei kennt
      </label>
      <input
        id="code"
        name="code"
        type="password"
        autoComplete="current-password"
        autoCapitalize="none"
        autoCorrect="off"
        value={code}
        onChange={(e) => setCode(e.target.value)}
        aria-invalid={Boolean(fehler)}
        aria-describedby={fehler ? "code-fehler" : undefined}
        className="korn relative w-full rounded-sm border border-linie bg-karte px-4 py-3 text-center font-hand text-2xl text-tinte shadow-karte outline-none transition placeholder:text-fluestern/60 focus:border-blau"
        placeholder="· · · · · ·"
      />

      {fehler && (
        <p id="code-fehler" role="alert" className="mt-3 text-center text-sm text-rot">
          {fehler}
        </p>
      )}

      <button
        type="submit"
        disabled={laedt || code.trim() === ""}
        className="mt-5 w-full cursor-pointer rounded-sm bg-tinte px-4 py-3 font-stempel text-xs uppercase tracking-[0.2em] text-papier transition hover:opacity-90 disabled:cursor-not-allowed disabled:opacity-35"
      >
        {laedt ? "Einen Moment …" : "Briefkasten öffnen"}
      </button>
    </form>
  );
}
