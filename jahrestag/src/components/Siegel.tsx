"use client";

/**
 * Das Wachssiegel. Zwei Dinge machen den Unterschied zwischen „roter Kreis“
 * und „Siegellack“:
 *  1. eine unregelmäßige Kontur (feTurbulence + feDisplacementMap), weil
 *     echtes Wachs beim Pressen seitlich herausquillt, und
 *  2. ein Glanzlicht auf einer Kuppel (feSpecularLighting), damit es rund wirkt.
 *
 * Beim Öffnen bricht es in zwei Hälften auseinander und fällt weg.
 * Die Filter laufen nur einmal beim Zeichnen, nicht pro Animationsbild.
 */

export function Siegel({
  gebrochen = false,
  initialen = "B&M",
  className = "",
}: {
  gebrochen?: boolean;
  initialen?: string;
  className?: string;
}) {
  return (
    <div
      aria-hidden="true"
      className={`pointer-events-none relative aspect-square ${className}`}
      data-gebrochen={gebrochen}
    >
      {(["links", "rechts"] as const).map((seite) => (
        <div
          key={seite}
          className="absolute inset-0 transition-[transform,opacity] duration-700 ease-[cubic-bezier(.3,.1,.3,1)]"
          style={{
            clipPath:
              seite === "links"
                ? "polygon(0 0, 50% 0, 50% 100%, 0 100%)"
                : "polygon(50% 0, 100% 0, 100% 100%, 50% 100%)",
            transform: gebrochen
              ? seite === "links"
                ? "translate(-46%, 34%) rotate(-24deg)"
                : "translate(46%, 40%) rotate(21deg)"
              : "none",
            opacity: gebrochen ? 0 : 1,
          }}
        >
          <svg viewBox="0 0 100 100" className="h-full w-full drop-shadow-[0_5px_9px_rgba(0,0,0,0.35)]">
            <g filter="url(#wachs-relief)">
              <circle cx="50" cy="50" r="40" fill="var(--siegel)" filter="url(#wachs-kante)" />
              <circle cx="50" cy="50" r="40" fill="url(#wachs-verlauf)" filter="url(#wachs-kante)" />
              <circle
                cx="50"
                cy="50"
                r="31"
                fill="none"
                stroke="var(--siegel-tief)"
                strokeWidth="1.6"
                opacity="0.45"
              />
              <text
                x="50"
                y="50"
                textAnchor="middle"
                dominantBaseline="central"
                fill="var(--siegel-tief)"
                fillOpacity="0.72"
                style={{
                  fontFamily: "var(--font-display)",
                  fontSize: initialen.length > 3 ? 20 : 26,
                  fontWeight: 600,
                  letterSpacing: "0.02em",
                }}
              >
                {initialen}
              </text>
            </g>
          </svg>
        </div>
      ))}
    </div>
  );
}
