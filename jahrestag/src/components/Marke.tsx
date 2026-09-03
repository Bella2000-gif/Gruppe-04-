import { Motiv } from "./Motive";
import type { StampMotif } from "@/lib/types";

/**
 * Eine Briefmarke — komplett als SVG, inklusive echter Zähnung an allen vier
 * Kanten (die Löcher kommen aus der Maske in `SvgDefinitionen`). Dadurch
 * skaliert sie verlustfrei vom 40-Pixel-Vorschaubild bis zur großen Ansicht,
 * ohne dass eine einzige Bilddatei geladen wird.
 */
export function Marke({
  art,
  wert = "7",
  land = "BELLA & MARCO",
  className = "",
}: {
  art: StampMotif;
  wert?: string;
  land?: string;
  className?: string;
}) {
  return (
    <svg
      viewBox="-3 -3 66 86"
      className={className}
      aria-hidden="true"
      style={{ filter: "drop-shadow(0 1px 1.5px rgb(0 0 0 / 0.22))" }}
    >
      <g mask="url(#marken-zaehnung)">
        {/* Markenpapier, minimal wärmer als der Umschlag darunter */}
        <rect x="0" y="0" width="60" height="80" fill="var(--marke-papier)" />
        <rect
          x="0" y="0" width="60" height="80"
          fill="var(--luftpost-rot)" opacity="0.08"
        />
        {/* Innenrahmen */}
        <rect
          x="4" y="4" width="52" height="72"
          fill="none" stroke="var(--luftpost-rot)" strokeOpacity="0.55" strokeWidth="0.7"
        />
      </g>

      {/* Beschriftung oben */}
      <text
        x="30" y="13"
        textAnchor="middle"
        fill="var(--tinte-leise)"
        style={{ fontFamily: "var(--font-stempel)", fontSize: 4, letterSpacing: "0.06em" }}
      >
        {land}
      </text>

      {/* Motiv */}
      <g transform="translate(14 20) scale(0.67)" color="var(--luftpost-rot)">
        <Motiv art={art} />
      </g>

      {/* Wertangabe unten */}
      <text
        x="30" y="72"
        textAnchor="middle"
        fill="var(--luftpost-rot)"
        style={{ fontFamily: "var(--font-display)", fontSize: 13, fontWeight: 600 }}
      >
        {wert}
        <tspan
          dx="1.5"
          style={{ fontFamily: "var(--font-stempel)", fontSize: 4.4, letterSpacing: "0.12em" }}
        >
          JAHRE
        </tspan>
      </text>
    </svg>
  );
}

/**
 * Der runde Entwertungsstempel, der halb über der Marke liegt.
 * Auch er ist SVG — so bleiben die Proportionen bei jeder Größe gleich,
 * egal ob er 30 oder 90 Pixel breit ist.
 */
export function Poststempel({
  text,
  ort = "MIT LIEBE",
  className = "",
}: {
  text: string;
  ort?: string;
  className?: string;
}) {
  return (
    <svg viewBox="0 0 60 60" className={`poststempel ${className}`} aria-hidden="true">
      <circle
        cx="30" cy="30" r="27"
        fill="none" stroke="currentColor" strokeWidth="1.8"
      />
      <text
        x="30" y="22" textAnchor="middle" fill="currentColor"
        style={{ fontFamily: "var(--font-stempel)", fontSize: 4.6, letterSpacing: "0.16em" }}
      >
        {ort}
      </text>
      <line x1="15" y1="26" x2="45" y2="26" stroke="currentColor" strokeWidth="0.8" opacity="0.7" />
      <text
        x="30" y="35" textAnchor="middle" fill="currentColor"
        style={{ fontFamily: "var(--font-stempel)", fontSize: 7, fontWeight: 700, letterSpacing: "0.04em" }}
      >
        {text}
      </text>
      <line x1="15" y1="39" x2="45" y2="39" stroke="currentColor" strokeWidth="0.8" opacity="0.7" />
      <text
        x="30" y="47" textAnchor="middle" fill="currentColor"
        style={{ fontFamily: "var(--font-stempel)", fontSize: 4.6, letterSpacing: "0.16em" }}
      >
        POST
      </text>
    </svg>
  );
}
