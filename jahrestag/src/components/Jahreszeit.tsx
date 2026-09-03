import type { Season } from "@/lib/types";

/**
 * Vier gezeichnete Landschaften — eine pro Jahreszeit — als Kopfbild über
 * jedem Brief. Alles Vektor: skaliert von 360px Handy bis 4K ohne Unschärfe
 * und ohne eine einzige Bilddatei.
 */

const HIMMEL: Record<Season, [string, string, string]> = {
  herbst: ["#f6d9b0", "#efb98a", "#d98b6a"],
  winter: ["#dbe6f0", "#c3d3e6", "#a9bcd6"],
  fruehling: ["#e8f0d8", "#cfe3c4", "#f3d9e2"],
  sommer: ["#ffe6b8", "#ffc98f", "#f6a97f"],
};

const HUEGEL: Record<Season, [string, string, string]> = {
  herbst: ["#b8763f", "#8d5730", "#5f3a24"],
  winter: ["#a9b6c6", "#7e8ea3", "#4f5c72"],
  fruehling: ["#9dbf7a", "#6f9a58", "#47693c"],
  sommer: ["#c8b06a", "#9a8c48", "#63612f"],
};

function Baum({ x, y, s, season }: { x: number; y: number; s: number; season: Season }) {
  const laub = { herbst: "#c4622f", winter: "#8c99ab", fruehling: "#7fae63", sommer: "#5f8f4a" }[season];
  return (
    <g transform={`translate(${x} ${y}) scale(${s})`}>
      <path d="M0 0v-26" stroke="#4a3526" strokeWidth="3.2" strokeLinecap="round" fill="none" />
      <path d="M0-18-8-26m8 3 7-8" stroke="#4a3526" strokeWidth="2.2" strokeLinecap="round" fill="none" />
      {season === "winter" ? null : (
        <>
          <circle cx="0" cy="-31" r="12" fill={laub} opacity="0.92" />
          <circle cx="-9" cy="-25" r="8.5" fill={laub} opacity="0.8" />
          <circle cx="9" cy="-24" r="8" fill={laub} opacity="0.86" />
        </>
      )}
    </g>
  );
}

export function Jahreszeit({ season, className = "" }: { season: Season; className?: string }) {
  const [h1, h2, h3] = HIMMEL[season];
  const [g1, g2, g3] = HUEGEL[season];
  const id = `jz-${season}`;

  return (
    <svg
      viewBox="0 0 800 260"
      preserveAspectRatio="xMidYMax slice"
      className={className}
      aria-hidden="true"
    >
      <defs>
        <linearGradient id={`${id}-himmel`} x1="0" y1="0" x2="0" y2="1">
          <stop offset="0%" stopColor={h1} />
          <stop offset="58%" stopColor={h2} />
          <stop offset="100%" stopColor={h3} />
        </linearGradient>
        <radialGradient id={`${id}-glut`} cx="50%" cy="50%" r="50%">
          <stop offset="0%" stopColor="#fff4d8" stopOpacity="0.95" />
          <stop offset="100%" stopColor="#fff4d8" stopOpacity="0" />
        </radialGradient>
      </defs>

      <rect width="800" height="260" fill={`url(#${id}-himmel)`} />

      {/* Sonne bzw. Mond */}
      <circle cx="612" cy="86" r="74" fill={`url(#${id}-glut)`} />
      <circle cx="612" cy="86" r="27" fill={season === "winter" ? "#f4f7fb" : "#fff1c9"} opacity="0.95" />

      {/* Wolken / Sterne */}
      {season === "winter" ? (
        <g fill="#ffffff" opacity="0.75">
          {[
            [90, 40], [150, 66], [240, 34], [330, 58], [420, 30], [500, 62], [700, 44], [760, 74],
          ].map(([cx, cy], i) => (
            <circle key={i} cx={cx} cy={cy} r={i % 3 === 0 ? 2.2 : 1.4} />
          ))}
        </g>
      ) : (
        <g fill="#ffffff" opacity="0.42">
          <ellipse cx="150" cy="58" rx="52" ry="15" />
          <ellipse cx="186" cy="50" rx="34" ry="12" />
          <ellipse cx="392" cy="38" rx="40" ry="11" />
        </g>
      )}

      {/* Vögel */}
      <g stroke="#5b4632" strokeWidth="1.6" fill="none" opacity="0.55" strokeLinecap="round">
        <path d="M232 96c4-5 8-5 12 0 4-5 8-5 12 0" />
        <path d="M270 78c3-4 6-4 9 0 3-4 6-4 9 0" />
      </g>

      {/* Hügelketten */}
      <path d="M0 176c120-34 210 12 320-8s180-46 300-16 180 20 180 20v88H0Z" fill={g1} opacity="0.9" />
      <path d="M0 206c150-40 250 6 372-12s208-30 330-6 98 14 98 14v58H0Z" fill={g2} opacity="0.95" />
      <path d="M0 236c180-26 280 8 420-6s240-18 380-4v34H0Z" fill={g3} />

      {/* Bäume */}
      <Baum x={88} y={228} s={1.15} season={season} />
      <Baum x={132} y={236} s={0.85} season={season} />
      <Baum x={694} y={232} s={1} season={season} />
      <Baum x={742} y={240} s={0.72} season={season} />

      {/* Jahreszeitliche Kleinigkeiten */}
      {season === "herbst" && (
        <g fill="#c4622f" opacity="0.75">
          {[[210, 150], [265, 186], [318, 132], [372, 174], [430, 148], [488, 192]].map(([x, y], i) => (
            <ellipse key={i} cx={x} cy={y} rx="4.5" ry="3" transform={`rotate(${i * 37} ${x} ${y})`} />
          ))}
        </g>
      )}
      {season === "winter" && (
        <g fill="#ffffff" opacity="0.85">
          {[[180, 140], [250, 178], [330, 122], [400, 166], [470, 138], [540, 184], [610, 150]].map(
            ([x, y], i) => (
              <circle key={i} cx={x} cy={y} r={i % 2 ? 2.4 : 1.8} />
            ),
          )}
          <path d="M0 236c180-26 280 8 420-6s240-18 380-4v10c-140-12-240-4-380 4S180 222 0 246Z" fill="#ffffff" opacity="0.55" />
        </g>
      )}
      {season === "fruehling" && (
        <g opacity="0.8">
          {[[196, 214], [242, 222], [286, 210], [452, 218], [500, 210], [548, 220]].map(([x, y], i) => (
            <g key={i} transform={`translate(${x} ${y})`}>
              <circle r="3.2" fill={i % 2 ? "#f2b8cb" : "#fdf3d0"} />
              <circle r="1.1" fill="#e8a33c" />
            </g>
          ))}
        </g>
      )}
      {season === "sommer" && (
        <g stroke="#fff1c9" strokeWidth="1.4" opacity="0.5" fill="none" strokeLinecap="round">
          <path d="M120 214h84m-64 12h64m-40 12h72" />
        </g>
      )}
    </svg>
  );
}
