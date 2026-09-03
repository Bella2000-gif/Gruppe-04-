/**
 * Blütenblätter, die einmalig durchs Bild segeln — genau dann, wenn ein
 * Brief zum ersten Mal aufgeht. Kein Canvas, keine Bibliothek: 28 kleine
 * SVGs, die per CSS fallen und sich dabei drehen.
 *
 * Die Streuung ist bewusst nicht zufällig, sondern berechnet: dadurch ist die
 * Komponente rein (gleiche Eingabe → gleiche Ausgabe), es gibt keinen
 * Unterschied zwischen Server- und Browser-Darstellung, und sie sieht in
 * jedem Umschlag trotzdem anders aus.
 *
 * `prefers-reduced-motion` blendet die ganze Schicht per CSS aus — dafür
 * braucht es kein JavaScript.
 */

const FARBEN = ["#e8a3a8", "#f0c9a8", "#d98b6a", "#c9a227", "#c1453c", "#efd9c0"];
const ANZAHL = 28;

/** Deterministisches Rauschen: gleicher Index, gleicher Wert — aber gut gestreut. */
function streu(i: number, versatz: number): number {
  const x = Math.sin(i * 12.9898 + versatz * 78.233) * 43758.5453;
  return x - Math.floor(x);
}

const BLAETTER = Array.from({ length: ANZAHL }, (_, i) => ({
  links: streu(i, 1) * 100,
  verzug: streu(i, 2) * 900,
  laufzeit: 2600 + streu(i, 3) * 1800,
  groesse: 9 + streu(i, 4) * 11,
  drift: (streu(i, 5) - 0.5) * 220,
  drehung: (streu(i, 6) - 0.5) * 900,
  farbe: FARBEN[i % FARBEN.length],
}));

export function Konfetti({ aktiv }: { aktiv: boolean }) {
  if (!aktiv) return null;

  return (
    <div aria-hidden="true" className="konfetti-schicht">
      {BLAETTER.map((b, i) => (
        <svg
          key={i}
          viewBox="0 0 20 20"
          width={b.groesse}
          height={b.groesse}
          style={{
            position: "absolute",
            top: 0,
            left: `${b.links}%`,
            ["--drift" as string]: `${b.drift}px`,
            ["--dreh" as string]: `${b.drehung}deg`,
            animation: `blatt-fall ${b.laufzeit}ms cubic-bezier(.35,.1,.4,1) ${b.verzug}ms both`,
          }}
        >
          <path
            d="M10 1c4.5 3 6.5 6.4 6.5 9.5A6.5 6.5 0 0 1 10 19a6.5 6.5 0 0 1-6.5-8.5C3.5 7.4 5.5 4 10 1Z"
            fill={b.farbe}
            opacity="0.88"
          />
        </svg>
      ))}
    </div>
  );
}
