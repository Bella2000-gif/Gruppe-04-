/**
 * Ein einziger versteckter SVG-Block ganz oben im Dokument, der die
 * Filter und Masken bereitstellt, die überall auf der Seite gebraucht werden.
 * So werden sie einmal definiert statt dreizehnmal wiederholt.
 */

/** Zähnungslöcher entlang der vier Kanten einer 60×80-Briefmarke. */
function Zaehnung() {
  const loecher: React.ReactElement[] = [];
  for (let x = 5; x <= 55; x += 5) {
    loecher.push(<circle key={`o${x}`} cx={x} cy={0} r={2.2} fill="#000" />);
    loecher.push(<circle key={`u${x}`} cx={x} cy={80} r={2.2} fill="#000" />);
  }
  for (let y = 5; y <= 75; y += 5) {
    loecher.push(<circle key={`l${y}`} cx={0} cy={y} r={2.2} fill="#000" />);
    loecher.push(<circle key={`r${y}`} cx={60} cy={y} r={2.2} fill="#000" />);
  }
  return <>{loecher}</>;
}

export function SvgDefinitionen() {
  return (
    <svg width="0" height="0" aria-hidden="true" className="absolute" focusable="false">
      <defs>
        {/* ── Briefmarke ──────────────────────────────────────────────
            Die Zähnung als Maske. Wichtig: `maskUnits="userSpaceOnUse"`,
            damit dieselbe Maske in jedem Marken-SVG mit dem gleichen
            viewBox (0 0 60 80) passt. */}
        <mask id="marken-zaehnung" maskUnits="userSpaceOnUse" x="-3" y="-3" width="66" height="86">
          <rect x="0" y="0" width="60" height="80" fill="#fff" />
          <Zaehnung />
        </mask>

        {/* ── Siegellack ──────────────────────────────────────────────
            1. unregelmäßige Kontur: echtes Wachs quillt beim Pressen
               seitlich heraus und wird nie ein sauberer Kreis
            2. Glanzlicht auf einer Kuppel, damit es rund und weich wirkt
            Beides läuft nur beim Zeichnen, nicht pro Animationsbild. */}
        <filter id="wachs-kante">
          <feTurbulence type="fractalNoise" baseFrequency="0.045" numOctaves="4" seed="7" result="t" />
          <feDisplacementMap in="SourceGraphic" in2="t" scale="6" xChannelSelector="R" yChannelSelector="G" />
        </filter>

        <filter id="wachs-relief" x="-25%" y="-25%" width="150%" height="150%">
          <feTurbulence type="fractalNoise" baseFrequency="1.1" numOctaves="3" seed="19" result="korn" />
          <feDisplacementMap in="SourceGraphic" in2="korn" scale="2.2" xChannelSelector="R" yChannelSelector="G" result="rau" />
          <feGaussianBlur in="rau" stdDeviation="0.5" result="weich" />
          <feSpecularLighting
            in="weich"
            surfaceScale="3.6"
            specularConstant="0.85"
            specularExponent="24"
            lightingColor="#ffd9d0"
            result="glanz"
          >
            <fePointLight x="-30" y="-55" z="110" />
          </feSpecularLighting>
          <feComposite in="glanz" in2="weich" operator="in" result="glanzMaske" />
          <feComposite in="glanzMaske" in2="rau" operator="arithmetic" k1="0" k2="1" k3="1" k4="0" />
        </filter>

        <radialGradient id="wachs-verlauf" cx="34%" cy="28%" r="78%">
          <stop offset="0%" stopColor="var(--siegel-hell)" stopOpacity="0.85" />
          <stop offset="52%" stopColor="var(--siegel)" stopOpacity="0" />
          <stop offset="100%" stopColor="var(--siegel-tief)" stopOpacity="0.7" />
        </radialGradient>
      </defs>
    </svg>
  );
}
