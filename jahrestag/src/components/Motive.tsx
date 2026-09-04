import type { StampMotif } from "@/lib/types";

/**
 * Dreizehn kleine Strichzeichnungen — für jeden Monat eine.
 * Bewusst als SVG und nicht als Bilddatei: sie nehmen die Farbe der Umgebung
 * an (`currentColor`), sind gestochen scharf auf jedem Display und die App
 * lädt dafür kein einziges Byte nach.
 */

const strich = {
  fill: "none",
  stroke: "currentColor",
  strokeWidth: 1.6,
  strokeLinecap: "round",
  strokeLinejoin: "round",
} as const;

const ZEICHNUNGEN: Record<StampMotif, React.ReactNode> = {
  herz: (
    <>
      <path {...strich} d="M24 34.5c-6.4-4.6-11-8.3-11-13.2a5.6 5.6 0 0 1 11-2 5.6 5.6 0 0 1 11 2c0 4.9-4.6 8.6-11 13.2Z" />
      <path {...strich} strokeWidth={1} opacity={0.5} d="M18.4 20.6a3 3 0 0 1 2.8-2.4" />
    </>
  ),
  blatt: (
    <>
      <path {...strich} d="M14 34C14 22 22 14 34 14c0 12-8 20-20 20Z" />
      <path {...strich} d="M16.5 31.5 31 17" />
      <path {...strich} strokeWidth={1} opacity={0.6} d="M22 26h5m-3.5-4.5h5M20.5 30h4" />
    </>
  ),
  mond: (
    <>
      <path {...strich} d="M29.5 12.5A11.5 11.5 0 1 0 35 26.8a9 9 0 0 1-5.5-14.3Z" />
      <path {...strich} strokeWidth={1} d="M34.5 13.5v3m1.5-1.5h-3M14 17v2m1-1h-2" />
    </>
  ),
  tanne: (
    <>
      <path {...strich} d="M24 11 16 21h4l-6 8h8v6h4v-6h8l-6-8h4Z" />
      <path {...strich} strokeWidth={1} opacity={0.6} d="M11 33h4m18 0h4" />
    </>
  ),
  schlittschuh: (
    <>
      <path {...strich} d="M18 12h4v13l7 3c1.7.7 2.6 1.9 2.6 3.6V33H18Z" />
      <path {...strich} d="M14 36.5h22" />
      <path {...strich} strokeWidth={1} d="M16 33v3.5m20-1.5v1.5" />
      <path {...strich} strokeWidth={1} opacity={0.6} d="M22 17h3m-3 4h4m-4 4h5" />
    </>
  ),
  tasse: (
    <>
      <path {...strich} d="M13 19h20v9a8 8 0 0 1-8 8h-4a8 8 0 0 1-8-8Z" />
      <path {...strich} d="M33 21.5h3.2a3.8 3.8 0 0 1 0 7.6H33" />
      <path {...strich} strokeWidth={1.2} opacity={0.65} d="M19 15c1.4-1.4 1.4-2.6 0-4m5 4c1.4-1.4 1.4-2.6 0-4m5 4c1.4-1.4 1.4-2.6 0-4" />
    </>
  ),
  tulpe: (
    <>
      <path {...strich} d="M24 24c-4 0-6-2.6-6-6.5 0 0 2.4 1.6 3.4 3.2.4-2.4 1.4-4.4 2.6-5.7 1.2 1.3 2.2 3.3 2.6 5.7 1-1.6 3.4-3.2 3.4-3.2 0 3.9-2 6.5-6 6.5Z" />
      <path {...strich} d="M24 24v13" />
      <path {...strich} d="M24 30c-3.5 0-5.5-1.6-6-4.5 3 0 5.2 1.4 6 4.5Zm0 3.5c3.5 0 5.5-1.6 6-4.5-3 0-5.2 1.4-6 4.5Z" />
    </>
  ),
  pasta: (
    <>
      <path {...strich} d="M12 24a12 12 0 0 1 24 0Z" />
      <path {...strich} d="M9.5 24h29" />
      <path {...strich} strokeWidth={1.2} d="M17 20.5c1.6-1.8 3.4-1.8 5 0s3.4 1.8 5 0 3.4-1.8 5 0" />
      <path {...strich} strokeWidth={1.2} opacity={0.6} d="M15 28.5c2.2 0 2.2 2 4.4 2s2.2-2 4.4-2 2.2 2 4.4 2 2.2-2 4.4-2" />
    </>
  ),
  fahrrad: (
    <>
      <circle {...strich} cx={15} cy={29} r={6.5} />
      <circle {...strich} cx={33} cy={29} r={6.5} />
      <path {...strich} d="m15 29 5.5-10H26l4 10M20.5 19h6.5m-6.5 0-2 10h14" />
      <path {...strich} strokeWidth={1.2} d="M29 19h3.5" />
    </>
  ),
  sonne: (
    <>
      <circle {...strich} cx={24} cy={24} r={7.5} />
      <path {...strich} d="M24 10v3.5m0 21V38M10 24h3.5m21 0H38m-9.9-9.9 2.5-2.5M15.4 32.6l-2.5 2.5m0-22.5 2.5 2.5m17.2 17.1 2.5 2.5" />
    </>
  ),
  welle: (
    <>
      <path {...strich} d="M10 21c2.8-3 5.6-3 8.4 0s5.6 3 8.4 0 5.6-3 8.4 0" />
      <path {...strich} d="M10 27c2.8-3 5.6-3 8.4 0s5.6 3 8.4 0 5.6-3 8.4 0" />
      <path {...strich} opacity={0.6} d="M10 33c2.8-3 5.6-3 8.4 0s5.6 3 8.4 0 5.6-3 8.4 0" />
      <circle {...strich} strokeWidth={1.2} cx={32} cy={14} r={3.4} />
    </>
  ),
  sternschnuppe: (
    <>
      <path {...strich} d="m33 15-1.6 4.6L27 21l4.4 1.4L33 27l1.6-4.6L39 21l-4.4-1.4Z" />
      <path {...strich} strokeWidth={1.2} d="M28 24 12 36m14-8L13 31m9-1-8 2.5" />
      <path {...strich} strokeWidth={1} opacity={0.6} d="M15 15.5v2.6m1.3-1.3h-2.6M22 12v2m1-1h-2" />
    </>
  ),
  kino: (
    <>
      <rect {...strich} x={9} y={13} width={30} height={22} rx={2} />
      <path {...strich} strokeWidth={1} d="M9 19h5m-5 5h5m-5 5h5m20-10h5m-5 5h5m-5 5h5" />
      <path {...strich} strokeWidth={1.3} d="M20 21.5v6l6-3Z" />
    </>
  ),
  kaffee: (
    <>
      <path {...strich} d="M15 20h16v6.5a7 7 0 0 1-7 7h-2a7 7 0 0 1-7-7Z" />
      <path {...strich} d="M31 22h2.6a3.2 3.2 0 0 1 0 6.4H31" />
      <path {...strich} d="M11 36.5h26" />
      <path {...strich} strokeWidth={1.2} opacity={0.65} d="M20 16.5c1.2-1.2 1.2-2.3 0-3.5m4.5 3.5c1.2-1.2 1.2-2.3 0-3.5m4.5 3.5c1.2-1.2 1.2-2.3 0-3.5" />
    </>
  ),
  berg: (
    <>
      <path {...strich} d="M8 34 20 15l6.5 10L30 20l10 14Z" />
      <path {...strich} strokeWidth={1.2} d="m16.2 21.5 3.8 3 3.8-3" />
      <path {...strich} strokeWidth={1} opacity={0.6} d="M33 12.5v2.6m1.3-1.3h-2.6" />
    </>
  ),
  korb: (
    <>
      <path {...strich} d="M11 21h26l-2.6 14a2.5 2.5 0 0 1-2.5 2H16.1a2.5 2.5 0 0 1-2.5-2Z" />
      <path {...strich} d="M18 21a6 6 0 0 1 12 0" />
      <path {...strich} strokeWidth={1} opacity={0.6} d="M17.5 25.5 19 33m10-7.5L27.5 33M23.5 25.5V33" />
      <path {...strich} strokeWidth={1.2} d="M12.5 27h23" />
    </>
  ),
  pinsel: (
    <>
      <path {...strich} d="M24 12c7.2 0 13 5 13 11 0 3.4-2.6 5-5.4 5-2.2 0-3 1.2-3 2.6 0 1.8 1.4 2.4 1.4 3.9 0 1.5-1.6 2.5-6 2.5-7.2 0-13-5.4-13-12.5S16.8 12 24 12Z" />
      <circle cx={18.5} cy={20.5} r={1.7} fill="currentColor" />
      <circle cx={25} cy={18} r={1.7} fill="currentColor" />
      <circle cx={30.5} cy={22.5} r={1.7} fill="currentColor" />
      <circle cx={19} cy={28} r={1.7} fill="currentColor" />
    </>
  ),
  tier: (
    <>
      <path {...strich} d="M14 18.5 12.5 11l6.5 3.6m10 0L35.5 11 34 18.5" />
      <path {...strich} d="M24 13.5c6 0 10 4.4 10 10.5 0 6.4-4.4 12-10 12s-10-5.6-10-12c0-6.1 4-10.5 10-10.5Z" />
      <circle cx={20} cy={24} r={1.5} fill="currentColor" />
      <circle cx={28} cy={24} r={1.5} fill="currentColor" />
      <path {...strich} strokeWidth={1.3} d="M24 28.5v2m-2.5 1.5a3.4 3.4 0 0 0 5 0" />
    </>
  ),
  album: (
    <>
      <rect {...strich} x={10} y={12} width={28} height={24} rx={2} />
      <path {...strich} d="M15 12v24" />
      <rect {...strich} strokeWidth={1.2} x={20} y={17} width={13} height={9.5} rx={1} />
      <path {...strich} strokeWidth={1.2} d="M26.5 33.5c-2.6-1.9-4.4-3.4-4.4-5.3a2.3 2.3 0 0 1 4.4-.8 2.3 2.3 0 0 1 4.4.8c0 1.9-1.8 3.4-4.4 5.3Z" />
    </>
  ),
  traube: (
    <>
      <circle {...strich} cx={24} cy={20} r={3} />
      <circle {...strich} cx={19} cy={25.5} r={3} />
      <circle {...strich} cx={29} cy={25.5} r={3} />
      <circle {...strich} cx={24} cy={30} r={3} />
      <circle {...strich} cx={24} cy={38} r={0.4} />
      <path {...strich} d="M24 17v-4c2.5 0 4.5-1 6-3" />
      <path {...strich} strokeWidth={1.2} d="M27 13c2.6-.6 4.4.2 5.5 2.4-2.6.7-4.4-.1-5.5-2.4Z" />
    </>
  ),
};

export function Motiv({ art, className }: { art: StampMotif; className?: string }) {
  return (
    <svg viewBox="0 0 48 48" className={className} aria-hidden="true">
      {ZEICHNUNGEN[art]}
    </svg>
  );
}
