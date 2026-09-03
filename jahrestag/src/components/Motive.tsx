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
