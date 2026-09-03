import type { Foto } from "@/lib/fotos";

/**
 * Ein Polaroid mit Klebeband.
 *
 * Das Bild wird in seinem echten Seitenverhältnis gezeigt — hochkant wie
 * quer, nichts wird beschnitten. Die Abmessungen kommen vom Server, deshalb
 * steht der Rahmen schon in der richtigen Form da, bevor das Bild geladen
 * ist: die Seite springt beim Laden nicht.
 *
 * Ist kein Foto hinterlegt, steht hier ein gezeichneter Platzhalter — die
 * Seite sieht also von Anfang an fertig aus und wird schöner, sobald Bella
 * Bilder in `public/fotos/` legt.
 */
export function FotoPlatz({
  foto,
  bildunterschrift,
  className = "",
}: {
  foto: Foto | null;
  bildunterschrift: string;
  className?: string;
}) {
  // Die Breite wird aus der gewünschten Bildhöhe zurückgerechnet, damit ein
  // hochkantes Handyfoto und ein Querformat nebeneinander gleich viel Gewicht
  // haben — sonst überragt das Hochformat die halbe Seite.
  const ZIELHOEHE = 25;
  const breiteRem = foto
    ? Math.min(22, Math.max(12, (ZIELHOEHE * foto.breite) / foto.hoehe))
    : 20;

  return (
    <figure
      className={`korn relative mx-auto w-full max-w-[20rem] rounded-[2px] bg-karte p-3 pb-10 shadow-hoch sm:max-w-[var(--polaroid-breite)] ${className}`}
      style={{
        transform: "rotate(-1.6deg)",
        ["--polaroid-breite" as string]: `${breiteRem.toFixed(2)}rem`,
      }}
    >
      <span
        aria-hidden="true"
        className="klebeband absolute -top-3 left-1/2 h-6 w-24 -translate-x-1/2 -rotate-2 rounded-[1px]"
      />

      <div
        className="relative overflow-hidden bg-papier-tief"
        style={{ aspectRatio: foto ? `${foto.breite} / ${foto.hoehe}` : "4 / 3" }}
      >
        {foto ? (
          // eslint-disable-next-line @next/next/no-img-element
          <img
            src={foto.quelle}
            width={foto.breite}
            height={foto.hoehe}
            alt={`Bella und Marco — ${bildunterschrift}`}
            className="h-full w-full object-cover"
            loading="lazy"
            decoding="async"
          />
        ) : (
          <div className="flex h-full w-full flex-col items-center justify-center gap-2 text-fluestern">
            <svg viewBox="0 0 48 48" className="w-10" aria-hidden="true">
              <rect
                x="7" y="12" width="34" height="26" rx="2.5"
                fill="none" stroke="currentColor" strokeWidth="1.6"
              />
              <circle cx="24" cy="24" r="7" fill="none" stroke="currentColor" strokeWidth="1.6" />
              <circle cx="24" cy="24" r="2.4" fill="currentColor" opacity="0.5" />
              <path
                d="M17 12l2.5-4h9l2.5 4"
                fill="none" stroke="currentColor" strokeWidth="1.6" strokeLinejoin="round"
              />
            </svg>
            <span className="kapitaelchen text-[0.5rem]">Platz für ein Foto</span>
          </div>
        )}
      </div>

      <figcaption className="hand absolute inset-x-0 bottom-1.5 text-center text-[1.1rem] text-leise">
        {bildunterschrift}
      </figcaption>
    </figure>
  );
}
