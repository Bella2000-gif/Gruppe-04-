/**
 * Ein Polaroid mit Klebeband.
 *
 * Ob ein Foto existiert, hat schon der Server geprüft (siehe `lib/fotos.ts`).
 * Ist keins da, steht hier ein gezeichneter Platzhalter — die Seite sieht
 * also von Anfang an fertig aus und wird schöner, sobald Bella Bilder in
 * `public/fotos/` legt.
 */
export function FotoPlatz({
  quelle,
  bildunterschrift,
  className = "",
}: {
  quelle: string | null;
  bildunterschrift: string;
  className?: string;
}) {
  return (
    <figure
      className={`korn relative rounded-[2px] bg-karte p-3 pb-10 shadow-hoch ${className}`}
      style={{ transform: "rotate(-1.6deg)" }}
    >
      <span
        aria-hidden="true"
        className="klebeband absolute -top-3 left-1/2 h-6 w-24 -translate-x-1/2 -rotate-2 rounded-[1px]"
      />

      <div className="relative aspect-[4/3] overflow-hidden bg-papier-tief">
        {quelle ? (
          // eslint-disable-next-line @next/next/no-img-element
          <img
            src={quelle}
            alt={bildunterschrift}
            className="h-full w-full object-cover"
            loading="lazy"
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
