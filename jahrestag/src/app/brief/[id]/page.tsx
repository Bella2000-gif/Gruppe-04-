import Link from "next/link";
import { notFound, redirect } from "next/navigation";
import { aktuelleRolle } from "@/lib/auth";
import { briefZugriff, jetztFuer } from "@/lib/briefkasten";
import { LETTERS } from "@/lib/letters";
import { berlinerMitternacht, datumLang, zeitpunktLang } from "@/lib/zeit";
import { Jahreszeit } from "@/components/Jahreszeit";
import { Marke, Poststempel } from "@/components/Marke";
import { Motiv } from "@/components/Motive";
import { Countdown } from "@/components/Countdown";
import { FotoPlatz } from "@/components/FotoPlatz";
import { fotoFuer } from "@/lib/fotos";
import { BriefAntwort } from "@/components/BriefAntwort";
import { FrischGeoeffnet } from "@/components/FrischGeoeffnet";
import { Kopfzeile } from "@/components/Kopfzeile";

const MONAT_KURZ = ["Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"];

export default async function BriefSeite({
  params,
  searchParams,
}: {
  params: Promise<{ id: string }>;
  searchParams: Promise<{ zeit?: string; frisch?: string }>;
}) {
  const rolle = await aktuelleRolle();
  if (!rolle) redirect("/anmelden");

  const { id: idRoh } = await params;
  const { zeit, frisch } = await searchParams;
  const id = Number(idRoh);
  if (!Number.isInteger(id)) notFound();

  const jetzt = jetztFuer(rolle, zeit);
  const zugriff = await briefZugriff(id, rolle, jetzt);

  if (!zugriff.erlaubt) {
    if (zugriff.grund === "unbekannt") notFound();
    return <NochZu unlock={zugriff.unlock!} rolle={rolle} />;
  }

  const { brief, status, vorschau } = zugriff;
  const [, monatNr] = brief.unlock.split("-").map(Number);
  const vorher = LETTERS.find((l) => l.id === brief.id - 1);
  const nachher = LETTERS.find((l) => l.id === brief.id + 1);

  return (
    <div className="min-h-dvh">
      {frisch === "1" && <FrischGeoeffnet />}
      <Kopfzeile rolle={rolle} zeitreise={rolle === "bella" ? (zeit ?? null) : null} />

      {/* ─────────── Kopfbild ─────────── */}
      {/* Die Landschaft wird nach unten hin ausmaskiert statt mit einer
          Farbe überblendet — so verläuft sie in den Seitenhintergrund,
          ohne dass eine sichtbare Kante entsteht. */}
      <div className="relative h-[34vh] min-h-[210px] w-full overflow-hidden sm:h-[42vh]">
        <div
          className="korn absolute inset-0"
          style={{
            maskImage: "linear-gradient(to bottom, #000 38%, transparent 97%)",
            WebkitMaskImage: "linear-gradient(to bottom, #000 38%, transparent 97%)",
          }}
        >
          <Jahreszeit season={brief.season} className="absolute inset-0 h-full w-full" />
        </div>

        <Link
          href={vorschau || rolle === "bella" ? `/${zeit ? `?zeit=${zeit}` : ""}` : "/"}
          className="absolute left-5 top-5 inline-flex items-center gap-2 rounded-sm bg-papier/75 px-3 py-1.5 font-stempel text-[0.6rem] uppercase tracking-[0.18em] text-tinte backdrop-blur-sm transition hover:bg-papier sm:left-8"
        >
          ← Briefkasten
        </Link>
      </div>

      <main className="mx-auto -mt-24 w-full max-w-3xl px-5 pb-24 sm:-mt-32 sm:px-8">
        {vorschau && (
          <p className="mb-4 rounded-sm border border-rot/40 bg-karte px-4 py-2 text-center font-stempel text-[0.62rem] uppercase tracking-[0.16em] text-rot">
            Vorschau · dieser Brief geht erst am {datumLang(brief.unlock)} auf
          </p>
        )}

        {/* ─────────── Der Brief ─────────── */}
        <article className="korn aufsteigen relative rounded-sm border border-linie bg-karte px-6 py-10 shadow-hoch sm:px-14 sm:py-16">
          {/* Marke und Stempel oben rechts, wie auf echtem Briefpapier aufgeklebt */}
          <div className="absolute right-5 top-5 w-14 sm:right-9 sm:top-9 sm:w-[4.5rem]">
            <Marke art={brief.stamp} className="block w-full" />
            <Poststempel
              text={`${MONAT_KURZ[monatNr - 1]} ${brief.unlock.slice(2, 4)}`}
              className="absolute -left-[40%] top-[36%] w-[88%]"
            />
          </div>

          <header className="max-w-[75%]">
            <p className="kapitaelchen">
              Brief Nr. {String(brief.id).padStart(2, "0")} von 13
            </p>
            <p className="kapitaelchen mt-1 text-rot">{brief.monat}</p>
            <h1 className="mt-4 text-[clamp(2.4rem,7vw,4rem)]">{brief.titel}</h1>
          </header>

          <div className="raute-trenner my-9">
            <Motiv art={brief.stamp} className="w-6 text-rot" />
          </div>

          <p className="hand text-[1.75rem] leading-tight text-tinte">{brief.anrede}</p>

          <div className="mt-5 space-y-5 text-[1.06rem] leading-[1.75] text-tinte">
            {brief.absaetze.map((absatz, i) => (
              <p key={i} className={i === 0 ? "first-letter:float-left first-letter:mr-2 first-letter:font-display first-letter:text-[3.4rem] first-letter:leading-[0.82] first-letter:text-rot" : ""}>
                {absatz}
              </p>
            ))}
          </div>

          <div className="mt-10">
            <p className="text-[1.02rem] italic text-leise">{brief.gruss}</p>
            <p className="hand mt-1 text-[2.1rem] leading-none text-rot">{brief.signatur}</p>
          </div>

          {brief.ps && (
            <p className="mt-9 border-t border-linie pt-5 text-[0.95rem] leading-relaxed text-leise">
              <span className="font-stempel text-[0.7rem] tracking-widest">P.S. </span>
              {brief.ps}
            </p>
          )}

          {status.geoeffnetAm && !vorschau && (
            <p className="kapitaelchen mt-8 text-[0.52rem] opacity-70">
              geöffnet am {zeitpunktLang(status.geoeffnetAm)} Uhr
            </p>
          )}
        </article>

        {/* ─────────── Das Date ─────────── */}
        <section
          className="aufsteigen mt-12"
          style={{ animationDelay: "120ms" }}
          aria-labelledby="date-titel"
        >
          <div className="mb-6 flex items-center gap-4">
            <span className="luftpost-rand h-[5px] flex-1 rounded-full opacity-70" />
            <span className="kapitaelchen shrink-0">Die Verabredung</span>
            <span className="luftpost-rand h-[5px] flex-1 rounded-full opacity-70" />
          </div>

          <div className="korn relative rounded-sm border border-linie bg-karte p-6 shadow-karte sm:p-10">
            <h2 id="date-titel" className="text-[clamp(1.8rem,5vw,2.6rem)]">
              {brief.date.titel}
            </h2>
            <p className="mt-2 font-hand text-[1.55rem] leading-snug text-rot">{brief.date.kurz}</p>

            <dl className="mt-7 grid gap-x-8 gap-y-7 sm:grid-cols-2">
              <div className="sm:col-span-2">
                <dt className="kapitaelchen mb-3">So läuft es ab</dt>
                <dd>
                  <ol className="space-y-3">
                    {brief.date.ablauf.map((schritt, i) => (
                      <li key={i} className="flex gap-3.5">
                        <span className="mt-0.5 grid h-6 w-6 shrink-0 place-items-center rounded-full border border-rot/45 font-stempel text-[0.65rem] text-rot">
                          {i + 1}
                        </span>
                        <span className="leading-relaxed">{schritt}</span>
                      </li>
                    ))}
                  </ol>
                </dd>
              </div>

              <div>
                <dt className="kapitaelchen mb-3">Das braucht ihr</dt>
                <dd>
                  <ul className="space-y-2">
                    {brief.date.brauchtIhr.map((sache, i) => (
                      <li key={i} className="flex gap-2.5 leading-relaxed">
                        <span aria-hidden="true" className="mt-[0.55em] h-1 w-1 shrink-0 rounded-full bg-messing" />
                        {sache}
                      </li>
                    ))}
                  </ul>
                </dd>
              </div>

              <div className="space-y-6">
                <div>
                  <dt className="kapitaelchen mb-1.5">Zeit</dt>
                  <dd className="leading-relaxed">{brief.date.dauer}</dd>
                </div>
                {brief.date.soundtrack && (
                  <div>
                    <dt className="kapitaelchen mb-1.5">Dazu läuft</dt>
                    <dd className="leading-relaxed">{brief.date.soundtrack}</dd>
                  </div>
                )}
              </div>

              <div className="sm:col-span-2">
                <div className="rounded-sm border border-blau/30 bg-papier px-5 py-4">
                  <dt className="kapitaelchen mb-1.5 text-blau">Plan B</dt>
                  <dd className="leading-relaxed text-leise">{brief.date.planB}</dd>
                </div>
              </div>
            </dl>
          </div>
        </section>

        {/* ─────────── Foto & Tagebuch ─────────── */}
        <section className="aufsteigen mt-12 grid gap-8 sm:grid-cols-[minmax(0,1fr)_minmax(0,1.35fr)] sm:items-start" style={{ animationDelay: "180ms" }}>
          <FotoPlatz foto={fotoFuer(brief.id)} bildunterschrift={brief.monat} />
          <BriefAntwort
            briefId={brief.id}
            erledigtAmInitial={status.erledigtAm}
            notizInitial={status.notiz}
            nurLesen={vorschau}
          />
        </section>

        {/* ─────────── Blättern ─────────── */}
        <nav className="mt-16 flex items-stretch gap-4 text-sm" aria-label="Andere Briefe">
          {vorher ? (
            <Link
              href={`/brief/${vorher.id}${zeit ? `?zeit=${zeit}` : ""}`}
              className="korn flex-1 rounded-sm border border-linie bg-karte px-5 py-4 transition hover:border-rot/45"
            >
              <span className="kapitaelchen text-[0.52rem]">← davor</span>
              <span className="mt-1 block font-display font-semibold">{vorher.monat}</span>
            </Link>
          ) : (
            <span className="flex-1" />
          )}
          {nachher ? (
            <Link
              href={`/brief/${nachher.id}${zeit ? `?zeit=${zeit}` : ""}`}
              className="korn flex-1 rounded-sm border border-linie bg-karte px-5 py-4 text-right transition hover:border-rot/45"
            >
              <span className="kapitaelchen text-[0.52rem]">danach →</span>
              <span className="mt-1 block font-display font-semibold">{nachher.monat}</span>
            </Link>
          ) : (
            <span className="flex-1" />
          )}
        </nav>
      </main>
    </div>
  );
}

/** Wird gezeigt, wenn jemand die Adresse eines noch verschlossenen Briefes rät. */
function NochZu({ unlock, rolle }: { unlock: string; rolle: "marco" | "bella" }) {
  return (
    <div className="min-h-dvh">
      <Kopfzeile rolle={rolle} zeitreise={null} />
      <main className="mx-auto flex min-h-[80dvh] w-full max-w-md flex-col items-center justify-center px-6 text-center">
        <Motiv art="mond" className="w-14 text-rot opacity-70" />
        <h1 className="mt-6 text-4xl">Noch nicht.</h1>
        <p className="mt-4 text-leise">
          Dieser Umschlag geht erst am {datumLang(unlock)} auf. Ich weiß, das ist
          gemein. Aber genau das ist ja der Trick an dem Geschenk.
        </p>
        <div className="mt-9">
          <Countdown zielMs={berlinerMitternacht(unlock)} />
        </div>
        <Link
          href="/"
          className="mt-10 rounded-sm bg-tinte px-5 py-2.5 font-stempel text-[0.62rem] uppercase tracking-[0.2em] text-papier transition hover:opacity-90"
        >
          zurück zum Briefkasten
        </Link>
      </main>
    </div>
  );
}
