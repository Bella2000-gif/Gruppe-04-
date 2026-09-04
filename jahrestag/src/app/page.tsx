import { redirect } from "next/navigation";
import { aktuelleRolle } from "@/lib/auth";
import { fortschritt, jetztFuer, naechsterBrief, uebersicht } from "@/lib/briefkasten";
import { PAAR } from "@/lib/letters";
import { datumLang, heuteInBerlin } from "@/lib/zeit";
import { Umschlag } from "@/components/Umschlag";
import { Countdown } from "@/components/Countdown";
import { Kopfzeile } from "@/components/Kopfzeile";
import { Motiv } from "@/components/Motive";

export default async function Briefkasten({
  searchParams,
}: {
  searchParams: Promise<{ zeit?: string }>;
}) {
  const rolle = await aktuelleRolle();
  if (!rolle) redirect("/anmelden");

  const { zeit } = await searchParams;
  const jetzt = jetztFuer(rolle, zeit);
  const [briefe, stand] = await Promise.all([uebersicht(rolle, jetzt), fortschritt()]);
  const naechster = naechsterBrief(jetzt);

  const bereit = briefe.filter((b) => b.status === "bereit" && !b.vorschau).length;
  const heute = heuteInBerlin(jetzt);

  return (
    <div className="min-h-dvh">
      <Kopfzeile rolle={rolle} zeitreise={rolle === "bella" ? (zeit ?? heute) : null} />

      <main className="mx-auto w-full max-w-6xl px-5 pb-24 sm:px-8">
        {/* ─────────────── Kopfbereich ─────────────── */}
        <section className="aufsteigen relative pt-10 text-center sm:pt-16">
          <p className="kapitaelchen">
            {PAAR.sie} &nbsp;✦&nbsp; {PAAR.er} &nbsp;·&nbsp; seit dem 10. Oktober 2019
          </p>

          <h1 className="mt-4 text-[clamp(2.75rem,9vw,5.5rem)]">
            Sieben Jahre
            <span className="mt-1 block font-hand text-[clamp(1.6rem,5vw,2.6rem)] font-normal tracking-normal text-rot">
              für Marcolino Popolino
            </span>
          </h1>

          <p className="mx-auto mt-6 max-w-xl text-balance text-[1.02rem] leading-relaxed text-leise">
            Dreizehn Umschläge. In jedem steckt ein Brief und eine Verabredung.
            Einer geht jeden Monat auf, immer am Zehnten — bis zum 10. Oktober 2027.
          </p>

          {/* Bellas Zeilen — das Erste, was Marco liest, bevor er irgendwo klickt */}
          <div className="korn relative mx-auto mt-10 max-w-lg rounded-sm border border-linie bg-karte px-7 py-7 text-left shadow-karte sm:px-9">
            <span
              aria-hidden="true"
              className="klebeband absolute -top-3 left-1/2 h-6 w-24 -translate-x-1/2 -rotate-2 rounded-[1px]"
            />
            <p className="hand text-[1.5rem] leading-[1.45] text-tinte">
              Thailand, Neuseeland, Italien, Dänemark — und all die Tage
              dazwischen hier in Deutschland. Ich liebe die letzten sieben Jahre
              mit dir, jeden einzelnen davon.
            </p>
            <p className="hand mt-4 text-[1.5rem] leading-[1.45] text-tinte">
              Du bist mein Herzensmensch.
            </p>
            <p className="hand mt-5 text-right text-[1.55rem] leading-none text-rot">
              deine Bella
            </p>
          </div>

          {/* Countdown bzw. Hinweis auf einen wartenden Brief */}
          <div className="mt-10 flex flex-col items-center">
            {bereit > 0 ? (
              <div className="korn relative inline-flex flex-col items-center gap-1 rounded-sm border border-rot/35 bg-karte px-7 py-4 shadow-karte">
                <span className="kapitaelchen text-rot">
                  {bereit === 1 ? "Ein Brief wartet auf dich" : `${bereit} Briefe warten auf dich`}
                </span>
                <span className="font-display text-lg font-semibold">Mach ihn auf.</span>
              </div>
            ) : naechster ? (
              <div className="flex flex-col items-center gap-3">
                <span className="kapitaelchen">Nächster Umschlag · {naechster.monat}</span>
                <Countdown zielMs={naechster.unlockMs} />
              </div>
            ) : (
              <p className="font-hand text-2xl text-rot">
                Alle dreizehn sind offen. Was für ein Jahr.
              </p>
            )}
          </div>

          {/* Fortschritt */}
          <div className="mx-auto mt-10 flex max-w-sm items-center gap-3">
            <span className="kapitaelchen shrink-0 text-[0.58rem]">
              {stand.geoeffnet}/{stand.gesamt} gelesen
            </span>
            <div
              className="h-[3px] flex-1 overflow-hidden rounded-full bg-linie"
              role="progressbar"
              aria-valuenow={stand.geoeffnet}
              aria-valuemin={0}
              aria-valuemax={stand.gesamt}
              aria-label="Gelesene Briefe"
            >
              <div
                className="h-full rounded-full bg-rot transition-[width] duration-700"
                style={{ width: `${(stand.geoeffnet / stand.gesamt) * 100}%` }}
              />
            </div>
            <span className="kapitaelchen shrink-0 text-[0.58rem]">
              {stand.erledigt} {stand.erledigt === 1 ? "Date" : "Dates"}
            </span>
          </div>
        </section>

        <div className="raute-trenner my-14">
          <Motiv art="herz" className="w-7 text-rot" />
        </div>

        {/* ─────────────── Der Briefkasten ─────────────── */}
        <section aria-label="Die dreizehn Briefe">
          <ul className="grid grid-cols-1 gap-x-6 gap-y-10 min-[420px]:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4">
            {briefe.map((b, i) => (
              <li key={b.id}>
                <Umschlag brief={b} index={i} />
              </li>
            ))}
          </ul>
        </section>

        <footer className="mt-24 text-center">
          <div className="raute-trenner mb-6">
            <span className="font-display text-sm">✦</span>
          </div>
          <p className="font-hand text-xl text-leise">
            Geschrieben im Herbst 2026, für jeden Monat bis zum nächsten Jahrestag.
          </p>
          <p className="kapitaelchen mt-3 text-[0.55rem]">
            {datumLang(heute)} · Berliner Zeit
          </p>
        </footer>
      </main>
    </div>
  );
}
