import { redirect } from "next/navigation";
import { aktuelleRolle, konfigProblem } from "@/lib/auth";
import { AnmeldeFormular } from "@/components/AnmeldeFormular";
import { Siegel } from "@/components/Siegel";

export default async function AnmeldeSeite() {
  if (await aktuelleRolle()) redirect("/");

  // Steht die Seite öffentlich, läuft aber noch mit den Standardwerten aus
  // dem Quelltext, muss das sofort sichtbar sein — nicht erst im Serverlog.
  const problem = konfigProblem();

  return (
    <main className="mx-auto flex min-h-dvh w-full max-w-md flex-col items-center justify-center px-6 py-16">
      <div className="aufsteigen flex w-full flex-col items-center">
        <div className="w-24">
          <Siegel initialen="B&M" />
        </div>

        <p className="kapitaelchen mt-8">Privat · nur für zwei</p>
        <h1 className="mt-2 text-center text-4xl">Dreizehn Briefe</h1>
        <p className="mt-3 max-w-xs text-center text-[0.95rem] leading-relaxed text-leise">
          Für Marcolino Popolino. Ein Umschlag pro Monat, vom 10. Oktober 2026
          bis zum 10. Oktober 2027.
        </p>

        <div className="raute-trenner my-8 w-full">
          <span className="font-display text-xs tracking-widest">✦</span>
        </div>

        {problem ? (
          <div
            role="alert"
            className="korn w-full rounded-sm border border-rot bg-karte p-5 text-left shadow-karte"
          >
            <p className="kapitaelchen text-rot">Noch nicht eingerichtet</p>
            <p className="mt-2 text-[0.95rem] leading-relaxed text-leise">{problem}</p>
            <p className="mt-3 text-[0.88rem] leading-relaxed text-fluestern">
              Bis dahin lässt die Seite niemanden herein — auch Marco nicht.
              Die Anleitung dazu steht in <code>HOSTING.md</code>.
            </p>
          </div>
        ) : (
          <AnmeldeFormular />
        )}
      </div>
    </main>
  );
}
