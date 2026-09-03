import { redirect } from "next/navigation";
import { aktuelleRolle } from "@/lib/auth";
import { AnmeldeFormular } from "@/components/AnmeldeFormular";
import { Siegel } from "@/components/Siegel";

export default async function AnmeldeSeite() {
  if (await aktuelleRolle()) redirect("/");

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

        <AnmeldeFormular />
      </div>
    </main>
  );
}
