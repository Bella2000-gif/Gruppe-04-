# Dreizehn Briefe

Ein Geschenk zum siebten Jahrestag: dreizehn Umschläge, die sich einer nach
dem anderen öffnen — immer am Zehnten, vom **10. Oktober 2026** bis zum
**10. Oktober 2027**. In jedem steckt ein Brief und ein Datevorschlag,
passend zur Jahreszeit.

Marco kann jeden Monat genau einen aufmachen. Das prüft der Server, nicht der
Browser: der Text eines noch verschlossenen Briefes verlässt den Server gar
nicht erst. Man kann ihn also weder mit den Entwicklerwerkzeugen noch durch
Herumraten an Adressen vorzeitig lesen.

---

## Schnellstart

```bash
cd jahrestag
npm install
cp .env.example .env.local     # und die Werte darin anpassen (siehe unten)
npm run dev
```

Dann [http://localhost:3000](http://localhost:3000) öffnen.

Für den echten Betrieb:

```bash
npm run build
npm start
```

---

## Die `.env.local`

| Eintrag          | Wofür                                                                 |
| ---------------- | --------------------------------------------------------------------- |
| `MARCO_CODE`     | Marcos Codewort. Groß-/Kleinschreibung egal. Standard: `popolino`      |
| `BELLA_CODE`     | Bellas Zugang mit Vorschau und Zeitreise. Standard: `bella`            |
| `SESSION_SECRET` | Unterschreibt das Anmelde-Cookie. **Unbedingt ändern.**                |
| `DATABASE_PATH`  | Optional. Wo die SQLite-Datei liegt. Standard: `./data/briefe.db`      |

Ein gutes Geheimnis erzeugen:

```bash
openssl rand -base64 32
```

> Ohne gesetztes `SESSION_SECRET` läuft alles, aber jeder Neustart des Servers
> meldet beide wieder ab.

---

## Zwei Zugänge

**Marco** (`MARCO_CODE`) sieht den Briefkasten so, wie er gedacht ist:
verschlossene Umschläge mit Countdown, offene zum Lesen. Er kann Dates
abhaken und ins gemeinsame Tagebuch schreiben.

**Bella** (`BELLA_CODE`) sieht zusätzlich:

- **Vorschau** — sie kann jeden Brief schon vor seinem Datum lesen und
  Korrektur lesen. Das zählt ausdrücklich *nicht* als „geöffnet“ und taucht
  bei Marco nirgends auf.
- **Zeitreise** — oben rechts ein Datumsfeld. Damit lässt sich prüfen, wie
  die Seite im März 2027 aussieht, ohne die Systemuhr zu verstellen.

---

## Die Briefe ändern

Alles steht in **`src/lib/letters.ts`** — und nur dort. Datum, Titel,
Anrede, Absätze, Grußformel, P.S. und der komplette Datevorschlag.
Die App holt sich alles Weitere daraus (Reihenfolge, Freischaltung,
Countdown, Briefmarke, Jahreszeit).

Ein Brief sieht so aus:

```ts
{
  id: 2,
  unlock: "2026-11-10",     // Freischaltung, immer YYYY-MM-DD
  monat: "November 2026",
  stempel: "10 · XI · 26",
  titel: "Novemberlicht",
  motto: "Draußen grau. Drinnen wir.",
  season: "herbst",         // herbst | winter | fruehling | sommer
  stamp: "mond",            // Motiv der Briefmarke, siehe unten
  anrede: "Lieber Marcolino,",
  absaetze: ["…", "…"],     // die Absätze des Briefes
  gruss: "Bis später auf dem Sofa,",
  signatur: "deine Bella",
  ps: "…",                  // optional
  date: { … },              // der Datevorschlag
}
```

**Streu ruhig eure eigenen Insider ein.** Die Texte sind bewusst kurz
gehalten und so geschrieben, dass sie als Grundgerüst funktionieren — genau
die persönlichen Stellen sind es aber, die so einen Brief unbezahlbar machen.

Verfügbare Briefmarken-Motive: `herz`, `blatt`, `mond`, `tanne`,
`schlittschuh`, `tasse`, `tulpe`, `pasta`, `fahrrad`, `sonne`, `welle`,
`sternschnuppe`, `traube`. Gezeichnet sind sie in
`src/components/Motive.tsx`.

---

## Fotos einsetzen

Schon eingesetzt sind fünf eurer Bilder, jeweils dort, wo Jahreszeit und Date
passen:

| Datei | Brief | Monat | Bild |
| --- | --- | --- | --- |
| `01.webp` | 1 | Oktober 2026 | Strand, „Happy 7th anniversary“ |
| `04.webp` | 4 | Januar 2027 | Bahnhof im Winter |
| `08.webp` | 8 | Mai 2027 | Kopenhagen |
| `10.webp` | 10 | Juli 2027 | Strand, schwarzweiß |
| `12.webp` | 12 | September 2027 | Wiesn |

Die übrigen acht Briefe zeigen einen gezeichneten Platzhalter, bis dort ein
Bild liegt. Zum Tauschen oder Ergänzen gibt es zwei Wege.

**Direkt:** Datei nach Briefnummer benannt in `public/fotos/` ablegen —
`01.jpg`, `02.png`, `03.webp` … `13.jpg`. `.jpg`, `.jpeg`, `.png` und `.webp`
funktionieren alle. Ein Foto nachträglich hineinlegen reicht, ein Neustart ist
nicht nötig.

**Bequemer bei Handyfotos:** Original nach `fotos-original/` legen, ebenfalls
nach Briefnummer benannt, dann

```bash
npm run fotos
```

Das dreht das Bild nach den EXIF-Daten richtig herum, verkleinert es auf
1500 Pixel Höhe und speichert es als WebP in `public/fotos/`. Aus 4 MB werden
so meist unter 200 KB, ohne sichtbaren Unterschied. Die Originale bleiben
unangetastet.

**Hoch- und Querformat funktionieren beide.** Der Server liest die
Abmessungen aus dem Dateikopf, das Polaroid übernimmt das Seitenverhältnis
des Bildes — nichts wird beschnitten, und der Rahmen steht schon in der
richtigen Form da, bevor das Bild geladen ist.

## Veröffentlichen

Die App braucht einen **Node-Server** (wegen der Datumsprüfung) und, wenn
Marcos Notizen dauerhaft erhalten bleiben sollen, **einen Ort zum Schreiben**.

**Empfohlen — Hosting mit Festplatte** (Railway, Render, Fly.io, Hetzner,
Raspberry Pi zu Hause):

1. `SESSION_SECRET`, `MARCO_CODE`, `BELLA_CODE` als Umgebungsvariablen setzen.
2. `DATABASE_PATH` auf ein Verzeichnis legen, das Neustarts überlebt,
   z. B. `/data/briefe.db`.
3. `npm run build && npm start`.

**Auf Vercel & Co.** funktioniert das Geschenk ebenfalls — dort ist die
Festplatte allerdings flüchtig. Die Freischaltung der Briefe hängt *nur* am
Datum und bleibt deshalb korrekt; verloren gehen können nur die Häkchen und
Tagebucheinträge. Wenn diese bleiben sollen, ist eine der oben genannten
Optionen die bessere Wahl.

Die Seite trägt `noindex` — sie taucht nicht in Suchmaschinen auf.

---

## Wie es gebaut ist

```
src/
  app/
    page.tsx                  Briefkasten mit den dreizehn Umschlägen
    brief/[id]/page.tsx       Ein Brief mit Date, Foto und Tagebuch
    anmelden/page.tsx         Codewort-Abfrage
    api/                      Anmeldung, Öffnen, Notiz, Erledigt
  components/                 Umschlag, Siegel, Marke, Motive, Landschaften …
  lib/
    letters.ts                ► HIER STEHT DAS GESCHENK
    briefkasten.ts            der Torwächter: wer darf was sehen
    zeit.ts                   Datumslogik, fest in Europe/Berlin
    db.ts                     SQLite: geöffnet, erledigt, Notizen
    auth.ts                   signiertes Cookie, zwei Codewörter
```

Die Gestaltung und die Gedanken dahinter stehen in **[KONZEPT.md](./KONZEPT.md)**.

### Warum die Freischaltung wirklich hält

`src/lib/briefkasten.ts` ist die einzige Stelle, die entscheidet, wer welchen
Brief sehen darf. Sowohl die Briefseite als auch alle API-Routen fragen dort
nach. Ein verschlossener Brief wird deshalb nie an den Browser geschickt —
auch nicht versteckt im HTML.

Zeitzonen sind bewusst festgenagelt: gerechnet wird immer in `Europe/Berlin`,
egal wo der Server steht und wie Marcos Handy eingestellt ist. Ein Brief geht
um Mitternacht deutscher Zeit auf, im Sommer wie im Winter.

---

## Technik

Next.js 16 (App Router) · React 19 · TypeScript · Tailwind CSS v4 ·
SQLite (better-sqlite3)

Keine externen Bilder, keine Icon-Bibliothek, keine Animationsbibliothek:
Briefmarken, Poststempel, Siegellack, Papierstruktur und die vier
Jahreszeiten-Landschaften sind alle als SVG und CSS gezeichnet. Die
Schriften (Fraunces, Karla, Caveat, Courier Prime) liegen selbst gehostet im
Projekt — dadurch lädt die Seite nichts von fremden Servern nach, funktioniert
offline und schickt keine Besucherdaten zu Google.
