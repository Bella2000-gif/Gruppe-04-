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

| Eintrag          | Wofür                                                                    |
| ---------------- | ------------------------------------------------------------------------ |
| `MARCO_CODE`     | Marcos Codewort, mindestens 10 Zeichen. Groß-/Kleinschreibung egal        |
| `BELLA_CODE`     | Bellas Zugang mit Vorschau und Zeitreise. Muss ein anderes Wort sein      |
| `SESSION_SECRET` | Unterschreibt das Anmelde-Cookie. Mindestens 24 zufällige Zeichen          |
| `DATABASE_URL`   | Nur bei serverlosen Hostern: Postgres-Verbindung. Sonst weglassen         |
| `DATABASE_PATH`  | Wo die SQLite-Datei liegt. Standard: `./data/briefe.db`                   |

Ein gutes Geheimnis erzeugen:

```bash
openssl rand -base64 32
```

> **Im Produktivbetrieb prüft die App das nach.** Fehlt eine Variable, ist ein
> Codewort zu kurz oder steht noch der Beispielwert drin, lässt die Seite
> niemanden herein und sagt auf der Anmeldeseite genau, was fehlt. Beim
> Entwickeln (`npm run dev`) stört sie das nicht.

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
  titel: "Kalte Nasen",
  motto: "Draußen wird es früh dunkel. Umso besser.",
  season: "winter",         // herbst | winter | fruehling | sommer
  stamp: "schlittschuh",    // Motiv der Briefmarke, siehe unten
  anrede: "Lieber Marcolino,",
  absaetze: ["…", "…"],     // die Absätze des Briefes
  gruss: "Zieh dich warm an,",
  signatur: "deine Bella",
  ps: "…",                  // optional
  date: {
    titel: "Schlittschuhfahren",
    kurz: "Kalte Nasen, wacklige Knie, und danach heiße Schokolade.",
    text: ["…", "…"],       // ein bis zwei kurze Absätze über das Date
    brauchtIhr: ["…"],      // nur, was wirklich gebraucht wird
    dauer: "Ein Nachmittag",
    wetter: "…",            // optional, bleibt bei der Aktivität
  },
}
```

**Streu ruhig eure eigenen Insider ein.** Die Texte sind bewusst kurz
gehalten und so geschrieben, dass sie als Grundgerüst funktionieren — genau
die persönlichen Stellen sind es aber, die so einen Brief unbezahlbar machen.

Verfügbare Briefmarken-Motive: `kino`, `schlittschuh`, `tanne`, `tasse`,
`pasta`, `kaffee`, `berg`, `korb`, `pinsel`, `mond`, `fahrrad`, `tier`,
`album`, `herz`, `blatt`, `tulpe`, `sonne`, `welle`, `sternschnuppe`,
`traube`. Gezeichnet sind sie in `src/components/Motive.tsx` — dort kannst du
auch neue dazumalen.

---

## Fotos einsetzen

Alle dreizehn Briefe haben ein Bild, jeweils dort, wo Jahreszeit, Türchen und
Stimmung zusammenpassen:

| Datei | Brief | Monat | Türchen | Bild |
| --- | --- | --- | --- | --- |
| `01.webp` | 1 | Oktober 2026 | Kino | Strand, „Happy 7th anniversary“ |
| `02.webp` | 2 | November 2026 | Schlittschuhfahren | Bahnhof im Winter, Mützen |
| `03.webp` | 3 | Dezember 2026 | Weihnachtsmarkt-Hopping | Umarmung im Frost, schwarzweiß |
| `04.webp` | 4 | Januar 2027 | Keramik bemalen | „vibes“, drinnen, albern |
| `05.webp` | 5 | Februar 2027 | Ich koche für dich | abends, warmes Licht |
| `06.webp` | 6 | März 2027 | Café-Tour | sonnig auf der Bank |
| `07.webp` | 7 | April 2027 | Wandern mit Snacks | im grünen Wald, mit Rucksack |
| `08.webp` | 8 | Mai 2027 | Picknick | Strand, schwarzweiß |
| `09.webp` | 9 | Juni 2027 | Malabend | Köpfe aneinander |
| `10.webp` | 10 | Juli 2027 | Open-Air-Kino | Strand im Sonnenuntergang |
| `11.webp` | 11 | August 2027 | Radltour mit Proviant | Kopenhagen |
| `12.webp` | 12 | September 2027 | Tierpark | Wiesn |
| `13.webp` | 13 | Oktober 2027 | Album vom Jahr | Sonnenuntergang, von hinten |

Ein Bild tauschen geht auf zwei Wegen. (Für einen Brief ohne Foto stünde ein
gezeichneter Platzhalter — die Seite sieht also auch dann fertig aus.)

**Direkt:** Datei nach Briefnummer benannt in `public/fotos/` ablegen —
`01.jpg`, `02.png`, `03.webp` … `13.jpg`. `.jpg`, `.jpeg`, `.png` und `.webp`
funktionieren alle. Ein Foto nachträglich hineinlegen reicht, ein Neustart ist
nicht nötig.

**Bequemer bei Handyfotos:** Original nach `fotos-original/` legen, ebenfalls
nach Briefnummer benannt, dann

```bash
npm run fotos
```

Das dreht das Bild nach den EXIF-Daten richtig herum, verkleinert es und
speichert es als WebP in `public/fotos/`. Aus 4 MB werden so meist unter
200 KB, ohne sichtbaren Unterschied. Bei sehr detailreichen Motiven — Laub,
Kies, Filmkorn — senkt das Skript zusätzlich die Auflösung, bis das Bild unter
300 KB bleibt; das sieht besser aus, als die Qualität immer weiter zu drücken,
und das Polaroid ist auf dem Bildschirm ohnehin nur rund 300 Pixel breit. Die
Originale bleiben unangetastet.

**Hoch- und Querformat funktionieren beide.** Der Server liest die
Abmessungen aus dem Dateikopf, das Polaroid übernimmt das Seitenverhältnis
des Bildes — nichts wird beschnitten, und der Rahmen steht schon in der
richtigen Form da, bevor das Bild geladen ist.

## Veröffentlichen

Ausführlich steht das in **[HOSTING.md](./HOSTING.md)** — inklusive der Frage,
warum die Seite auch auf Vercel privat bleibt, und einer Prüfliste zum Abhaken.

Die Kurzfassung: die Privatsphäre kommt aus der Anmeldung in der App, nicht
vom Hoster. Zu entscheiden ist nur, wo Marcos Häkchen und Notizen bleiben.

| Hoster | Speicher | Einzurichten |
| --- | --- | --- |
| **Vercel, Netlify** (serverlos) | Festplatte ist flüchtig | `DATABASE_URL` auf eine Postgres-Datenbank (z. B. Neon) — **Pflicht**, sonst ist nach jedem Deploy alles weg |
| **Railway, Fly.io, eigener Server, Raspberry Pi** | Festplatte bleibt | Ein Volume, `DATABASE_PATH` daraufzeigen. Keine Datenbank nötig |

Die App merkt selbst, was sie vor sich hat: ist `DATABASE_URL` gesetzt,
benutzt sie Postgres, sonst eine SQLite-Datei. Am Code ändert sich nichts.

Empfehlung für ein Jahr ohne Wartung: **Vercel + Neon**, beides kostenlos,
`git push` genügt zum Aktualisieren.

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
    auth.ts                   signiertes Cookie, zwei Codewörter, Sperre
    fotos.ts                  findet die Fotos und misst sie aus
    db/
      index.ts                wählt den Speicher anhand der Umgebung
      sqlite.ts               Datei — für Hoster mit Festplatte
      postgres.ts             Datenbank — für serverlose Hoster
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
SQLite (better-sqlite3) oder Postgres (pg)

Keine externen Bilder, keine Icon-Bibliothek, keine Animationsbibliothek:
Briefmarken, Poststempel, Siegellack, Papierstruktur und die vier
Jahreszeiten-Landschaften sind alle als SVG und CSS gezeichnet. Die
Schriften (Fraunces, Karla, Caveat, Courier Prime) liegen selbst gehostet im
Projekt — dadurch lädt die Seite nichts von fremden Servern nach, funktioniert
offline und schickt keine Besucherdaten zu Google.
