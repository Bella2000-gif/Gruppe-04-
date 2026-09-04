# Konzept & Gestaltung

Warum die Seite so aussieht, wie sie aussieht — und wie sie gebaut wurde.

---

## 1. Die Idee

Ein Geschenk, das man nicht einmal auspackt und dann weglegt, sondern das ein
ganzes Jahr lang immer wieder etwas hergibt. Dreizehn Briefe, einer pro Monat,
vom siebten bis zum achten Jahrestag. In jedem steckt neben dem Brief eine
konkrete Verabredung — geplant, nicht bloß angedeutet.

Drei Dinge mussten dafür stimmen:

1. **Das Warten muss echt sein.** Ein Brief, den man vorzeitig aufmachen kann,
   ist kein Brief, sondern eine Liste. Die Sperre gehört deshalb auf den
   Server, nicht in den Browser.
2. **Der Moment des Öffnens muss sich lohnen.** Er ist das eigentliche
   Geschenk und passiert dreizehnmal. Er darf ruhig eine Sekunde dauern.
3. **Man muss sehen, dass jemand sich Mühe gegeben hat.** Nicht durch eine
   einzelne große Geste, sondern durch viele kleine Details, die auffallen,
   wenn man genauer hinschaut.

---

## 2. Die Gestaltungsrichtung: „Poste Restante“

Recherchiert wurden vier Richtungen für Geschenke dieser Art. Gewonnen hat
die postalische:

| Richtung | Kurz | Warum nicht |
| --- | --- | --- |
| **Poste Restante** — Luftpost-Korrespondenz | Umschläge, Briefmarken, Poststempel, Siegellack | **gewählt** |
| Pressed Petals — gepresste Botanik | Herbarium, Aquarell, Seidenpapier | Collagen brechen beim Verkleinern auf Handygröße |
| Quiet Ink — literarische Typografie | nur Schrift, sehr modern | zu wenig „süß“, alles hängt an einem Detail |
| Deco Nocturne — Art déco | Gold, Bögen, Symmetrie | wirkt festlich-formell statt verliebt |

**Warum Poste Restante gewinnt:** die Metapher macht die halbe Arbeit.
Dreizehn Briefe *sind* dreizehn Umschläge. Jeder Zustand hat schon eine
natürliche Darstellung — versiegelt, entwertet, geöffnet — und niemand muss
erklärt bekommen, was das Raster auf der Startseite bedeutet. Außerdem sind
Umschläge Rechtecke: ein 4 × 4-Raster füllt einen 16:9-Bildschirm fast
perfekt und fällt auf dem Handy sauber in eine Spalte, ohne dass eine
Komposition zerreißt.

Das Ganze bleibt trotzdem modern, weil die postalischen Elemente **klein**
bleiben (Marke und Stempel sind Akzente, keine Tapete) und die Typografie
die eigentliche Arbeit macht.

### Farben

Warmes Papier, Luftpostrot, Tintenblau, Siegellack, ein Hauch Messing.

```
Papier   #faf4ea      Luftpostrot   #c1453c
Karte    #fffcf6      Tintenblau    #2f4b7c
Tinte    #2e2a26      Siegel        #7b2233
Linie    #e2d6c2      Messing       #c9a227
```

Der Dunkelmodus ist **warmes** Schwarz (`#17140f`), nie `#000` — reines
Schwarz mit hellem Text flimmert auf OLED-Displays und wirkt kalt.

### Schriften

| Rolle | Schrift | Warum |
| --- | --- | --- |
| Überschriften | **Fraunces** | variable Achsen `SOFT` und `WONK` — im Großen tintig und handgemacht, im Kleinen sachlich |
| Fließtext | **Karla** | humanistisch, leicht eigenwillig, hält 17 px auf dem Handy aus |
| Handschrift | **Caveat** | elegant, aber wirklich lesbar — kein Bastelbogen |
| Stempel & Daten | **Courier Prime** | Schreibmaschine, für Datumsangaben und Kapitälchen |

Alle vier liegen selbst gehostet im Projekt. Das ist kein Detail: eine
`<link>`-Einbindung von Google Fonts überträgt die IP-Adresse jedes Besuchers
an Google, was ein deutsches Gericht 2022 für unzulässig erklärt hat.

---

## 3. Der Moment des Öffnens

Die Bewegung hat drei Schläge, die aufeinander warten:

```
  0 ms   Das Siegel bricht in zwei Hälften und fällt weg
180 ms   Die Klappe klappt in echtem 3D nach hinten  (rotateX -172°)
620 ms   Der Brief schiebt sich heraus und nach vorn  (translateZ 64px)
1500 ms  Seitenwechsel — auf der Briefseite regnet es Blütenblätter
```

Dazu ein kurzer Vibrationsimpuls auf dem Handy (`navigator.vibrate`), damit
das Siegelbrechen auch zu spüren ist.

Bei `prefers-reduced-motion` fällt die ganze Choreografie weg und es geht
direkt zum Brief. Die Blütenblätter werden dann per CSS ausgeblendet — nicht
in einer Millisekunde durchgejagt.

### Was daran technisch heikel war

**`z-index` funktioniert in einer 3D-Ebene nicht.** Sobald `transform-style:
preserve-3d` gesetzt ist, sortieren sich Geschwister nach ihrer echten
Z-Position. Der klassische Trick „Klappe erst über, dann unter dem Brief“
lässt sich damit nicht lösen. Stattdessen macht es hier die Geometrie: die
Klappe liegt bei `translateZ(3px)`, und der Brief fährt beim Öffnen auf
`translateZ(64px)` — er schiebt sich also wirklich an ihr vorbei.

**Fast jede hübsche CSS-Eigenschaft macht 3D kaputt.** `overflow`, `opacity`,
`filter`, `clip-path`, `mask` und `mix-blend-mode` zwingen alle Kindelemente
in eine Ebene zurück. Deshalb hat der Umschlag selbst **kein**
`overflow: hidden`, und die Papierstruktur liegt als `background-image` auf
den Flächen statt als CSS-Filter.

**Das Siegel ist ein Geschwister der Klappe, kein Kind.** Wäre es ein Kind,
läge es nach dem Aufklappen spiegelverkehrt auf der Rückseite.

**Die Briefmarke sitzt unterhalb der V-Kerbe.** Die Vorderseite des Umschlags
ist oben ausgeschnitten (dort schaut die Klappe heraus). Alles, was in diese
Kerbe ragt, wird abgeschnitten — die erste Fassung hatte genau diesen Fehler.

---

## 4. Alles gezeichnet, nichts geladen

Die Seite lädt **kein einziges Bild**. Das ist keine Sparsamkeit, sondern
Absicht: keine Lizenzfragen, keine toten Links in fünf Jahren, keine fremden
Server, gestochen scharf auf jedem Display, und die ganze Seite funktioniert
offline.

| Was | Wie |
| --- | --- |
| Papierstruktur | `feTurbulence` als Data-URI-Hintergrund, mit `stitchTiles="stitch"` gegen sichtbare Nähte |
| Siegellack | `feDisplacementMap` für die unregelmäßige Kante (echtes Wachs quillt beim Pressen heraus) + `feSpecularLighting` für das Glanzlicht auf der Kuppel |
| Briefmarken | SVG mit einer Maske aus 52 Kreisen entlang der Kanten — echte Zähnung |
| Poststempel | SVG, damit die Proportionen bei jeder Größe stimmen |
| 13 Motive | Strichzeichnungen in `currentColor`, nehmen die Farbe der Umgebung an |
| 4 Landschaften | geschichtete Hügel, Bäume, Sonne, Sterne — eine pro Jahreszeit |
| Blütenblätter | 28 SVG-Blätter, per CSS fallend, deterministisch gestreut statt zufällig |

Die einzige Stelle für echte Bilder ist `public/fotos/` — dort liegen die
dreizehn Fotos der beiden, eines pro Brief, aufgeteilt nach Jahreszeit und
Stimmung. Sie werden also nicht auf einmal ausgeschüttet, sondern kommen
Monat für Monat dazu: mit jedem Umschlag ein Brief, ein Date und ein Bild.
Fehlt eines, steht ein gezeichneter Platzhalter, damit die Seite nie
unfertig aussieht.

Die Abmessungen liest der Server selbst aus dem Dateikopf (JPEG, PNG und
WebP, rund vierzig Zeilen in `lib/fotos.ts`, ohne zusätzliche Bibliothek).
Dadurch bekommt jedes Polaroid das Seitenverhältnis seines Bildes: Hochformat
bleibt hochkant, nichts wird beschnitten, und der Rahmen springt beim Laden
nicht.

---

## 5. Wie die Sperre wirklich hält

Der ganze Wert des Geschenks hängt daran, dass man nicht vorspulen kann.
Deshalb:

- **Eine einzige Torwächter-Funktion** (`lib/briefkasten.ts`), die sowohl die
  Seite als auch jede API-Route fragt.
- **Verschlossener Text wird nie ausgeliefert.** Nicht ausgegraut, nicht
  versteckt, nicht im HTML — er verlässt den Server nicht. Auch die Übersicht
  auf der Startseite kennt Titel und Motto nur von bereits geöffneten Briefen.
- **Zeitzone festgenagelt.** Gerechnet wird in `Europe/Berlin`, unabhängig von
  Server und Endgerät. Die Sommerzeitumstellung ist mitgedacht: der 10. Januar
  geht um 23:00 UTC des Vortages auf, der 10. Oktober um 22:00 UTC.
- **Der Countdown holt sich die Zielzeit vom Server.** Die Uhr des Handys
  umzustellen hilft also nichts.
- **Direkte API-Aufrufe prüfen erneut.** `POST /api/briefe/2/oeffnen` auf einen
  noch verschlossenen Brief antwortet mit `423 Locked`.

Die Datenbank speichert nur, *was danach passiert* — wann geöffnet wurde, ob
das Date stattgefunden hat, was Marco dazu geschrieben hat. Selbst wenn sie
verloren geht, funktioniert das Geschenk weiter; es fehlen dann nur die
Notizen.

### Sobald die Seite öffentlich steht

Jeder Hoster vergibt eine Adresse, die grundsätzlich jeder aufrufen kann.
„Privat" entsteht deshalb nicht durch die Wahl des Anbieters, sondern durch
die Anmeldung — und die musste dafür etwas fester werden:

- **Kein Codewort aus dem Quelltext.** Im Produktivbetrieb prüft die App die
  Güte der eingetragenen Werte: mindestens zehn Zeichen, verschieden
  voneinander, nicht die Beispiele aus `.env.example`. Ist etwas davon nicht
  erfüllt, lässt sie *niemanden* herein und schreibt auf die Anmeldeseite,
  was fehlt. Lieber eine Seite, die nicht aufgeht, als eine, die für alle
  aufgeht.
- **Sperre nach acht Fehlversuchen** je Absender, dann eine Viertelstunde
  Pause; dazu eine halbe Sekunde Verzögerung pro Fehlversuch.
- **Cookie** `httpOnly`, `sameSite=lax`, im Produktivbetrieb `secure`, mit
  HMAC unterschrieben und nach 400 Tagen abgelaufen.
- **`noindex` und `robots.txt`**, damit die Adresse nicht in Suchmaschinen
  auftaucht.

### Ein Speicher, zwei Rückseiten

Für das Veröffentlichen zerfallen die Hoster in zwei Lager: serverlose ohne
dauerhafte Festplatte (Vercel) und solche mit Volume (Railway, ein eigener
Server). Statt sich für eines zu entscheiden, gibt es beide Rückseiten hinter
derselben Schnittstelle — SQLite für die Datei, Postgres für alles Serverlose.
Gewählt wird über die Umgebung: ist `DATABASE_URL` gesetzt, ist es Postgres,
sonst eine Datei. Am übrigen Code ändert sich nichts.

Beide legen ihre Tabelle beim ersten Zugriff selbst an, es gibt also keinen
Migrationsschritt, den man beim Aufsetzen vergessen könnte. Und beide legen
Zeitstempel als ISO-Zeichenkette ab statt als Datumstyp der Datenbank — das
hält die Fassungen wirklich austauschbar und erspart Überraschungen mit
Zeitzonen.

## 6. Über das Jahr wird ein Tagebuch daraus

Unter jedem Brief kann Marco abhaken, dass das Date stattgefunden hat, und
dazuschreiben, wie es war. Gespeichert wird automatisch, kurz nachdem er
aufhört zu tippen.

Am 10. Oktober 2027 ist die Seite deshalb nicht mehr dasselbe wie am Anfang:
aus dreizehn Versprechen sind dreizehn Erinnerungen geworden. Der letzte
Brief greift das auf.

---

## 7. Auf allen Geräten

- **16:9-Bildschirm:** vier Umschläge pro Reihe, das Raster füllt das Bild.
- **Tablet:** drei pro Reihe.
- **Handy:** einer pro Reihe, groß und gut zu treffen — der Umschlag ist
  schließlich die Hauptsache und kein Listeneintrag.
- **Hell und dunkel** folgen der Systemeinstellung, lassen sich oben rechts
  aber auch fest umstellen. Ein winziges Skript im `<head>` setzt das
  gespeicherte Thema, bevor das erste Pixel gemalt wird — sonst blitzt beim
  Laden kurz die helle Fassung auf.
- **Barrierefreiheit:** jeder Umschlag ist ein echter Knopf mit sprechender
  Beschriftung, der Fortschrittsbalken hat `role="progressbar"`, der
  Speicherstand des Tagebuchs meldet sich über `aria-live`, und
  `prefers-reduced-motion` schaltet alle Bewegung ab.
