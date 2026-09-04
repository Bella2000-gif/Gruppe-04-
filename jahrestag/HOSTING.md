# Veröffentlichen

Damit Marco die Seite von seinem Handy aus aufrufen kann — und sonst niemand.

---

## Zuerst das Wichtigste: Wovon hängt die Privatsphäre ab?

**Nicht vom Hoster.** Jeder Anbieter — Vercel, Railway, Fly, ein eigener
Server — vergibt eine Adresse, die grundsätzlich jeder aufrufen kann, der sie
kennt. „Privat" heißt bei keinem von ihnen „unerreichbar".

Privat wird die Seite durch die **Anmeldung in der App selbst**. Ohne gültiges
Cookie liefert der Server:

- auf `/` und `/brief/…` nur eine Weiterleitung zur Anmeldung,
- auf jede API-Route ein `401`,
- **keinen einzigen Brieftext**, auch nicht versteckt im HTML.

Dazu kommt: Die Seite trägt `noindex`, ihre `robots.txt` sperrt alle
Suchmaschinen aus, und nach acht Fehlversuchen ist die Anmeldung für eine
Viertelstunde gesperrt.

Deshalb ist **Vercel durchaus geeignet**. Die Frage ist eine andere.

---

## Die eigentliche Frage: Wo bleiben Marcos Häkchen?

Beim Aufheben dessen, was Marco im Laufe des Jahres anklickt und schreibt,
teilen sich die Anbieter in zwei Lager:

| | Festplatte | Was das bedeutet |
| --- | --- | --- |
| **Serverlos** (Vercel, Netlify) | flüchtig | Nach jedem neuen Deploy und nach längerer Pause ist die Datei weg. **Eine Datenbank daneben ist Pflicht.** |
| **Mit Volume** (Railway, Fly.io, Render, eigener Server, Raspberry Pi) | bleibt | Eine SQLite-Datei genügt. Nichts weiter einzurichten. |

Beides funktioniert. Die App merkt selbst, was sie vor sich hat:

```
DATABASE_URL gesetzt (postgres://…)  →  Postgres
sonst                                →  SQLite-Datei
```

> **Zur Beruhigung:** Selbst wenn die Datenbank einmal verloren geht, ist das
> Geschenk nicht kaputt. Welcher Brief offen ist, hängt **allein am Datum** —
> das rechnet der Server jedes Mal neu aus. Verloren gingen nur die Häkchen
> und die Tagebucheinträge.

---

## Empfehlung: Vercel + Neon

Für ein Geschenk, das ein Jahr lang unfallfrei laufen soll, ist das die
bequemste Kombination: beide haben einen kostenlosen Tarif, es gibt nichts zu
warten, HTTPS ist automatisch dabei, und ein `git push` reicht zum
Aktualisieren.

### 1. Datenbank anlegen

1. Auf [neon.tech](https://neon.tech) ein Konto und ein Projekt anlegen
   (Region Frankfurt oder Amsterdam ist am nächsten).
2. Die **Verbindungszeichenkette** kopieren. Sie sieht so aus:
   ```
   postgresql://benutzer:passwort@ep-irgendwas.eu-central-1.aws.neon.tech/neondb?sslmode=require
   ```
   Wenn Neon eine gepoolte Variante anbietet („Pooled connection"), nimm die.

*Alternativen mit kostenlosem Tarif: Supabase, Railway Postgres, Vercel
Postgres. Alle sprechen dieselbe Sprache — die Zeichenkette einsetzen genügt.*

### 2. Auf Vercel deployen

1. Auf [vercel.com](https://vercel.com) anmelden, **New Project**, dieses
   GitHub-Repository auswählen.
2. Wichtig: bei **Root Directory** `jahrestag` eintragen — das Projekt liegt
   in einem Unterordner. Sonst findet Vercel nichts.
3. Unter **Environment Variables** eintragen:

   | Name | Wert |
   | --- | --- |
   | `MARCO_CODE` | euer Wort für Marco, mindestens 10 Zeichen |
   | `BELLA_CODE` | dein Wort, muss sich davon unterscheiden |
   | `SESSION_SECRET` | `openssl rand -base64 32` |
   | `DATABASE_URL` | die Zeichenkette von Neon |

4. **Deploy**. Fertig.

### 3. Nachschauen, ob alles stimmt

Ruf die Adresse auf. Du solltest die Anmeldung sehen.

- Steht dort stattdessen **„Noch nicht eingerichtet"**, fehlt eine der
  Variablen oder ein Codewort ist zu kurz. Der Kasten sagt genau, welche.
  Nach dem Nachtragen einmal **Redeploy** auslösen.
- Melde dich mit **Bellas** Codewort an, nicht mit Marcos. Vor dem
  10. Oktober 2026 sind alle Umschläge zu — mit Marcos Wort sähe man nur
  dreizehn Countdowns und dächte, es sei kaputt.
- Melde dich an, öffne einen Brief, setz ein Häkchen. Dann in Vercel ein
  **Redeploy** auslösen und nochmal nachsehen: Häkchen noch da? Dann sitzt
  die Datenbank richtig.

---

## Alternative: ein Hoster mit Festplatte

Wenn du gar keine Datenbank einrichten willst — bei Railway, Fly.io, einem
kleinen Server oder einem Raspberry Pi zu Hause:

1. `MARCO_CODE`, `BELLA_CODE` und `SESSION_SECRET` als Umgebungsvariablen
   setzen. `DATABASE_URL` **nicht** setzen.
2. Ein **Volume** einhängen, das Neustarts übersteht, und
   `DATABASE_PATH=/data/briefe.db` daraufzeigen lassen (Pfad je nach Anbieter).
3. Bauen und starten:
   ```bash
   npm ci
   npm run build
   npm start
   ```

Ohne Volume läuft es zwar, aber die Häkchen sind nach jedem Neustart weg —
genau das, was wir vermeiden wollen.

---

## Das Codewort

Es ist das Einzige, was zwischen Fremden und euren Briefen steht. Die App
verlangt deshalb mindestens 10 Zeichen und lehnt die Beispielwerte aus dem
Quelltext ab.

Gut sind mehrere Wörter, die zu euch gehören und die man sich sagen kann:

```
marcolino-popolino-2019
unser-erstes-date-war-im-oktober
```

Schlecht sind einzelne Wörter, Namen allein oder Geburtsdaten.

Schick Marco den Link und das Wort am besten über zwei verschiedene Wege —
den Link per Nachricht, das Wort mündlich. Dann steht beides nirgends
zusammen.

---

## Später etwas ändern

Briefe umschreiben, Fotos tauschen, Daten verschieben: Datei ändern, committen,
pushen. Vercel baut automatisch neu. Bei einem Hoster mit Festplatte einmal
`npm run build && npm start`.

Was Marco angeklickt und geschrieben hat, überlebt jeden Deploy — es liegt in
der Datenbank, nicht im Code.

---

## Kurze Prüfliste

- [ ] `MARCO_CODE`, `BELLA_CODE`, `SESSION_SECRET` gesetzt, Codewörter ≥ 10 Zeichen und verschieden
- [ ] `DATABASE_URL` gesetzt (serverlos) **oder** Volume + `DATABASE_PATH` (mit Festplatte)
- [ ] Adresse aufrufen: Anmeldung erscheint, **kein** roter Kasten
- [ ] Ohne Anmeldung `/brief/1` aufrufen → landet auf der Anmeldeseite
- [ ] Anmelden, Brief öffnen, Häkchen setzen
- [ ] Redeploy auslösen, nachsehen: Häkchen noch da
- [ ] Auf dem Handy testen — dort wird Marco es öffnen
