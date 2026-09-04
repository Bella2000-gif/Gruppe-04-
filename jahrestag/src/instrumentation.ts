/**
 * Läuft einmal beim Start des Servers. Wenn die Seite öffentlich steht, aber
 * noch nicht richtig eingerichtet ist, soll das im Log stehen — und zwar so,
 * dass man es nicht übersieht.
 *
 * Die Prüfung wird bewusst erst hier hineingeladen und nur in der
 * Node-Umgebung ausgeführt: Next übersetzt diese Datei auch für die
 * Edge-Runtime, und dort gibt es das `node:crypto` aus `lib/auth` nicht.
 */
export async function register() {
  if (process.env.NEXT_RUNTIME !== "nodejs") return;

  const { konfigProblem } = await import("@/lib/auth");
  const problem = konfigProblem();
  if (!problem) return;

  const rahmen = "═".repeat(72);
  console.error(
    `\n${rahmen}\n  ACHTUNG — die Seite ist noch nicht sicher eingerichtet\n\n  ${problem}\n${rahmen}\n`,
  );
}
