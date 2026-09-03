"use client";

import { useEffect } from "react";
import { Konfetti } from "./Konfetti";

/**
 * Der Moment direkt nach dem Öffnen: Blütenblätter über dem Brief.
 *
 * Der Umschlag hängt beim Weiterleiten `?frisch=1` an die Adresse. Das wird
 * hier sofort wieder aus der Adresszeile entfernt — sonst würde beim
 * Neuladen jedes Mal wieder Konfetti fallen und der Zauber wäre schnell weg.
 */
export function FrischGeoeffnet() {
  useEffect(() => {
    const url = new URL(window.location.href);
    if (url.searchParams.has("frisch")) {
      url.searchParams.delete("frisch");
      window.history.replaceState(null, "", url.pathname + url.search);
    }
  }, []);

  return <Konfetti aktiv />;
}
