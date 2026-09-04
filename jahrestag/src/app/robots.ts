import type { MetadataRoute } from "next";

/**
 * Suchmaschinen bleiben draußen. Das ist keine Zugangssperre — die macht die
 * Anmeldung —, aber es sorgt dafür, dass die Adresse nicht in Google auftaucht.
 */
export default function robots(): MetadataRoute.Robots {
  return {
    rules: [{ userAgent: "*", disallow: "/" }],
  };
}
