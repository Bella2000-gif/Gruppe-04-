import type { Metadata, Viewport } from "next";
import "./globals.css";
import { SvgDefinitionen } from "@/components/SvgDefinitionen";

export const metadata: Metadata = {
  title: "Dreizehn Briefe · für Marcolino Popolino",
  description:
    "Ein Jahr, dreizehn Umschläge, dreizehn Verabredungen. Vom siebten bis zum achten Jahrestag.",
  robots: { index: false, follow: false },
  appleWebApp: { capable: true, title: "Dreizehn Briefe", statusBarStyle: "default" },
};

export const viewport: Viewport = {
  themeColor: [
    { media: "(prefers-color-scheme: light)", color: "#faf4ea" },
    { media: "(prefers-color-scheme: dark)", color: "#17140f" },
  ],
  width: "device-width",
  initialScale: 1,
  viewportFit: "cover",
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="de" className="h-full">
      <head>
        {/* Setzt das gespeicherte Thema, bevor das erste Pixel gemalt wird —
            sonst blitzt beim Laden kurz die helle Variante auf. */}
        <script
          dangerouslySetInnerHTML={{
            __html:
              "try{var t=localStorage.getItem('thema');if(t==='hell'||t==='dunkel')document.documentElement.dataset.thema=t}catch(e){}",
          }}
        />
      </head>
      <body className="min-h-full antialiased">
        <div className="buehne korn" aria-hidden="true" />
        <SvgDefinitionen />
        {children}
      </body>
    </html>
  );
}
