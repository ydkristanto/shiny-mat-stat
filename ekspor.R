# Mengekspor aplikasi Shiny ke folder "app".
shinylive::export(
  appdir = "apl-derajat-radian",
  destdir = "app",
  subdir = "derajat-radian"
)
shinylive::export(
  appdir = "apl-dist-t",
  destdir = "app",
  subdir = "dist-t"
)
shinylive::export(
  appdir = "apl-hukum-bil-besar",
  destdir = "app",
  subdir = "hukum-bil-besar"
)

shinylive::export(
  appdir = "apl-idn-pisa-2022",
  destdir = "app",
  subdir = "idn-pisa-2022"
)

shinylive::export(
  appdir = "apl-lempar-undi-dadu",
  destdir = "app",
  subdir = "lempar-undi-dadu"
)
shinylive::export(
  appdir = "apl-selang-kepercayaan",
  destdir = "app",
  subdir = "selang-kepercayaan"
)
shinylive::export(
  appdir = "apl-simpangan-baku",
  destdir = "app",
  subdir = "simpangan-baku"
)
shinylive::export(
  appdir = "apl-simulasi-tabungan",
  destdir = "app",
  subdir = "simulasi-tabungan"
)
shinylive::export(
  appdir = "apl-teor-lim-pusat",
  destdir = "app",
  subdir = "teor-lim-pusat"
)
shinylive::export(
  appdir = "apl-uji-t-welch",
  destdir = "app",
  subdir = "uji-t-welch"
)
shinylive::export(
  appdir = "app-vid-analytics",
  destdir = "app",
  subdir = "vid-analytics"
)

shinylive::export(
  appdir = "app-bib-math-educ",
  destdir = "app",
  subdir = "bib-math-educ"
)

shinylive::export(
  appdir = "reprex",
  destdir = "app",
  subdir = "reprex"
)

shinylive::export(
  appdir = "apl-kalkulator-matahari",
  destdir = "app",
  subdir = "kalkulator-matahari"
)

shinylive::export(
  appdir = "apl-jelajah-raja-ampat",
  destdir = "app",
  subdir = "jelajah-raja-ampat"
)

# Menguji aplikasi dalam folder "docs".
httpuv::runStaticServer("app")
