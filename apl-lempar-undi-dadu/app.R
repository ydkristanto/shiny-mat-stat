# Paket ----
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)

# Tautan ----
tautan_apl_lain <- tags$a(
  shiny::icon("shapes"),
  "Lainnya",
  href = "https://kristantomath.com/matematika-untuk-smp-mts-kelas-ix/",
  target = "_blank"
)
tautan_github <- tags$a(
  shiny::icon("github"),
  "Github",
  href = "https://github.com/ydkristanto/apl-lempar-undi-dadu",
  target = "_blank"
)

# Antarmuka ----
ui <- page_navbar(
  title = "Lempar Undi Dadu",
  id = "lempar_undi_dadu",
  ## Sidebar ----
  sidebar = sidebar(
    ### Sidebar simulasi ----
    conditionalPanel(
      "input.lempar_undi_dadu === 'Simulasi'",
      #### Definisi sukses ----
      selectInput(
        "sukses",
        div("Mata dadu sukses:", style = "font-weight: bold;"),
        choices = c("1", "2", "3", "4", "5", "6"),
        selected = "5",
        multiple = TRUE
      ),
      #### Banyak lempar undi ----
      selectInput(
        "n_lempar",
        div("Banyak lempar undi:", style = "font-weight: bold;"),
        choices = c("1", "10", "100"),
        selected = "1",
        multiple = FALSE
      ),
      #### Pelempar ----
      selectInput(
        "nama_pelempar",
        div("Pelempar undi:", style = "font-weight: bold;"),
        choices = c("Abel", "Ahmad", "Karuna", "Paulina", "Sondang"),
        selected = "Karuna",
        multiple = TRUE,
        selectize = TRUE
      ),
      #### Garis bantu ----
      checkboxInput(
        "garis_bantu",
        "Tampilkan garis bantu",
        value = FALSE
      ),
      hr(),
      #### Tombol eksperimen ----
      p(
        actionButton(
          "tombol_eksperimen",
          "Lempar undi!",
          width = "100%"
        )
      ),
      #### Tombol hapus ----
      p(
        actionButton(
          "tombol_hapus",
          "Hapus data",
          width = "100%"
        )
      )
    ),
    conditionalPanel(
      "input.lempar_undi_dadu === 'Informasi'",
      h4(
        "Deskripsi",
        style = "font-weight: bold; font-size: inherit;"
      ),
      p(
        "Aplikasi Shiny ini mensimulasikan frekuensi relatif suatu kejadian ketika percobaannya semakin banyak."
      ),
      hr(),
      h4(
        "Lisensi MIT",
        style = "font-weight: bold; font-size: inherit;"
      ),
      p(
        "Copyright © 2024 Yosep Dwi Kristanto"
      )
    )
  ),
  ## Simulasi ----
  nav_panel(
    title = "Simulasi",
    icon = icon("dice"),
    navset_card_underline(
      ### Luaran grafik ----
      nav_panel(
        title = "Grafik",
        plotOutput("luaran_grafik")
      ),
      ### Luaran tabel ----
      nav_panel(
        title = "Tabel",
        tableOutput("luaran_tabel")
      ),
      full_screen = TRUE
    )
  ),
  ## Informasi ----
  nav_panel(
    title = "Informasi",
    icon = icon("circle-info"),
    layout_column_wrap(
      width = 1 / 2,
      navset_card_underline(
        #### Tentang ----
        nav_panel(
          title = "Tentang",
          p("Aplikasi berbasis web ini memiliki dua tujuan. Pertama, aplikasi ini menunjukkan frekuensi relatif atau peluang empiris kejadian yang melibatkan lempar undi dadu. Frekuensi relatif tersebut disajikan dalam diagram maupun tabel."),
          p("Tujuan kedua, aplikasi ini mendemonstrasikan apa yang terjadi terhadap frekuensi relatif ketika banyak percobaan semakin besar. Frekuensi relatif tersebut semakin mendekati peluang teoretisnya ketika banyak percobaannya semakin besar."),
          hr(),
          p("Aplikasi ini merupakan aktivitas pembelajaran penunjang untuk buku Matematika untuk SMP/MTs Kelas IX yang diterbitkan oleh Pusat Perbukuan, Kementerian Pendidikan, Kebudayaan, Riset, dan Teknologi. Unduh atau baca bukunya di ", a("https://buku.kemdikbud.go.id/katalog/matematika-untuk-smpmts-kelas-ix.", href = "https://buku.kemdikbud.go.id/katalog/matematika-untuk-smpmts-kelas-ix", target = "_blank"), " Untuk melihat sumber belajar penunjang lainnya untuk buku tersebut, silakan kunjungi ", a("https://kristantomath.com/matematika-untuk-smp-mts-kelas-ix/.", href = "https://kristantomath.com/matematika-untuk-smp-mts-kelas-ix/", target = "_blank"))
        ),
        nav_panel(
          #### Alat ----
          title = "Alat",
          p("Aplikasi ini dikembangkan dengan menggunakan bahasa pemrograman", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah. Tata letak dasbor ini diatur dengan menggunakan ", a("bslib.", href = "https://CRAN.R-project.org/package=bslib", target = "_blank")),
          p("Paket-paket ", a("dplyr,", href = "https://CRAN.R-project.org/package=dplyr", target = "_blank"), a("tidyr,", href = "https://CRAN.R-project.org/package=tidyr", target = "_blank"), " dan ", a("ggplot2", href = "https://ggplot2.tidyverse.org", target = "_blank"), " juga digunakan dalam pengembangan aplikasi ini. Paket dplyr digunakan untuk memanipulasi data, tidyr untuk membuat data yang rapi, dan ggplot2 untuk membuat grafik.")
        ),
        nav_panel(
          #### Pengembang ----
          title = "Pengembang",
          p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta.")
        ),
        nav_panel(
          #### Kode Sumber ----
          title = "Kode Sumber",
          p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/apl-lempar-undi-dadu", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-lempar-undi-dadu/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-lempar-undi-dadu/pulls", target = "_blank"), "di repositori tersebut.")
        )
      ),
      #### Hukum Bilangan Besar ----
      card(
        card_header("Hukum Bilangan Besar"),
        p("Ide matematis yang didemonstrasikan dalam aplikasi ini merupakan kasus khusus dari Hukum Bilangan Besar. Simulasi mengenai hukum ini dapat diakses melalui ", a("https://people.usd.ac.id/~ydkristanto/app/hukum-bil-besar/.", href = "https://people.usd.ac.id/~ydkristanto/app/hukum-bil-besar/", target = "_blank"))
      )
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Tautan",
    nav_item(tautan_apl_lain),
    nav_item(tautan_github),
    icon = shiny::icon("link"),
    align = "right"
  )
)

# Peladen ----
server <- function(input, output, session) {
  ## Data awal ----
  data_eksperimen <- reactiveVal(
    tibble(
      `Hasil Abel` = integer(),
      `Kategori Abel` = character(),
      `Frek. Rel. Abel` = numeric(),
      `Hasil Ahmad` = integer(),
      `Kategori Ahmad` = character(),
      `Frek. Rel. Ahmad` = numeric(),
      `Hasil Karuna` = integer(),
      `Kategori Karuna` = character(),
      `Frek. Rel. Karuna` = numeric(),
      `Hasil Paulina` = integer(),
      `Kategori Paulina` = character(),
      `Frek. Rel. Paulina` = numeric(),
      `Hasil Sondang` = integer(),
      `Kategori Sondang` = character(),
      `Frek. Rel. Sondang` = numeric()
    )
  )
  
  ## Menambah baris data ----
  observeEvent(input$tombol_eksperimen, {
    # Input
    sukses <- input$sukses
    n_lempar <- as.numeric(input$n_lempar)
    
    # Hasil lempar undi
    matriks_hasil <- matrix(
      sample(1:6, 5 * n_lempar, replace = TRUE),
      ncol = 5,
      byrow = FALSE,
      dimnames = list(
        1:n_lempar,
        c("Abel", "Ahmad", "Karuna", "Paulina", "Sondang")
      )
    )
    
    # Menambah data eksperimen
    data_eksperimen() %>% 
      add_row(
        `Hasil Abel` = matriks_hasil[, "Abel"],
        `Kategori Abel` = ifelse(
          `Hasil Abel` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Hasil Ahmad` = matriks_hasil[, "Ahmad"],
        `Kategori Ahmad` = ifelse(
          `Hasil Ahmad` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Hasil Karuna` = matriks_hasil[, "Karuna"],
        `Kategori Karuna` = ifelse(
          `Hasil Karuna` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Hasil Paulina` = matriks_hasil[, "Paulina"],
        `Kategori Paulina` = ifelse(
          `Hasil Paulina` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Hasil Sondang` = matriks_hasil[, "Sondang"],
        `Kategori Sondang` = ifelse(
          `Hasil Sondang` %in% sukses,
          "Sukses", "Gagal"
        )
      ) %>% data_eksperimen()
  })
  
  ## Menghapus baris data ----
  observeEvent(input$tombol_hapus, {
    data_eksperimen(
      tibble(
        `Hasil Abel` = integer(),
        `Kategori Abel` = character(),
        `Frek. Rel. Abel` = numeric(),
        `Hasil Ahmad` = integer(),
        `Kategori Ahmad` = character(),
        `Frek. Rel. Ahmad` = numeric(),
        `Hasil Karuna` = integer(),
        `Kategori Karuna` = character(),
        `Frek. Rel. Karuna` = numeric(),
        `Hasil Paulina` = integer(),
        `Kategori Paulina` = character(),
        `Frek. Rel. Paulina` = numeric(),
        `Hasil Sondang` = integer(),
        `Kategori Sondang` = character(),
        `Frek. Rel. Sondang` = numeric()
      )
    )
  })
  
  ## Luaran grafik ----
  output$luaran_grafik <- renderPlot({
    # Input
    sukses <- input$sukses
    nama_pelempar <- input$nama_pelempar
    n_baris <- nrow(data_eksperimen())
    
    # Mempersiapkan data
    if (n_baris > 0) {
      data_abel <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(1:3) %>% 
        rename(
          hasil = `Hasil Abel`,
          kategori = `Kategori Abel`,
          fr = `Frek. Rel. Abel`
        ) %>% 
        mutate(
          "nama" = rep("Abel", n_baris),
          "lemparan" = 1:n_baris
        )
      data_ahmad <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(4:6) %>% 
        rename(
          hasil = `Hasil Ahmad`,
          kategori = `Kategori Ahmad`,
          fr = `Frek. Rel. Ahmad`
        ) %>% 
        mutate(
          "nama" = rep("Ahmad", n_baris),
          "lemparan" = 1:n_baris
        )
      data_karuna <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(7:9) %>% 
        rename(
          hasil = `Hasil Karuna`,
          kategori = `Kategori Karuna`,
          fr = `Frek. Rel. Karuna`
        ) %>% 
        mutate(
          "nama" = rep("Karuna", n_baris),
          "lemparan" = 1:n_baris
        )
      data_paulina <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(10:12) %>% 
        rename(
          hasil = `Hasil Paulina`,
          kategori = `Kategori Paulina`,
          fr = `Frek. Rel. Paulina`
        ) %>% 
        mutate(
          "nama" = rep("Paulina", n_baris),
          "lemparan" = 1:n_baris
        )
      data_sondang <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(13:15) %>% 
        rename(
          hasil = `Hasil Sondang`,
          kategori = `Kategori Sondang`,
          fr = `Frek. Rel. Sondang`
        ) %>% 
        mutate(
          "nama" = rep("Sondang", n_baris),
          "lemparan" = 1:n_baris
        )
    } else {
      data_abel <- data_eksperimen() %>% 
        select(1:3)
      data_ahmad <- data_eksperimen() %>% 
        select(4:6)
      data_karuna <- data_eksperimen() %>% 
        select(7:9)
      data_paulina <- data_eksperimen() %>% 
        select(10:12)
      data_sondang <- data_eksperimen() %>% 
        select(13:15)
    }
    
    # Data panjang
    data_panjang <- bind_rows(
      data_abel,
      data_ahmad,
      data_karuna,
      data_paulina,
      data_sondang
    )
    
    # Plot
    if (n_baris > 0) {
      plot <- data_panjang %>% 
        filter(
          nama %in% nama_pelempar
        ) %>% 
        ggplot(aes(x = lemparan, y = fr)) +
        geom_line(
          aes(group = nama, color = nama),
          linewidth = 1.5,
          alpha = .8
        ) +
        geom_point(
          aes(color = nama),
          size = 3,
          alpha = .8
        ) +
        scale_y_continuous(
          breaks = c(seq(0, 1, 0.25)),
          limits = c(0, 1)
        ) +
        scale_color_viridis_d(
          name = "Nama Pelempar"
        ) +
        theme_bw(base_size = 16) +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold")
        ) +
        labs(
          title = "Hubungan Banyak Lempar Undi dan Frekuensi Relatif",
          x = "Banyak Lempar Undi",
          y = "Frekuensi Relatif"
        )
      if (input$garis_bantu == TRUE) {
        plot <- plot +
          geom_hline(
            yintercept = length(sukses) / 6,
            linetype = "dashed",
            linewidth = 1
          ) +
          scale_y_continuous(
            breaks = c(seq(0, 1, 0.25), round(length(sukses) / 6, 2)),
            limits = c(0, 1)
          )
      }
    } else {
      plot <- ggplot(
        data = data.frame(x = as.integer(c(0, 10)), y = c(0, 1)),
        aes(x, y)
      ) +
        geom_blank() +
        geom_label(
          aes(
            x = 5, y = .5,
            label = "Silakan mulai mengumpulkan data!",
            hjust = .5,
            size = 24
          ),
          show.legend = FALSE
        ) +
        theme_bw(
          base_size = 16
        ) +
        ylim(0, 1) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        labs(
          title = "Hubungan Antara Banyak Lempar Undi dan Frekuensi Relatif",
          x = "Banyak Lempar Undi",
          y = "Frekuensi Relatif"
        )
    }
    
    plot
    
  })
  
  ## Luaran tabel ----
  output$luaran_tabel <- renderTable(
    data_eksperimen() %>% 
      mutate(
        `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
          seq_along(`Hasil Abel`),
        `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
          seq_along(`Hasil Ahmad`),
        `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
          seq_along(`Hasil Karuna`),
        `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
          seq_along(`Hasil Paulina`),
        `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
          seq_along(`Hasil Sondang`)
      ) %>% 
      select(
        matches(paste0(input$nama_pelempar, collapse = "|"))
      ),
    striped = TRUE,
    hover = TRUE,
    rownames = TRUE,
    width = "100%"
  )
    
}

# Aplikasi ----
shinyApp(ui = ui, server = server)
