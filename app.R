# SDM-Demo
#
# Ordnerstruktur (Wichtig!!1)
#   data/
#     species_data.csv
#     climate_scenarios/
#        curclim.csv
#        <beliebiger_name>.csv
#        <beliebiger_name>.csv
#
# Zusätzlich benötigtes Paket ggü. der Ursprungsversion:
#   install.packages("shinycssloaders")   # für die Ladeanimationen


library(shiny)
library(ggplot2)
library(readr)
library(sf)
library(rnaturalearth)
library(bslib)
library(shinycssloaders)

# Vorbereitung

species_file  <- "data/species_data.csv"
climate_dir   <- "data/climate_scenarios"
baseline_name <- "curclim"


# Deutsche Trivialnamen der Arten
species_labels <- c(
  Nucifraga_caryocatactes  = "Tannenhäher",
  Merops_apiaster          = "Bienenfresser",
  Grus_grus                = "Kranich",
  Coenagrion_mercuriale    = "Helm-Azurjungfer",
  Ophiogomphus_cecilia     = "Grüne Flussjungfer",
  Phaneroptera_falcata     = "Punktierte Zartschrecke",
  Pinus_cembra             = "Zirbelkiefer",
  Corylus_avellana         = "Hasel"
)


scenario_labels <- c(
  curclim = "Aktuelles Klima",
  climFutCC2670 = "RCP 2.6 2070 (CC)",
  climFutCC6070 = "RCP 6.0 2070 (CC)",
  climFutMC2670 = "RCP 2.6 2070 (MC)",
  climFutMC6070 = "RCP 6.0 2070 (MC)"
)


# 1. Daten laden

species_data <- read_csv(species_file, show_col_types = FALSE)

climate_files <- list.files(climate_dir, pattern = "\\.csv$", full.names = TRUE)

climate_list <- setNames(
  lapply(climate_files, read_csv, show_col_types = FALSE),
  tools::file_path_sans_ext(basename(climate_files))
)

species_cols <- setdiff(names(species_data), c("lat", "lon"))


# 2. Europa-Umriss laden

europe_sf   <- ne_countries(continent = "Europe", scale = "medium", returnclass = "sf")
europe_bbox <- st_bbox(c(xmin = -25, xmax = 45, ymin = 34, ymax = 72), crs = st_crs(4326))
europe_sf   <- st_crop(europe_sf, europe_bbox)   # Ergebnis wird jetzt tatsächlich verwendet


# 3. SDM

fit_species_model <- function(species_col, species_df, climate_df) {
  train_df <- merge(
    species_df[, c("lat", "lon", species_col)],
    climate_df[, c("lat", "lon", "bio1_mean", "bio12_mean")],
    by = c("lat", "lon")
  )
  names(train_df)[names(train_df) == species_col] <- "presence"
  
  glm(
    presence ~ bio1_mean + I(bio1_mean^2) +
      bio12_mean + I(bio12_mean^2),
    family = "binomial", data = train_df)
}

species_models <- setNames(
  lapply(species_cols, fit_species_model,
         species_df = species_data,
         climate_df = climate_list[[baseline_name]]),
  species_cols
)


# 4. Modell auf Klimaszenarien anpassen

predict_suitability <- function(model, climate_df) {
  climate_df$suitability <- predict(model, newdata = climate_df, type = "response")
  climate_df
}

# Rastergröße für die Kartendarstellung
grid_res <- function(df) {
  list(
    lon = min(diff(sort(unique(df$lon)))),
    lat = min(diff(sort(unique(df$lat))))
  )
}
res_baseline <- grid_res(climate_list[[baseline_name]])

# 5. UI

species_choices <- setNames(
  species_cols,
  ifelse(species_cols %in% names(species_labels),
         species_labels[species_cols],
         gsub("_", " ", species_cols))
)

scenario_choices <- setNames(
  names(climate_list),
  ifelse(names(climate_list) %in% names(scenario_labels),
         scenario_labels[names(climate_list)],
         names(climate_list))
)


ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "lumen"),
  tags$head(
    tags$style(HTML("
       body {margin: auto;}
  "))
  ),
    
  titlePanel("Species Distribution Models"),
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 selectInput("species", "Art auswählen",
                             choices = species_choices),
                 selectInput("scenario", "Klimaszenario auswählen",
                             choices = scenario_choices,
                             selected = baseline_name),
                 hr(),
                 uiOutput("species_image"),
                 
                 hr(),
                 strong("Modellinfo:"),
                 verbatimTextOutput("model_info"),
                
    ),
    
    mainPanel(width = 10,
              tabsetPanel(
                tabPanel("Hintergrund",
                         br(),
                         h4("Was zeigt diese App?"),
                         p("Diese App demonstriert das Konzept eines Species Distribution Models (SDM): ",
                           "Aus bekannten Vorkommen einer Art und den Klimabedingungen an diesen Orten wird ein statistisches ",
                           "Modell (hier ein logistisches Regressionsmodell, GLM) trainiert, das die Vorkommenswahrscheinlichkeit ",
                           "der Art in Abhängigkeit von Temperatur und Niederschlag beschreibt."),
                         p("Dieses Modell wird anschließend auf neue Klimabedingungen (Hier potentielle Zukunftsszenarien) angewendet, ",
                           "um vorherzusagen, wie sich das geeignete Klimaareal der Art verändern könnte."),
                         
                         h4("Was zeigen mir die Karten?"),
                         tags$ul(
                           tags$li("Karte 1: Vorhersage unter aktuellem Klima, mit beobachteten Vorkommen (rote Punkte) zum Abgleich."),
                           tags$li("Karte 2: Vorhersage unter dem gewählten Klimaszenario."),
                           tags$li("Karte 3: Differenz der beiden Karten. Hier sieht man direkt, wo die Art profitieren (Lila) oder verlieren (Orange) könnte.")
                         ),
                         h4("Herkunft der Klimadaten"),
                         p("Sowohl die Daten zum heutigen Klima als auch die Zukunftsszenarien stammen aus der WorldClim ",
                           "Version 1.4 (worldclim.org, Hijmans et al. 2005)."),
                         p("Es stehen verschiedene Zukunftsszenarien zur Verfügung, jeweils kombiniert aus:"),
                         tags$ul(
                           tags$li("Einem von vier repräsentativen Konzentrationspfaden (RCPs 2.6, 4.5, 6.0 und 8.5). Hier werden RCP 2.6 und RCP 6.0 angeboten."),
                           tags$li("Einem von mehreren allgemeinen Zirkulationsmodellen bzw. globalen Klimamodellen (GCMs): MIROC5 (\"MC\", Modell für interdisziplinäre Klimaforschung) und CCSM4.0 (\"CC\", Gemeinschaftliches Klimasystemmodell).")
                         ),
                         p("Beide GCMs stammen aus dem IPCC-Bericht von 2014.")
                ),
                
                tabPanel("Karten & Kurven",
                         br(),
                         fluidRow(
                           column(
                             width = 6,
                             h4("Aktuelles Klima (Referenz)"),
                             withSpinner(
                               plotOutput("map_current", height = "550px"),
                               color = "#2C3E50"
                             )
                           ),
                           
                           column(
                             width = 6,
                             h4("Gewähltes Szenario"),
                             withSpinner(
                               plotOutput("map_scenario", height = "550px"),
                               color = "#2C3E50"
                             )
                           ),
                           
                           column(
                             width = 6,
                             h4("Veränderung (Szenario − Referenz)"),
                             withSpinner(
                               plotOutput("map_diff", height = "550px"),
                               color = "#2C3E50"
                             )
                           )
                         ),
                         helpText("Rote Punkte auf der linken Karte = tatsächlich beobachtete Vorkommen. ",
                                  "Lila in der rechten Karte = Zunahme, Orange = Abnahme der Vorkommenswahrscheinlichkeit."),
                         hr(),
                         h4("Response Curves der Art"),
                         fluidRow(
                           column(6, withSpinner(plotOutput("response_curve_temp", height = "360px"), color = "#2C3E50")),
                           column(6, withSpinner(plotOutput("response_curve_precip", height = "360px"), color = "#2C3E50"))
                         )
                )
              )
    )
  )
)


# 6. Server

server <- function(input, output, session) {
  
  # Anzeigename der aktuell gewählten Art, für Plot-Titel
  species_label <- reactive({
    names(species_choices)[species_choices == input$species]
  })
  
  scenario_label <- reactive({
    names(scenario_choices)[scenario_choices == input$scenario]
  })
  
  output$species_image <- renderUI({
    
    species <- input$species
    
    filename <- paste0(species, ".jpg")
    
    tags$div(
      style = "text-align: center;",
      
      tags$img(
        src = filename,
        style = "
        max-width: 100%;
        max-height: 450px;
        width: auto;
        height: auto;
        object-fit: contain;
      "
      )
    )
    
  })
  
  
  presence_points <- reactive({
    df <- species_data[, c("lat", "lon", input$species)]
    names(df)[3] <- "presence"
    df[df$presence == 1, ]
  })
  
  data_current <- reactive({
    predict_suitability(species_models[[input$species]], climate_list[[baseline_name]])
  })
  
  data_scenario <- reactive({
    predict_suitability(species_models[[input$species]], climate_list[[input$scenario]])
  })
  
  data_diff <- reactive({
    cur  <- data_current()[, c("lat", "lon", "suitability")]
    scen <- data_scenario()[, c("lat", "lon", "suitability")]
    merged <- merge(cur, scen, by = c("lat", "lon"), suffixes = c("_current", "_scenario"))
    merged$diff <- merged$suitability_scenario - merged$suitability_current
    merged
  })
  
  
  make_map <- function(df, title, show_points = FALSE) {
    p <- ggplot() +
      geom_tile(data = df, aes(x = lon, y = lat, fill = suitability),
                width = res_baseline$lon, height = res_baseline$lat)
    
    if (!is.null(europe_sf)) {
      p <- p + geom_sf(data = europe_sf, fill = NA, color = "grey30",
                       linewidth = 0.2, inherit.aes = FALSE)
    }
    if (show_points) {
      p <- p + geom_point(data = presence_points(), aes(x = lon, y = lat),
                          color = "red", size = 0.7, alpha = 0.6)
    }
    
    p <- p + scale_fill_viridis_c(limits = c(0, 1), name = "Vorkommens-\nwahrscheinlichkeit") +
      theme_minimal(base_size = 16) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            legend.title = element_text(hjust = 0.5)) +
      labs(x = NULL, y = NULL, title = title)
    
    if (!is.null(europe_sf)) {
      p <- p + coord_sf(xlim = range(df$lon), ylim = range(df$lat), expand = FALSE)
    } else {
      p <- p + coord_fixed(ratio = 1)
    }
    p
  }
  
  make_diff_map <- function(df, title) {
    max_abs <- max(abs(df$diff), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
    
    p <- ggplot() +
      geom_tile(data = df, aes(x = lon, y = lat, fill = diff),
                width = res_baseline$lon, height = res_baseline$lat)
    
    if (!is.null(europe_sf)) {
      p <- p + geom_sf(data = europe_sf, fill = NA, color = "grey30",
                       linewidth = 0.2, inherit.aes = FALSE)
    }
    
    p <- p + scale_fill_gradient2(low = "#e66101", mid = "#f7f7f7", high = "#5e3c99",
                                  midpoint = 0, limits = c(-max_abs, max_abs),
                                  name = "Veränderung der\nVorkommens-\nwahrscheinlichkeit") +
      theme_minimal(base_size = 16) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            legend.title = element_text(hjust = 0.5)) +
      labs(x = NULL, y = NULL, title = title)
    
    if (!is.null(europe_sf)) {
      p <- p + coord_sf(xlim = range(df$lon), ylim = range(df$lat), expand = FALSE)
    } else {
      p <- p + coord_fixed(ratio = 1)
    }
    p
  }
  
  output$map_current  <- renderPlot({
    make_map(data_current(), paste0(species_label(), " \u2013 Aktuelles Klima"), show_points = TRUE)
  })
  
  output$map_scenario <- renderPlot({
    make_map(data_scenario(), paste0(species_label(), " \u2013 ", scenario_label()), show_points = FALSE)
  })
  
  output$map_diff <- renderPlot({
    make_diff_map(data_diff(), paste0("Veränderung: ", scenario_label(), " vs. Referenz"))
  })
  
  output$model_info <- renderText({
    model <- species_models[[input$species]]
    n <- nrow(model$model)
    n_presence <- sum(model$model$presence)
    paste0(
      "Trainiert auf ", n, " Rasterzellen\n",
      "davon ", n_presence, " mit Vorkommen (",
      round(100 * n_presence / n, 1), " %)"
    )
  })
  
  output$response_curve_temp <- renderPlot({
    base_df <- climate_list[[baseline_name]]
    bio1_seq <- seq(min(base_df$bio1_mean), max(base_df$bio1_mean), length.out = 200)
    newdata <- data.frame(
      bio1_mean = bio1_seq,
      bio12_mean = mean(base_df$bio12_mean, na.rm = TRUE)
    )
    newdata$suitability <- predict(species_models[[input$species]],
                                   newdata = newdata, type = "response")
    
    ggplot(newdata, aes(x = bio1_mean / 10, y = suitability)) +
      geom_line(linewidth = 1.2, color = "darkgreen") +
      ylim(0, 1) +
      theme_minimal(base_size = 16) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16)) +
      labs(x = "Jahresdurchschnittstemperatur (°C)",
           y = "Vorkommenswahrscheinlichkeit",
           title = species_label())
  })
  
  output$response_curve_precip <- renderPlot({
    
    base_df <- climate_list[[baseline_name]]
    
    bio12_seq <- seq(min(base_df$bio12_mean), max(base_df$bio12_mean), length.out = 200)
    
    newdata <- data.frame(
      bio1_mean = mean(base_df$bio1_mean, na.rm = TRUE),
      bio12_mean = bio12_seq
    )
    
    newdata$suitability <- predict(species_models[[input$species]],
                                   newdata = newdata, type = "response")
    
    ggplot(newdata, aes(x = bio12_mean, y = suitability)) +
      geom_line(linewidth = 1.2, color = "steelblue") +
      ylim(0, 1) +
      theme_minimal(base_size = 16) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16)) +
      labs(x = "Jahresniederschlag (mm)",
           y = "Vorkommenswahrscheinlichkeit",
           title = species_label())
    
  })
  
}


# 7. App starten

shinyApp(ui = ui, server = server)