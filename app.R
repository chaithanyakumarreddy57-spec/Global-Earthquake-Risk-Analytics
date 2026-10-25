# ====================================================================
# Global Earthquake Risk Analytics - Shiny dashboard
#
# Run locally (from the project folder, after earthquake_analysis.R):
#   shiny::runApp()            # or press "Run App" in RStudio
#
# Needs these files (created by earthquake_analysis.R):
#   data/processed/earthquakes_clean.rds   (or .csv as a fallback)
# Optional (the "Models" tab shows a notice if they are missing):
#   outputs/tables/model_comparison.csv, feature_importance.csv,
#   cluster_profile.csv
# ====================================================================

# ---- packages --------------------------------------------------------
# Packages are NOT installed automatically: that is slow, needs internet
# and does not work on hosted servers. Run install_packages.R once instead.
required_pkgs <- c("shiny", "bslib", "dplyr", "ggplot2", "readr",
                   "leaflet", "plotly", "DT", "scales")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Missing R packages: ", paste(missing_pkgs, collapse = ", "),
       ". Run source('install_packages.R') once, then start the app again.",
       call. = FALSE)
}
# Explicit library() calls so hosting services (shinyapps.io) detect the dependencies
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)
library(plotly)
library(DT)
library(scales)

# ---- locate project files --------------------------------------------
# Works when started from the project root or from a sub-folder.
find_file <- function(...) {
  cand <- c(file.path(...), file.path("..", ...))
  cand <- cand[file.exists(cand)]
  if (length(cand)) cand[1] else NA_character_
}

rds_path <- find_file("data", "processed", "earthquakes_clean.rds")
csv_path <- find_file("data", "processed", "earthquakes_clean.csv")

if (!is.na(rds_path)) {
  eq <- readRDS(rds_path)
} else if (!is.na(csv_path)) {
  eq <- readr::read_csv(csv_path, show_col_types = FALSE)
} else {
  stop("Clean dataset not found (data/processed/earthquakes_clean.rds). ",
       "Run earthquake_analysis.R first and start the app from the project folder.",
       call. = FALSE)
}

needed <- c("datetime", "latitude", "longitude", "magnitude", "depth", "year",
            "risk_score", "risk_level", "cluster", "region")
absent <- setdiff(needed, names(eq))
if (length(absent)) {
  stop("Missing columns in the clean dataset: ", paste(absent, collapse = ", "),
       ". Re-run earthquake_analysis.R completely (Phase 12 saves the final file).",
       call. = FALSE)
}

risk_levels  <- c("Low", "Moderate", "High", "Severe")
risk_colours <- c(Low = "#4CAF50", Moderate = "#FFC107",
                  High = "#FF7043", Severe = "#B71C1C")

eq <- eq %>%
  mutate(risk_level = factor(risk_level, levels = risk_levels),
         cluster    = factor(cluster),
         region     = as.character(region),
         datetime   = as.character(datetime))

regions  <- sort(unique(eq$region))
clusters <- levels(eq$cluster)

mag_min   <- floor(min(eq$magnitude, na.rm = TRUE) * 10) / 10
mag_max   <- ceiling(max(eq$magnitude, na.rm = TRUE) * 10) / 10
depth_min <- floor(min(eq$depth, na.rm = TRUE))
depth_max <- ceiling(max(eq$depth, na.rm = TRUE))
year_min  <- min(eq$year, na.rm = TRUE)
year_max  <- max(eq$year, na.rm = TRUE)

# ---- optional model / cluster tables ---------------------------------
read_optional <- function(name) {
  p <- find_file("outputs", "tables", name)
  if (is.na(p)) NULL else readr::read_csv(p, show_col_types = FALSE)
}
model_cmp  <- read_optional("model_comparison.csv")
feat_imp   <- read_optional("feature_importance.csv")

# ---- UI ---------------------------------------------------------------
ui <- page_navbar(
  title = "Global Earthquake Risk Analytics",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  sidebar = sidebar(
    title = "Filters", width = 300,
    sliderInput("magnitude", "Magnitude", min = mag_min, max = mag_max,
                value = c(mag_min, mag_max), step = 0.1),
    sliderInput("depth", "Depth (km)", min = depth_min, max = depth_max,
                value = c(depth_min, depth_max)),
    sliderInput("years", "Year", min = year_min, max = year_max,
                value = c(year_min, year_max), step = 1, sep = ""),
    checkboxGroupInput("risk", "Risk level", choices = risk_levels,
                       selected = risk_levels, inline = TRUE),
    selectInput("region", "Region", choices = regions, selected = regions,
                multiple = TRUE),
    selectInput("cluster", "K-means cluster", choices = clusters,
                selected = clusters, multiple = TRUE),
    actionButton("reset", "Reset filters", class = "btn-outline-primary w-100")
  ),

  nav_panel("Overview",
    layout_column_wrap(
      width = "180px", fill = FALSE,
      value_box("Earthquakes",        textOutput("total_quakes")),
      value_box("Average magnitude",  textOutput("avg_magnitude")),
      value_box("Maximum magnitude",  textOutput("max_magnitude")),
      value_box("Median depth (km)",  textOutput("median_depth")),
      value_box("High / Severe",      textOutput("high_risk"))
    ),
    layout_column_wrap(
      width = "420px",
      card(full_screen = TRUE, card_header("Magnitude distribution"),
           plotlyOutput("magnitude_plot", height = "380px")),
      card(full_screen = TRUE, card_header("Earthquakes per year"),
           plotlyOutput("year_plot", height = "380px"))
    ),
    layout_column_wrap(
      width = "420px",
      card(full_screen = TRUE, card_header("Depth distribution"),
           plotlyOutput("depth_plot", height = "380px")),
      card(full_screen = TRUE, card_header("Magnitude vs depth"),
           plotlyOutput("depth_magnitude_plot", height = "380px"))
    )
  ),

  nav_panel("Risk analysis",
    layout_column_wrap(
      width = "420px",
      card(full_screen = TRUE, card_header("Risk-level distribution"),
           plotlyOutput("risk_plot", height = "380px")),
      card(full_screen = TRUE, card_header("Mean risk score per year"),
           plotlyOutput("risk_year_plot", height = "380px"))
    ),
    card(full_screen = TRUE, card_header("Risk by region (filtered data)"),
         DTOutput("region_table"))
  ),

  nav_panel("Map",
    card(full_screen = TRUE,
         card_header(
           "Earthquake map",
           radioButtons("map_colour", NULL, choices = c("Magnitude", "Risk level"),
                        inline = TRUE)),
         textOutput("map_note"),
         leafletOutput("earthquake_map", height = "650px"))
  ),

  nav_panel("Clusters",
    card(card_header("K-means cluster profile (filtered data)"),
         p(class = "text-muted",
           "Clusters were fitted on 3-D position on the globe, log depth and magnitude ",
           "(k = 6, chosen in earthquake_analysis.R). See the report for the caveats."),
         DTOutput("cluster_table"))
  ),

  nav_panel("Models",
    uiOutput("model_ui")
  ),

  nav_panel("Data",
    card(card_header("Filtered earthquakes",
                     downloadButton("download", "Download CSV", class = "btn-sm")),
         DTOutput("earthquake_table"))
  ),

  nav_panel("About",
    card(
      h4("Risk score"),
      p("Each earthquake gets a score from 0 to 100: ",
        strong("60% magnitude"), " (scaled from 5.5 to the largest magnitude in the data) plus ",
        strong("40% shallowness"), " (1 minus depth/300 km, capped at 300 km). ",
        "Shallow earthquakes usually shake the surface harder."),
      p("Risk levels: ", strong("Low"), " (score up to 30), ", strong("Moderate"), " (30-42), ",
        strong("High"), " (42-55), ", strong("Severe"), " (above 55)."),
      p("This is a descriptive score for exploring the catalogue. It is not a hazard ",
        "forecast and ignores population, buildings and local geology."),
      h4("Data"),
      p("Significant earthquakes 1965-2016 (magnitude 5.5 and above), cleaned by ",
        code("earthquake_analysis.R"), ". Only events of type 'Earthquake' are kept."),
      h4("Project"),
      p("C Chaitanya Kumar Reddy (160124771035), B.E. Artificial Intelligence and Data Science, ",
        "Chaitanya Bharathi Institute of Technology (CBIT), Hyderabad.")
    )
  )
)

# ---- server -----------------------------------------------------------
server <- function(input, output, session) {

  no_data <- "No earthquakes match the current filters. Widen the filters or press Reset."

  filtered_data <- reactive({
    req(input$magnitude, input$depth, input$years)
    eq %>%
      filter(magnitude >= input$magnitude[1], magnitude <= input$magnitude[2],
             depth >= input$depth[1], depth <= input$depth[2],
             year >= input$years[1], year <= input$years[2],
             risk_level %in% input$risk,
             region %in% input$region,
             cluster %in% input$cluster)
  })

  observeEvent(input$reset, {
    updateSliderInput(session, "magnitude", value = c(mag_min, mag_max))
    updateSliderInput(session, "depth",     value = c(depth_min, depth_max))
    updateSliderInput(session, "years",     value = c(year_min, year_max))
    updateCheckboxGroupInput(session, "risk", selected = risk_levels)
    updateSelectInput(session, "region",  selected = regions)
    updateSelectInput(session, "cluster", selected = clusters)
  })

  # KPI boxes (safe when the filter returns no rows)
  output$total_quakes   <- renderText(scales::comma(nrow(filtered_data())))
  output$avg_magnitude  <- renderText({
    d <- filtered_data(); if (!nrow(d)) "-" else sprintf("%.2f", mean(d$magnitude))
  })
  output$max_magnitude  <- renderText({
    d <- filtered_data(); if (!nrow(d)) "-" else sprintf("%.1f", max(d$magnitude))
  })
  output$median_depth   <- renderText({
    d <- filtered_data(); if (!nrow(d)) "-" else sprintf("%.0f", median(d$depth))
  })
  output$high_risk      <- renderText({
    d <- filtered_data()
    if (!nrow(d)) return("-")
    n <- sum(d$risk_level %in% c("High", "Severe"))
    sprintf("%s (%.0f%%)", scales::comma(n), 100 * n / nrow(d))
  })

  # Overview plots
  output$magnitude_plot <- renderPlotly({
    d <- filtered_data(); validate(need(nrow(d) > 0, no_data))
    ggplotly(ggplot(d, aes(magnitude)) +
               geom_histogram(binwidth = 0.1, fill = "firebrick", colour = "white") +
               labs(x = "Magnitude", y = "Number of earthquakes") + theme_minimal())
  })

  output$year_plot <- renderPlotly({
    d <- filtered_data() %>% count(year); validate(need(nrow(d) > 0, no_data))
    ggplotly(ggplot(d, aes(year, n)) +
               geom_line(colour = "steelblue", linewidth = 1) + geom_point(colour = "steelblue") +
               labs(x = "Year", y = "Number of earthquakes") + theme_minimal())
  })

  output$depth_plot <- renderPlotly({
    d <- filtered_data(); validate(need(nrow(d) > 0, no_data))
    ggplotly(ggplot(d, aes(depth)) +
               geom_histogram(bins = 50, fill = "steelblue", colour = "white") +
               labs(x = "Depth (km)", y = "Number of earthquakes") + theme_minimal())
  })

  output$depth_magnitude_plot <- renderPlotly({
    d <- filtered_data()
    validate(need(nrow(d) > 2, "Not enough earthquakes match the current filters."))
    # plotly gets slow with many points: draw a sample, fit the trend on all rows
    pts <- if (nrow(d) > 4000) dplyr::slice_sample(d, n = 4000) else d
    ggplotly(ggplot(d, aes(depth, magnitude)) +
               geom_point(data = pts, alpha = 0.35, colour = "purple") +
               geom_smooth(method = "lm", formula = y ~ x, colour = "black") +
               labs(x = "Depth (km)", y = "Magnitude") + theme_minimal())
  })

  # Risk plots and table
  output$risk_plot <- renderPlotly({
    d <- filtered_data() %>% count(risk_level, .drop = FALSE)
    validate(need(sum(d$n) > 0, no_data))
    ggplotly(ggplot(d, aes(risk_level, n, fill = risk_level)) + geom_col() +
               scale_fill_manual(values = risk_colours) +
               labs(x = "Risk level", y = "Number of earthquakes") +
               theme_minimal() + theme(legend.position = "none"))
  })

  output$risk_year_plot <- renderPlotly({
    d <- filtered_data() %>% group_by(year) %>%
      summarise(mean_risk = mean(risk_score), .groups = "drop")
    validate(need(nrow(d) > 0, no_data))
    ggplotly(ggplot(d, aes(year, mean_risk)) +
               geom_line(colour = "firebrick", linewidth = 1) + geom_point(colour = "firebrick") +
               labs(x = "Year", y = "Mean risk score") + theme_minimal())
  })

  output$region_table <- renderDT({
    d <- filtered_data(); validate(need(nrow(d) > 0, no_data))
    d %>% group_by(Region = region) %>%
      summarise(Earthquakes = n(), `Mean magnitude` = mean(magnitude),
                `Mean risk score` = mean(risk_score),
                `% High/Severe` = 100 * mean(risk_level %in% c("High", "Severe")),
                .groups = "drop") %>%
      arrange(desc(`Mean risk score`)) %>%
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 10)) %>%
      formatRound(c("Mean magnitude", "Mean risk score", "% High/Severe"), 2)
  })

  # Map
  map_data_sampled <- reactive({
    d <- filtered_data()
    if (nrow(d) > 5000) dplyr::slice_sample(d, n = 5000) else d
  })

  output$map_note <- renderText({
    n <- nrow(filtered_data())
    if (n > 5000) sprintf("Showing a random sample of 5,000 of %s earthquakes.", scales::comma(n)) else ""
  })

  output$earthquake_map <- renderLeaflet({
    d <- map_data_sampled(); validate(need(nrow(d) > 0, no_data))
    popup <- paste0("<b>Magnitude:</b> ", d$magnitude,
                    "<br><b>Depth:</b> ", round(d$depth, 1), " km",
                    "<br><b>Risk score:</b> ", round(d$risk_score, 1),
                    "<br><b>Risk level:</b> ", d$risk_level,
                    "<br><b>Date:</b> ", substr(d$datetime, 1, 10))
    m <- leaflet(d) %>% addProviderTiles(providers$CartoDB.Positron)
    if (identical(input$map_colour, "Risk level")) {
      pal <- colorFactor(unname(risk_colours), levels = risk_levels)
      m %>% addCircleMarkers(~longitude, ~latitude, radius = ~pmax(3, (magnitude - 4) * 1.5),
                             color = ~pal(risk_level), fillOpacity = 0.7, stroke = FALSE,
                             popup = popup) %>%
        addLegend("bottomright", pal = pal, values = risk_levels, title = "Risk level")
    } else {
      pal <- colorNumeric("YlOrRd", domain = eq$magnitude)   # fixed to full data
      m %>% addCircleMarkers(~longitude, ~latitude, radius = ~pmax(3, (magnitude - 4) * 1.5),
                             color = ~pal(magnitude), fillOpacity = 0.7, stroke = FALSE,
                             popup = popup) %>%
        addLegend("bottomright", pal = pal, values = eq$magnitude, title = "Magnitude")
    }
  })

  # Clusters
  output$cluster_table <- renderDT({
    d <- filtered_data(); validate(need(nrow(d) > 0, no_data))
    d %>% group_by(Cluster = cluster) %>%
      summarise(Earthquakes = n(), `Mean latitude` = mean(latitude),
                `Mean longitude` = mean(longitude), `Mean magnitude` = mean(magnitude),
                `Mean depth (km)` = mean(depth), `Mean risk score` = mean(risk_score),
                .groups = "drop") %>%
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 10)) %>%
      formatRound(c("Mean latitude", "Mean longitude", "Mean magnitude",
                    "Mean depth (km)", "Mean risk score"), 2)
  })

  # Models (static results saved by earthquake_analysis.R; not affected by filters)
  output$model_ui <- renderUI({
    if (is.null(model_cmp)) {
      return(card(card_header("Models"),
                  p("outputs/tables/model_comparison.csv was not found. ",
                    "Run earthquake_analysis.R to create it.")))
    }
    best <- model_cmp %>% arrange(desc(f1), desc(auc)) %>% slice(1)
    tagList(
      card(card_header("Model comparison (held-out 20% test set)"),
           p(class = "text-muted",
             "Task: classify an earthquake as High/Severe vs Low/Moderate risk using location, ",
             "time, status and magnitude type. Magnitude and depth are excluded because they ",
             "define the risk score. These results were saved when the analysis script ran; ",
             "the filters on the left do not change them."),
           DTOutput("model_table"),
           p(class = "mt-3", "Best by F1 score: ", strong(best$model), sprintf(" (F1 = %.2f, AUC = %.2f).", best$f1, best$auc))),
      if (!is.null(feat_imp)) {
        card(full_screen = TRUE, card_header("Random forest feature importance"),
             plotlyOutput("importance_plot", height = "350px"))
      } else {
        card(p("outputs/tables/feature_importance.csv was not found."))
      }
    )
  })

  output$model_table <- renderDT({
    req(model_cmp)
    model_cmp %>%
      rename_with(~ c("Model", "Accuracy", "Precision", "Recall", "F1", "AUC")) %>%
      datatable(rownames = FALSE, options = list(dom = "t")) %>%
      formatRound(c("Accuracy", "Precision", "Recall", "F1", "AUC"), 3)
  })

  output$importance_plot <- renderPlotly({
    req(feat_imp)
    ggplotly(ggplot(feat_imp, aes(reorder(feature, MeanDecreaseGini), MeanDecreaseGini)) +
               geom_col(fill = "steelblue") + coord_flip() +
               labs(x = NULL, y = "Mean decrease in Gini") + theme_minimal())
  })

  # Data table + download
  output$earthquake_table <- renderDT({
    filtered_data() %>%
      select(datetime, latitude, longitude, magnitude, depth, risk_score,
             risk_level, region, cluster) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(c("latitude", "longitude", "depth", "risk_score"), 2)
  })

  output$download <- downloadHandler(
    filename = function() "filtered_earthquakes.csv",
    content  = function(file) readr::write_csv(filtered_data(), file)
  )
}

shinyApp(ui = ui, server = server)
