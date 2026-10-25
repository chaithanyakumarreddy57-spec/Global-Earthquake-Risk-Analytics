# ================================================================
# PHASE 11 - SHINY DASHBOARD
# Global Earthquake Risk Analytics using R
# Run earthquake_analysis.R first, then launch this app from the
# project folder (the one that contains data/processed/).
# ================================================================

pkgs <- c("shiny", "tidyverse", "leaflet", "plotly", "DT", "scales")
new <- pkgs[!(pkgs %in% rownames(installed.packages()))]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ------------------------------------------------
# LOAD PROCESSED DATA
# ------------------------------------------------

data_path <- c("data/processed/earthquakes_clean.csv",
               "../data/processed/earthquakes_clean.csv")
data_path <- data_path[file.exists(data_path)][1]

if (is.na(data_path)) {
  stop("Clean dataset not found. Run earthquake_analysis.R first ",
       "and start the app from the project folder.")
}

eq <- read_csv(data_path, show_col_types = FALSE)

needed <- c("datetime", "latitude", "longitude", "magnitude", "depth",
            "year", "risk_score", "risk_level", "cluster")
absent <- setdiff(needed, names(eq))
if (length(absent)) {
  stop("Missing columns in the clean dataset: ", paste(absent, collapse = ", "),
       ". Re-run earthquake_analysis.R completely (Phase 12 saves the final file).")
}

# CSV loses factor information: restore it so plots keep a sensible order
risk_levels  <- c("Low", "Moderate", "High", "Severe")
risk_colours <- c(Low = "#4CAF50", Moderate = "#FFC107", High = "#FF7043", Severe = "#B71C1C")

eq <- eq %>%
  mutate(
    risk_level = factor(risk_level, levels = risk_levels),
    cluster    = factor(cluster)
  )

# Slider bounds (computed once)
mag_min   <- floor(min(eq$magnitude, na.rm = TRUE))
mag_max   <- ceiling(max(eq$magnitude, na.rm = TRUE))
depth_min <- floor(min(eq$depth, na.rm = TRUE))
depth_max <- ceiling(max(eq$depth, na.rm = TRUE))

# ------------------------------------------------
# UI
# ------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Global Earthquake Risk Analytics Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      sliderInput("magnitude", "Magnitude Range",
                  min = mag_min, max = mag_max,
                  value = c(mag_min, mag_max), step = 0.1),
      
      sliderInput("depth", "Depth Range (km)",
                  min = depth_min, max = depth_max,
                  value = c(depth_min, depth_max)),
      
      selectInput("risk", "Risk Level",
                  choices = c("All", risk_levels), selected = "All"),
      
      selectInput("year", "Year",
                  choices = c("All", sort(unique(eq$year))), selected = "All"),
      
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      
      # KPI CARDS
      fluidRow(
        column(3, wellPanel(h4("Total Earthquakes"), h3(textOutput("total_quakes")))),
        column(3, wellPanel(h4("Average Magnitude"), h3(textOutput("avg_magnitude")))),
        column(3, wellPanel(h4("Maximum Magnitude"), h3(textOutput("max_magnitude")))),
        column(3, wellPanel(h4("High / Severe"),      h3(textOutput("high_risk"))))
      ),
      
      tabsetPanel(
        
        tabPanel("Overview", br(),
                 plotlyOutput("magnitude_plot", height = "450px"), br(),
                 plotlyOutput("year_plot", height = "450px")),
        
        tabPanel("Risk Analysis", br(),
                 plotlyOutput("risk_plot", height = "450px"), br(),
                 plotlyOutput("risk_year_plot", height = "450px")),
        
        tabPanel("Earthquake Map", br(),
                 leafletOutput("earthquake_map", height = "700px")),
        
        tabPanel("Depth Analysis", br(),
                 plotlyOutput("depth_plot", height = "450px"), br(),
                 plotlyOutput("depth_magnitude_plot", height = "450px")),
        
        tabPanel("Data", br(),
                 DTOutput("earthquake_table"))
      )
    )
  )
)

# ================================================================
# SERVER
# ================================================================

server <- function(input, output, session) {
  
  no_data_msg <- "No earthquakes match the current filters."
  
  # ------------------------------------------------
  # FILTERED DATA
  # ------------------------------------------------
  
  filtered_data <- reactive({
    
    data <- eq %>%
      filter(
        magnitude >= input$magnitude[1],
        magnitude <= input$magnitude[2],
        depth >= input$depth[1],
        depth <= input$depth[2]
      )
    
    if (input$risk != "All") {
      data <- data %>% filter(risk_level == input$risk)
    }
    
    if (input$year != "All") {
      data <- data %>% filter(year == as.numeric(input$year))
    }
    
    data
  })
  
  # ------------------------------------------------
  # RESET BUTTON
  # ------------------------------------------------
  
  observeEvent(input$reset, {
    updateSliderInput(session, "magnitude", value = c(mag_min, mag_max))
    updateSliderInput(session, "depth",     value = c(depth_min, depth_max))
    updateSelectInput(session, "risk", selected = "All")
    updateSelectInput(session, "year", selected = "All")
  })
  
  # ------------------------------------------------
  # KPIs (safe when the filter returns no rows)
  # ------------------------------------------------
  
  output$total_quakes <- renderText({
    scales::comma(nrow(filtered_data()))
  })
  
  output$avg_magnitude <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("-")
    as.character(round(mean(d$magnitude, na.rm = TRUE), 2))
  })
  
  output$max_magnitude <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("-")
    as.character(round(max(d$magnitude, na.rm = TRUE), 2))
  })
  
  output$high_risk <- renderText({
    d <- filtered_data()
    scales::comma(sum(d$risk_level %in% c("High", "Severe"), na.rm = TRUE))
  })
  
  # ================================================================
  # MAGNITUDE PLOT
  # ================================================================
  
  output$magnitude_plot <- renderPlotly({
    
    d <- filtered_data()
    validate(need(nrow(d) > 0, no_data_msg))
    
    p <- ggplot(d, aes(x = magnitude)) +
      geom_histogram(binwidth = 0.1, fill = "firebrick", colour = "white") +
      labs(title = "Earthquake Magnitude Distribution",
           x = "Magnitude", y = "Number of Earthquakes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ================================================================
  # YEAR PLOT
  # ================================================================
  
  output$year_plot <- renderPlotly({
    
    d <- filtered_data() %>% count(year)
    validate(need(nrow(d) > 0, no_data_msg))
    
    p <- ggplot(d, aes(x = year, y = n)) +
      geom_line(colour = "steelblue", linewidth = 1) +
      geom_point(colour = "steelblue") +
      labs(title = "Earthquakes per Year",
           x = "Year", y = "Number of Earthquakes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ================================================================
  # RISK PLOT
  # ================================================================
  
  output$risk_plot <- renderPlotly({
    
    d <- filtered_data() %>% count(risk_level)
    validate(need(nrow(d) > 0, no_data_msg))
    
    p <- ggplot(d, aes(x = risk_level, y = n, fill = risk_level)) +
      geom_col() +
      scale_fill_manual(values = risk_colours) +
      labs(title = "Earthquake Risk Distribution",
           x = "Risk Level", y = "Number of Earthquakes") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # ================================================================
  # RISK BY YEAR
  # ================================================================
  
  output$risk_year_plot <- renderPlotly({
    
    d <- filtered_data() %>%
      group_by(year) %>%
      summarise(mean_risk = mean(risk_score, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, no_data_msg))
    
    p <- ggplot(d, aes(x = year, y = mean_risk)) +
      geom_line(colour = "firebrick", linewidth = 1) +
      geom_point(colour = "firebrick") +
      labs(title = "Average Risk Score Over Time",
           x = "Year", y = "Average Risk Score") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ================================================================
  # EARTHQUAKE MAP
  # ================================================================
  
  output$earthquake_map <- renderLeaflet({
    
    data <- filtered_data()
    validate(need(nrow(data) > 0, no_data_msg))
    
    if (nrow(data) > 5000) {
      data <- slice_sample(data, n = 5000)
    }
    
    # Palette fixed to the full dataset so colours do not change with filters
    pal <- colorNumeric(palette = "YlOrRd", domain = eq$magnitude)
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~pmax(3, (magnitude - 4) * 1.5),
        color = ~pal(magnitude),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste0(
          "<b>Magnitude:</b> ", magnitude, "<br>",
          "<b>Depth:</b> ", round(depth, 2), " km<br>",
          "<b>Risk Score:</b> ", round(risk_score, 2), "<br>",
          "<b>Risk Level:</b> ", risk_level, "<br>",
          "<b>Year:</b> ", year
        )
      ) %>%
      addLegend(position = "bottomright", pal = pal,
                values = eq$magnitude, title = "Magnitude")
  })
  
  # ================================================================
  # DEPTH DISTRIBUTION
  # ================================================================
  
  output$depth_plot <- renderPlotly({
    
    d <- filtered_data()
    validate(need(nrow(d) > 0, no_data_msg))
    
    p <- ggplot(d, aes(x = depth)) +
      geom_histogram(bins = 50, fill = "steelblue", colour = "white") +
      labs(title = "Earthquake Depth Distribution",
           x = "Depth (km)", y = "Number of Earthquakes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ================================================================
  # DEPTH VS MAGNITUDE
  # ================================================================
  
  output$depth_magnitude_plot <- renderPlotly({
    
    d <- filtered_data()
    validate(need(nrow(d) > 2, "Not enough earthquakes match the current filters."))
    
    # Points are sampled (plotly gets very slow with ~20k points);
    # the trend line is still fitted on all filtered data.
    pts <- if (nrow(d) > 5000) slice_sample(d, n = 5000) else d
    
    p <- ggplot(d, aes(x = depth, y = magnitude)) +
      geom_point(data = pts, alpha = 0.4, colour = "purple") +
      geom_smooth(method = "lm", formula = y ~ x, colour = "black") +
      labs(title = "Magnitude vs Depth",
           x = "Depth (km)", y = "Magnitude") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ================================================================
  # DATA TABLE
  # ================================================================
  
  output$earthquake_table <- renderDT({
    
    filtered_data() %>%
      select(datetime, latitude, longitude, magnitude, depth,
             risk_score, risk_level, cluster) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(c("latitude", "longitude", "depth", "risk_score"), 2)
  })
}

# ================================================================
# RUN APPLICATION
# ================================================================

shinyApp(ui = ui, server = server)