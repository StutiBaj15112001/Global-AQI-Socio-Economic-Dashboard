# app.R

#  0. Libraries 
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(ggplot2)
library(DT)
library(tidyr)
library(htmltools)   # for HTML() in labels
library(shinyjs)     # for JavaScript functionality

#  1. Load & Prepare Data 

# 1a. City-level AQI + country socio-econ
city_data  <- read.csv("C:/Users/Stuti/Downloads/StutiBaj_31981259_Code/wrangled_city_aqi.csv",
                       stringsAsFactors = FALSE)
world_data <- read.csv("C:/Users/Stuti/Downloads/StutiBaj_31981259_Code/cleaned_worlddata.csv",
                       stringsAsFactors = FALSE)
names(world_data) <- trimws(names(world_data))

merged_data <- city_data %>%
  left_join(world_data, by = "country")

# 1b. UK emissions tidy
uk_emissions <- read.csv("C:/Users/Stuti/Downloads/StutiBaj_31981259_Code/emissions1_tidy.csv",
                         stringsAsFactors = FALSE) %>%
  filter(!is.na(year)) %>%
  mutate(
    Industry       = as.factor(industry),
    Gas            = as.factor(pollutant),
    Emission_Value = as.numeric(emission_value),
    year           = as.numeric(year)
  ) %>%
  filter(!is.na(Emission_Value))

year_range <- range(uk_emissions$year, na.rm = TRUE)

#  2. UI Inputs & Palettes

socio_cols <- c(
  "electricity_access", "gdp_capita", "labor_rate", "life_expectancy",
  "adult_literacy", "water_access", "air_pollution", "population_density",
  "alcohol_consumption", "unemployment_rate", "social_support",
  "freedom", "generosity", "income_class"
)

aqi_colors <- c(
  "Good"                            = "#2ECC40",
  "Moderate"                        = "#FFDC00",
  "Unhealthy"                       = "#FF851B",
  "Unhealthy for Sensitive Groups"  = "#FFB347",
  "Very Unhealthy"                  = "#FF4136",
  "Hazardous"                       = "#85144b"
)

#  3. UI Definition 

ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  tags$head(
    tags$style(HTML("
      /* Light mode styles */
      :root {
        --bg-color: #ffffff;
        --text-color: #2c3e50;
        --box-bg: #ffffff;
        --box-border: #ddd;
        --accent-color: #007bff;
        --accent-secondary: #28a745;
        --desc-color: #666;
        --footer-bg: #f8f9fa;
      }
      
      /* Dark mode styles */
      [data-bs-theme='dark'] {
        --bg-color: #1a1a1a;
        --text-color: #e1e1e1;
        --box-bg: #2d2d2d;
        --box-border: #404040;
        --accent-color: #0d6efd;
        --accent-secondary: #198754;
        --desc-color: #a0a0a0;
        --footer-bg: #2d2d2d;
      }
      
      body {
        background-color: var(--bg-color);
        color: var(--text-color);
      }
      
      .leaflet-container { 
        background-color: var(--bg-color); 
      }
      
      .viz-box { 
        border: 1px solid var(--box-border);
        padding: 15px; 
        margin-bottom: 20px;
        border-radius: 4px; 
        background: var(--box-bg);
        box-shadow: 0 1px 3px rgba(0,0,0,0.12); 
      }
      
      .intro-box {
        background: var(--box-bg);
        padding: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
        border-left: 4px solid var(--accent-color);
      }
      
      .graph-desc {
        font-size: 14px;
        color: var(--desc-color);
        margin-top: 10px;
        font-style: italic;
      }
      
      .footer {
        background: var(--footer-bg);
        padding: 15px;
        text-align: center;
        margin-top: 30px;
        border-top: 1px solid var(--box-border);
      }
      
      .tab-description {
        margin: 20px 0;
        padding: 15px;
        background: var(--box-bg);
        border-left: 4px solid var(--accent-secondary);
        border-radius: 0 5px 5px 0;
      }
      
      /* Theme toggle switch */
      .theme-container {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        z-index: 1000;
        background: var(--bg-color);
        border-bottom: 1px solid var(--box-border);
        padding: 10px;
        text-align: right;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .theme-switch {
        display: inline-block;
        margin-right: 20px;
      }
      
      .theme-switch label {
        cursor: pointer;
        padding: 8px 12px;
        border-radius: 4px;
        background: var(--box-bg);
        border: 1px solid var(--box-border);
        transition: all 0.3s ease;
      }
      
      .theme-switch label:hover {
        background: var(--accent-color);
        color: white;
      }
      
      /* Adjust main content to account for fixed header */
      .main-content {
        margin-top: 60px;
      }
    "))
  ),
  
  # Theme toggle container
  div(class = "theme-container",
      div(class = "theme-switch",
          checkboxInput("darkMode", "Dark Mode", FALSE)
      )
  ),
  
  # Wrap main content
  div(class = "main-content",
      # Dashboard Title and Introduction
      div(class = "intro-box",
          h2("Air Quality Analysis Dashboard", style = "color: var(--text-color);"),
          # Detailed Introduction
          tags$div(
            style = "padding: 20px; background-color: #2c3e50; color: #ecf0f1; border-radius: 5px; margin-bottom: 20px;",
            tags$h3("Introduction"),
            tags$p(
              "Air pollution is one of the greatest environmental risks to human health and ecosystems worldwide. ",
              "Fine particulate matter (PM2.5/PM10), nitrogen oxides, ozone and carbon monoxide are all linked ",
              "to respiratory diseases, reduced life expectancy, and economic costs. As urbanization and industrial ",
              "activity continue to increase particularly in rapidly developing regions it becomes critical to ",
              "track how air quality varies across countries and sectors, and to understand the socio-economic ",
              "and policy levers that can improve it."
            ),
            tags$p(
              "This interactive dashboard is divided into two main analyses:",  
              tags$ul(
                tags$li(
                  strong("Global Air Quality Analysis:"), " Explore city-level AQI (overall or by pollutant) on a world map, ",
                  "see the distribution of air quality categories, and examine how AQI correlates with key socio-economic ",
                  "factors such as GDP per capita, population density, and electronic access."
                ),
                tags$li(
                  strong("UK Emissions Analysis:"), " Dive into industrial emissions in the United Kingdom. ",
                  "Compare sectors via packed bubble plots that scale to total emissions, and follow time-series trends ",
                  "to see where policy and technology have driven emissions down (or up)."
                )
              )
            ),
            tags$p(
              "My motivation for this project stems from growing up in India where air pollution poses a severe public ",
              "health challenge in many cities and seeing firsthand how socio-economic development both contributes ",
              "to and can help solve these problems. By combining open data on pollutant concentrations, economic ",
              "indicators, and industry emissions, this dashboard aims to provide clear, actionable insights for researchers, ",
              "policymakers, and the public alike."
            )
          )
      ),
      
      navbarPage(
        title = NULL,
        id = "navbar",
        
        # Tab 1: Global AQI  
        tabPanel("Global Air Quality Analysis",
                 div(class = "tab-description",
                     # Add Global AQI Description
                     tags$div(
                       style="padding:8px; background:#2c3e50; color:#ecf0f1; border-radius:4px; margin-bottom:10px;",
                       tags$h4("Global AQI & Socio-Economic Insights"),
                       tags$p(
                         "Use the world map to view city-level AQI categories.  
                  The AQI distribution histogram shows how often each category occurs,  
                  and the correlation plot reveals the relationship between AQI values and your chosen socio-economic indicator."
                       )
                     )
                 ),
                 sidebarLayout(
                   sidebarPanel(
                     width = 3, style = "margin-top:20px;",
                     fluidRow(
                       column(9,
                              selectInput("countries", "Select Countries (max 5):",
                                          choices = sort(unique(merged_data$country)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = NULL)
                       ),
                       column(3,
                              actionButton("reset_countries", "Reset", style = "margin-top:25px;")
                       )
                     ),
                     selectInput("socio_factor", "Select Socio-economic Factor:",
                                 choices = socio_cols, selected = "gdp_capita"),
                     selectInput("gas", "Select Gas:",
                                 choices = c("All","PM2.5","NO2","CO","Ozone"),
                                 selected = "All"),
                     selectInput("aqi_cat", "Select AQI Category:",
                                 choices = c("All", names(aqi_colors)),
                                 selected = "All"),
                     tags$div(style="padding:10px;",
                              tags$h4("How to use:"),
                              tags$ul(
                                tags$li("Select up to 5 countries (leave empty for all)"),
                                tags$li("Choose a socio-economic factor"),
                                tags$li("Filter by gas or view overall AQI"),
                                tags$li("Filter by AQI category"),
                                tags$li("Hover over map points for details")
                              )
                     )
                   ),
                   mainPanel(
                     width = 9, style = "margin-top:20px;",
                     div(class="viz-box", 
                         h4("Global Air Quality Distribution Map"),
                         p(class="graph-desc", "Interactive map showing air quality categories across different cities. Size indicates pollution level."),
                         leafletOutput("map", height="600px")
                     ),
                     fluidRow(
                       column(6,
                              div(class="viz-box", 
                                  h4("AQI Distribution"),
                                  p(class="graph-desc", "Distribution of air quality categories showing the frequency of each AQI level."),
                                  plotlyOutput("aqi_dist", height="300px"))
                       ),
                       column(6,
                              div(class="viz-box", 
                                  h4("Socio-Economic Correlation"),
                                  p(class="graph-desc", "Scatter plot showing relationship between AQI and selected socio-economic factor."),
                                  plotlyOutput("socio_correlation", height="300px"))
                       )
                     )
                   )
                 )
        ),
        
        # Tab 2: UK Emissions 
        tabPanel("UK Emissions Analysis",
                 div(class = "tab-description",
                     # Add UK Emissions Description
                     tags$div(
                       style = "padding:8px; background:#2c3e50; color:#ecf0f1; border-radius:4px; margin-bottom:10px;",
                       tags$h4("UK Industrial Emissions Overview"),
                       tags$p(
                         "Analyze UK industrial pollutant volumes with two linked views:",
                         tags$ul(
                           tags$li(strong("Packed Bubble Chart:"), " Circle area shows total emissions by industry."),
                           tags$li(strong("Trend Line Plot:"), " Emissions over time for selected industries and pollutants.")
                         )
                       )
                     )
                 ),
                 sidebarLayout(
                   sidebarPanel(
                     width = 3, style = "margin-top:20px;",
                     fluidRow(
                       column(9,
                              selectInput("industries", "Select Industries (max 5):",
                                          choices = sort(unique(uk_emissions$Industry)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = NULL)
                       ),
                       column(3,
                              actionButton("reset_industries", "Reset", style = "margin-top:25px;")
                       )
                     ),
                     selectInput("pollutants","Select Pollutant:",
                                 choices = c("All", levels(uk_emissions$Gas)),
                                 selected = "All"),
                     sliderInput("year_range","Select Year Range:",
                                 min = year_range[1], max = year_range[2],
                                 value = year_range, step = 1, sep = ""),
                     tags$div(style="padding:10px;",
                              tags$h4("How to use:"),
                              tags$ul(
                                tags$li("Select industries and pollutants"),
                                tags$li("Use the slider for time period"),
                                tags$li("Bubble size = total emissions"),
                                tags$li("Colors = different pollutants")
                              )
                     )
                   ),
                   mainPanel(
                     width = 9, style = "margin-top:20px;",
                     div(class="viz-box", 
                         h4("Industry-wise Emissions Distribution"),
                         p(class="graph-desc", "Bubble plot showing emission values distribution across industries. Bubble size represents emission magnitude."),
                         plotlyOutput("emissions_bubble", height="400px")
                     ),
                     div(class="viz-box", 
                         h4("Emissions Trend Over Time"),
                         p(class="graph-desc", "Line plot showing temporal trends of emissions for selected industries."),
                         plotlyOutput("emissions_trend", height="400px")
                     )
                   )
                 )
        )
      ),
      
      # Footer with student details
      div(class = "footer",
          hr(),
          h4("Data Exploration and Visualization Project"),
          p("Created by: Stuti Baj"),
          p("Student ID: 31981259"),
          p(paste("Last Updated:", format(Sys.Date(), "%B %Y")))
      )
  )
)

#  4. Server Logic 

server <- function(input, output, session) {
  
  # Dark mode handler
  observeEvent(input$darkMode, {
    if (input$darkMode) {
      runjs("document.documentElement.setAttribute('data-bs-theme', 'dark');")
    } else {
      runjs("document.documentElement.setAttribute('data-bs-theme', 'light');")
    }
  })
  
  #  Limit country selection to 5 
  observe({
    if (length(input$countries) > 5) {
      updateSelectInput(session, "countries",
                        selected = tail(input$countries, 5))
    }
  })
  observeEvent(input$reset_countries, {
    updateSelectInput(session, "countries", selected = character(0))
  })
  
  #  Limit industry selection to 5 
  observe({
    if (length(input$industries) > 5) {
      updateSelectInput(session, "industries",
                        selected = tail(input$industries, 5))
    }
  })
  observeEvent(input$reset_industries, {
    updateSelectInput(session, "industries", selected = character(0))
  })
  
  # Tab 1: reactive filter & outputs 
  filtered_data <- reactive({
    req(input$socio_factor, input$gas, input$aqi_cat)
    df <- merged_data
    if (length(input$countries) > 0) {
      df <- df %>% filter(country %in% input$countries)
    }
    if (input$gas != "All") {
      df <- df %>%
        filter(gas == input$gas) %>%
        mutate(
          display_aqi_category = aqi_category,
          display_aqi_value    = aqi_value
        )
    } else {
      df <- df %>%
        mutate(
          display_aqi_category = overall_aqi_category,
          display_aqi_value    = overall_aqi_value
        )
    }
    if (input$aqi_cat != "All") {
      df <- df %>% filter(display_aqi_category == input$aqi_cat)
    }
    df
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    pal  <- colorFactor(palette = aqi_colors,
                        levels  = names(aqi_colors))
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat,
        color       = ~pal(display_aqi_category),
        radius      = 4,
        fillOpacity = 0.7,
        stroke      = FALSE,
        label       = ~lapply(paste0(
          "<b>", city, ", ", country, "</b><br/>",
          "AQI Category: ", display_aqi_category, "<br/>",
          "AQI Value: ", round(display_aqi_value,2), "<br/>",
          input$socio_factor, ": ",
          get(input$socio_factor)
        ), HTML)
      ) %>%
      addLegend(
        position = "bottomright",    # named explicitly
        pal      = pal,
        values   = unique(data$display_aqi_category),
        title    = "AQI Categories",
        opacity  = 1
      )
  })
  
  output$aqi_dist <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x=~display_aqi_category,
            type="histogram",
            color=~display_aqi_category,
            colors=aqi_colors) %>%
      layout(title="Distribution of AQI Categories",
             xaxis=list(title="AQI Category"),
             yaxis=list(title="Count"), showlegend=FALSE)
  })
  
  output$socio_correlation <- renderPlotly({
    data <- filtered_data()
    plot_ly(data,
            x = as.formula(paste0("~`", input$socio_factor, "`")),
            y = ~display_aqi_value,
            type="scatter", mode="markers",
            color=~display_aqi_category,
            colors=aqi_colors,
            marker=list(size=10)) %>%
      layout(title=paste("AQI vs", input$socio_factor),
             xaxis=list(title=input$socio_factor),
             yaxis=list(title="AQI Value"))
  })
  
  #  Tab 2: Emissions Bubble 
  output$emissions_bubble <- renderPlotly({
    # First prepare the data based on selections
    base_data <- uk_emissions %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
    
    if(input$pollutants != "All") {
      base_data <- base_data %>% 
        filter(Gas == input$pollutants) %>%
        # Add log transformation for better scaling when dealing with benzene
        mutate(Emission_Value = if(input$pollutants == "Benzene") {
          log10(Emission_Value + 1)  # log transform benzene values
        } else {
          Emission_Value
        })
    }
    
    # Add validation at the start
    validate(need(length(input$industries) > 0, "Please select at least one industry"))
    
    # Calculate the data for plotting
    data <- if(length(input$industries) > 0) {
      # If industries selected, first find max emission per industry per year
      base_data %>%
        filter(Industry %in% input$industries) %>%
        group_by(year, Industry) %>%
        # For each industry-year combo, find the pollutant with max emission
        slice_max(order_by = Emission_Value, n = 1, with_ties = FALSE) %>%
        # Then for each year, take the industry with the highest emission
        group_by(year) %>%
        slice_max(order_by = Emission_Value, n = 1, with_ties = FALSE) %>%
        ungroup()
    } else {
      # If no industries selected, show highest emission for each year
      base_data %>%
        group_by(year) %>%
        slice_max(order_by = Emission_Value, n = 1, with_ties = FALSE) %>%
        ungroup()
    }
    
    validate(need(nrow(data) > 0, "No data available"))
    
    # Create more spread out jittered positions
    data <- data %>%
      group_by(year) %>%
      mutate(
        x_jitter = year + runif(n(), -0.2, 0.2),  # Reduced x jitter
        y_jitter = runif(n(), -0.4, 0.4),         # Increased y jitter
        y_base = as.numeric(as.factor(Industry)) * 2  # More vertical spacing
      ) %>%
      ungroup()
    
    # Use a fixed set of colors instead of dynamic generation
    industry_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
                        "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
    
    # Apply stronger scaling for better visualization
    max_emission <- max(data$Emission_Value)
    size_scaling <- if(input$pollutants == "Benzene") {
      # Very aggressive scaling for Benzene
      list(
        sizeref = max_emission / (20^2),  # Much smaller reference
        sizemin = 2,
        sizemax = 15     # Significantly smaller maximum size
      )
    } else {
      # Modified scaling for other pollutants
      list(
        sizeref = max_emission / (30^2),
        sizemin = 3,
        sizemax = 25
      )
    }
    
    plot_ly(data, 
            x = ~x_jitter,
            y = ~y_base + y_jitter,
            size = ~Emission_Value,
            color = ~Industry,
            colors = industry_colors,
            type = "scatter",
            mode = "markers",              
            marker = list(
                sizemode = "area",
                sizeref = size_scaling$sizeref,
                sizemin = size_scaling$sizemin,
                sizemax = size_scaling$sizemax,
                opacity = 0.8,  # Increased opacity
                line = list(color = "white", width = 0.5)  # Thinner border
              ),
            hoverinfo = "text",              text = ~paste(
              "Year:", year,
              "<br>Industry:", Industry,
              "<br>Highest Emission:", if(input$pollutants == "Benzene") {
                paste(format(10^Emission_Value - 1, scientific = TRUE), "(original value)")
              } else {
                round(Emission_Value, 2)
              },
              "<br>From Pollutant:", Gas,
              if(length(input$industries) > 1) 
                paste("<br>(Highest among selected industries)") 
              else ""
            )
    ) %>%
      layout(
        title = list(
          text = "Emission Values Distribution",
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Year",
          tickmode = "linear",
          dtick = 1,
          range = c(min(data$year)-0.5, max(data$year)+0.5)
        ),
        yaxis = list(
          title = "Industries",
          showticklabels = FALSE,  # Hide y-axis values for cleaner look
          zeroline = FALSE
        ),
        showlegend = TRUE,
        hovermode = "closest"
      )
  })
  
  output$emissions_trend <- renderPlotly({
    validate(need(length(input$industries)>0,
                  "Please select at least one industry"))
    data <- uk_emissions %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             Industry %in% input$industries)
    if (input$pollutants != "All") {
      data <- data %>% filter(Gas == input$pollutants)
    }
    data <- data %>%
      group_by(year, Industry) %>%
      summarise(Emission_Value = sum(Emission_Value, na.rm=TRUE),
                .groups="drop")
    validate(need(nrow(data)>0, "No data available"))
    plot_ly(data,
            x=~year, y=~Emission_Value,
            color=~Industry,
            type="scatter", mode="lines+markers",
            line=list(width=2), marker=list(size=8),
            hoverinfo="text",
            text=~paste(
              "Year:", year,
              "<br>Industry:", Industry,
              "<br>Emissions:", round(Emission_Value,2)
            )
    ) %>%
      layout(
        title="Total Emissions Trend Over Time",
        xaxis=list(title="Year"), yaxis=list(title="Total Emissions")
      )
  })
  
}  # end server

#  5. Launch App 
shinyApp(ui, server)