# =============================================================================
#  Air Quality Analysis Dashboard — Final Version
#  Author : Stuti Baj  |  Student ID: 31981259
#  New in this version:
#    Tab 1  - Choropleth toggle, Top-10 cities table, Pearson r on scatter,
#             Health Risk explainer panel, Download filtered data button
#    Tab 2  - YoY % change KPI, Stacked area chart, Forecast trend line
#    Tab 3  - Insights tab with plain-English analytical findings
# =============================================================================

# 0. Libraries ----------------------------------------------------------------
library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(DT)
library(htmltools)

# 1. Data Loading -------------------------------------------------------------
data_dir <- "C:/Users/Stuti/Downloads/StutiBaj_31981259_Code"

city_data  <- read.csv(file.path(data_dir, "wrangled_city_aqi.csv"),
                       stringsAsFactors = FALSE)
world_data <- read.csv(file.path(data_dir, "cleaned_worlddata.csv"),
                       stringsAsFactors = FALSE)
names(world_data) <- trimws(names(world_data))

merged_data <- city_data %>%
  left_join(world_data, by = "country")

uk_emissions <- read.csv(file.path(data_dir, "emissions1_tidy.csv"),
                         stringsAsFactors = FALSE) %>%
  filter(!is.na(year)) %>%
  mutate(
    Industry       = as.factor(industry),
    Gas            = as.factor(pollutant),
    Emission_Value = as.numeric(emission_value),
    year           = as.integer(year)
  ) %>%
  filter(!is.na(Emission_Value))

year_range <- range(uk_emissions$year, na.rm = TRUE)

# 2. Constants ----------------------------------------------------------------
SOCIO_COLS <- c(
  "electricity_access", "gdp_capita", "labor_rate", "life_expectancy",
  "adult_literacy", "water_access", "air_pollution", "population_density",
  "alcohol_consumption", "unemployment_rate", "social_support",
  "freedom", "generosity", "income_class"
)

AQI_COLORS <- c(
  "Good"                           = "#2ECC40",
  "Moderate"                       = "#FFDC00",
  "Unhealthy for Sensitive Groups" = "#FFB347",
  "Unhealthy"                      = "#FF851B",
  "Very Unhealthy"                 = "#FF4136",
  "Hazardous"                      = "#85144b"
)

INDUSTRY_COLORS <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#A65628", "#F781BF", "#999999"
)

# Health risk descriptions shown in the explainer panel
HEALTH_RISK <- list(
  "Good" = list(
    colour  = "#2ECC40",
    who     = "Everyone",
    risk    = "Air quality is considered satisfactory and poses little or no risk.",
    advice  = "Great day to be outdoors. No precautions needed for any group.",
    icon    = "Good"
  ),
  "Moderate" = list(
    colour  = "#e6c200",
    who     = "Unusually sensitive individuals",
    risk    = "Air quality is acceptable. A small number of people who are unusually sensitive to air pollution may experience minor symptoms.",
    advice  = "Most people can enjoy normal outdoor activities. If you notice symptoms such as coughing or shortness of breath, consider reducing prolonged exertion outdoors.",
    icon    = "Moderate"
  ),
  "Unhealthy for Sensitive Groups" = list(
    colour  = "#FF8C00",
    who     = "Children, elderly, people with heart or lung disease",
    risk    = "Members of sensitive groups may experience health effects. The general public is not likely to be affected.",
    advice  = "Sensitive groups should limit prolonged outdoor exertion. Keep reliever inhalers accessible if you have asthma.",
    icon    = "Caution"
  ),
  "Unhealthy" = list(
    colour  = "#FF4136",
    who     = "Everyone",
    risk    = "Everyone may begin to experience health effects. Sensitive groups may experience more serious effects.",
    advice  = "Everyone should reduce prolonged or heavy outdoor exertion. Sensitive groups should avoid outdoor activity where possible.",
    icon    = "Warning"
  ),
  "Very Unhealthy" = list(
    colour  = "#c0392b",
    who     = "Everyone",
    risk    = "Health alert: everyone may experience more serious health effects including respiratory and cardiovascular issues.",
    advice  = "Everyone should avoid prolonged outdoor exertion. Sensitive groups should remain indoors and keep activity levels low.",
    icon    = "Alert"
  ),
  "Hazardous" = list(
    colour  = "#85144b",
    who     = "Everyone",
    risk    = "Health warnings of emergency conditions. The entire population is likely to be affected with serious health consequences.",
    advice  = "Everyone should avoid all outdoor activity. Keep windows and doors closed. Use air purifiers indoors if available. Seek medical advice if experiencing symptoms.",
    icon    = "Emergency"
  )
)

# Pre-compute global insights used in Tab 3 -----------------------------------

# Insight 1: high-GDP vs low-GDP AQI difference
gdp_split <- merged_data %>%
  filter(!is.na(gdp_capita), !is.na(overall_aqi_value)) %>%
  mutate(gdp_group = ifelse(gdp_capita >= 20000, "High (above $20k)", "Low (below $20k)")) %>%
  group_by(gdp_group) %>%
  summarise(avg_aqi = round(mean(overall_aqi_value, na.rm = TRUE), 1), .groups = "drop")

gdp_high_aqi <- gdp_split$avg_aqi[gdp_split$gdp_group == "High (above $20k)"]
gdp_low_aqi  <- gdp_split$avg_aqi[gdp_split$gdp_group == "Low (below $20k)"]
gdp_ratio    <- round(gdp_low_aqi / max(gdp_high_aqi, 1), 1)

# Insight 2: most & least polluted countries (avg AQI)
country_aqi <- merged_data %>%
  group_by(country) %>%
  summarise(avg_aqi = round(mean(overall_aqi_value, na.rm = TRUE), 1),
            n_cities = n(), .groups = "drop") %>%
  filter(n_cities >= 3) %>%
  arrange(desc(avg_aqi))

most_polluted  <- head(country_aqi, 5)
least_polluted <- tail(country_aqi, 5)

# Insight 3: UK emissions overall trend
uk_total_by_year <- uk_emissions %>%
  group_by(year) %>%
  summarise(total = sum(Emission_Value, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)

uk_first_yr  <- uk_total_by_year$year[1]
uk_last_yr   <- uk_total_by_year$year[nrow(uk_total_by_year)]
uk_first_val <- round(uk_total_by_year$total[1], 0)
uk_last_val  <- round(uk_total_by_year$total[nrow(uk_total_by_year)], 0)
uk_pct_change <- round((uk_last_val - uk_first_val) / uk_first_val * 100, 1)

# Insight 4: industry with biggest absolute reduction
industry_change <- uk_emissions %>%
  group_by(Industry, year) %>%
  summarise(total = sum(Emission_Value, na.rm = TRUE), .groups = "drop") %>%
  group_by(Industry) %>%
  summarise(
    first_val = total[which.min(year)],
    last_val  = total[which.max(year)],
    change    = total[which.max(year)] - total[which.min(year)],
    .groups   = "drop"
  ) %>%
  arrange(change)   # most negative = biggest reduction

top_reducer      <- as.character(industry_change$Industry[1])
top_reducer_drop <- abs(round(industry_change$change[1], 0))

# Insight 5: life expectancy correlation direction
le_cor <- cor(merged_data$life_expectancy, merged_data$overall_aqi_value,
              use = "complete.obs")
le_direction <- ifelse(le_cor < 0, "lower", "higher")

# 3. UI -----------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      :root {
        --bg: #ffffff; --text: #2c3e50; --box-bg: #f8f9fa;
        --border: #dee2e6; --accent: #007bff; --accent2: #28a745;
        --muted: #6c757d; --footer-bg: #f1f3f5;
      }
      [data-bs-theme='dark'] {
        --bg: #121212; --text: #e8e8e8; --box-bg: #1e1e1e;
        --border: #333; --accent: #4dabf7; --accent2: #51cf66;
        --muted: #868e96; --footer-bg: #1a1a1a;
      }

      body    { background: var(--bg); color: var(--text); transition: background .3s, color .3s; }
      .navbar { background: var(--box-bg) !important; border-bottom: 1px solid var(--border); }
      .nav-tabs .nav-link { color: var(--text); }

      .viz-box {
        border: 1px solid var(--border); padding: 16px; margin-bottom: 20px;
        border-radius: 6px; background: var(--box-bg);
        box-shadow: 0 1px 4px rgba(0,0,0,.08);
      }
      .viz-box h4 { margin-top: 0; color: var(--text); }

      .intro-box {
        background: #2c3e50; color: #ecf0f1;
        padding: 24px; border-radius: 6px; margin-bottom: 20px;
      }
      .intro-box h2, .intro-box h3 { color: #ecf0f1; margin-top: 0; }

      .tab-desc {
        margin: 16px 0; padding: 14px 18px;
        background: var(--box-bg); border-left: 4px solid var(--accent2);
        border-radius: 0 6px 6px 0; color: var(--text);
      }

      .graph-desc { font-size: 13px; color: var(--muted); font-style: italic; margin: 6px 0 12px; }

      .kpi-row  { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 16px; }
      .kpi-card {
        flex: 1; min-width: 140px; padding: 14px 18px;
        background: var(--box-bg); border: 1px solid var(--border);
        border-radius: 6px; text-align: center;
      }
      .kpi-card .kpi-val { font-size: 26px; font-weight: 700; color: var(--accent); }
      .kpi-card .kpi-lbl { font-size: 12px; color: var(--muted); margin-top: 4px; }

      /* Health risk panel */
      .health-panel {
        border-radius: 6px; padding: 16px; margin-bottom: 16px;
        border: 2px solid; transition: all .4s ease;
      }
      .health-panel h4 { margin-top: 0; font-weight: 700; }
      .health-who  { font-size: 13px; font-weight: 600; margin-bottom: 6px; }
      .health-risk { font-size: 13px; margin-bottom: 8px; }
      .health-advice {
        font-size: 13px; background: rgba(255,255,255,0.15);
        padding: 8px 12px; border-radius: 4px; border-left: 3px solid rgba(255,255,255,0.6);
      }

      /* Insights tab */
      .insight-card {
        background: var(--box-bg); border: 1px solid var(--border);
        border-radius: 8px; padding: 20px; margin-bottom: 18px;
        border-left: 5px solid var(--accent);
      }
      .insight-card h4 { margin-top: 0; color: var(--accent); }
      .insight-card p  { color: var(--text); margin-bottom: 0; line-height: 1.6; }
      .insight-highlight {
        font-size: 22px; font-weight: 700; color: var(--accent2);
        display: block; margin: 8px 0;
      }
      .insight-section-title {
        font-size: 18px; font-weight: 700; color: var(--text);
        border-bottom: 2px solid var(--border); padding-bottom: 8px;
        margin: 28px 0 16px;
      }

      .theme-bar {
        position: fixed; top: 0; left: 0; right: 0; z-index: 1100;
        background: var(--box-bg); border-bottom: 1px solid var(--border);
        padding: 6px 16px; text-align: right;
        box-shadow: 0 2px 4px rgba(0,0,0,.08);
      }
      .main-content { margin-top: 52px; padding: 0 12px; }

      .footer {
        background: var(--footer-bg); padding: 16px;
        text-align: center; margin-top: 30px;
        border-top: 1px solid var(--border); color: var(--muted); font-size: 13px;
      }

      .well { background: var(--box-bg) !important; border-color: var(--border) !important; }
      .selectize-input, .selectize-dropdown {
        background: var(--box-bg) !important; color: var(--text) !important;
      }

      /* Download button */
      .btn-download {
        background: var(--accent2); color: white; border: none;
        padding: 7px 16px; border-radius: 4px; font-size: 13px; cursor: pointer;
        margin-top: 8px;
      }
      .btn-download:hover { opacity: 0.85; }

      /* Top-10 table */
      .dataTables_wrapper { color: var(--text); }
      table.dataTable { color: var(--text) !important; }
    "))
  ),

  div(class = "theme-bar",
      checkboxInput("darkMode", "Dark Mode", FALSE)
  ),

  div(class = "main-content",

    # Introduction ------------------------------------------------------------
    div(class = "intro-box",
        h2("Air Quality Analysis Dashboard"),
        h3("Introduction"),
        p("Air pollution is one of the greatest environmental risks to human health
          worldwide. Fine particulate matter (PM2.5/PM10), nitrogen oxides, ozone,
          and carbon monoxide are all linked to respiratory disease, reduced life
          expectancy, and significant economic costs. As urbanisation and industrial
          activity continue to grow — particularly in rapidly developing regions —
          tracking air quality and its socio-economic drivers is critical."),
        tags$ul(
          tags$li(strong("Global Air Quality Analysis:"),
                  " Explore city-level AQI on a world map, see category distributions,
                    examine correlations with socio-economic factors, and understand
                    what each AQI level means for your health."),
          tags$li(strong("UK Emissions Analysis:"),
                  " Compare UK industrial emission volumes via bubble plots, track
                    how different sectors have changed over time, and see a forecast
                    of where trends are heading."),
          tags$li(strong("Key Insights:"),
                  " Plain-English findings drawn from the data — no technical
                    background required.")
        ),
        p(em("Motivation: Growing up in India — where air pollution poses a severe
              public-health challenge — inspired this project. Combining open data
              on pollutant concentrations, economic indicators, and industry
              emissions aims to provide actionable insights for researchers and
              policymakers alike."))
    ),

    navbarPage(
      title = NULL,
      id    = "navbar",

      # ======================================================================
      # Tab 1: Global Air Quality
      # ======================================================================
      tabPanel("Global Air Quality",
        div(class = "tab-desc",
            strong("Tip:"), " Select up to 5 countries to zoom in. Choose a
            socio-economic factor to explore its relationship with air quality.
            Use the map type toggle to switch between city dots and a country
            choropleth. Download the filtered data using the button in the sidebar."
        ),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            fluidRow(
              column(9,
                selectInput("countries", "Countries (max 5):",
                            choices  = sort(unique(merged_data$country)),
                            multiple = TRUE, selectize = TRUE)
              ),
              column(3,
                actionButton("reset_countries", "Reset",
                             style = "margin-top:25px; padding:5px 10px;")
              )
            ),
            selectInput("socio_factor", "Socio-economic Factor:",
                        choices = SOCIO_COLS, selected = "gdp_capita"),
            selectInput("gas", "Gas / Pollutant:",
                        choices = c("All", "PM2.5", "NO2", "CO", "Ozone"),
                        selected = "All"),
            selectInput("aqi_cat", "AQI Category:",
                        choices = c("All", names(AQI_COLORS)),
                        selected = "All"),
            radioButtons("map_type", "Map Display:",
                         choices  = c("City dots" = "dots",
                                      "Country average" = "choropleth"),
                         selected = "dots"),
            hr(),
            downloadButton("download_data", "Download Filtered Data",
                           class = "btn-download"),
            hr(),
            helpText("Leave Countries empty to show all cities worldwide.")
          ),
          mainPanel(
            width = 9,
            uiOutput("kpi_row"),

            # Health Risk Explainer
            uiOutput("health_panel"),

            div(class = "viz-box",
                h4("Global Air Quality Distribution Map"),
                p(class = "graph-desc",
                  "City dots: each point = one city, coloured by AQI.
                   Country average: countries filled by their mean AQI value.
                   Hover for details."),
                leafletOutput("map", height = "520px")
            ),

            fluidRow(
              column(6,
                div(class = "viz-box",
                    h4("AQI Category Distribution"),
                    p(class = "graph-desc",
                      "How frequently each AQI category appears in the filtered data."),
                    plotlyOutput("aqi_dist", height = "280px"))
              ),
              column(6,
                div(class = "viz-box",
                    h4("Socio-Economic Correlation"),
                    p(class = "graph-desc",
                      "Scatter plot of AQI value vs. chosen factor.
                       The Pearson r value shows how strong the relationship is."),
                    plotlyOutput("socio_correlation", height = "280px"),
                    uiOutput("pearson_label"))
              )
            ),

            div(class = "viz-box",
                h4("Top 10 Most Polluted Cities (filtered selection)"),
                p(class = "graph-desc",
                  "Ranked by average AQI value. Use the filters on the left to
                   narrow by country or gas."),
                DTOutput("top10_table")
            )
          )
        )
      ),

      # ======================================================================
      # Tab 2: UK Emissions
      # ======================================================================
      tabPanel("UK Emissions",
        div(class = "tab-desc",
            strong("Tip:"), " Select industries and a pollutant, then use the year
            slider. Bubble size = emission magnitude. The stacked area chart shows
            each industry's share of total emissions. The trend chart includes a
            dashed forecast line showing where each industry is heading."
        ),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            fluidRow(
              column(9,
                selectInput("industries", "Industries (max 5):",
                            choices  = sort(levels(uk_emissions$Industry)),
                            multiple = TRUE, selectize = TRUE)
              ),
              column(3,
                actionButton("reset_industries", "Reset",
                             style = "margin-top:25px; padding:5px 10px;")
              )
            ),
            selectInput("pollutants", "Pollutant:",
                        choices  = c("All", levels(uk_emissions$Gas)),
                        selected = "All"),
            sliderInput("year_range", "Year Range:",
                        min = year_range[1], max = year_range[2],
                        value = year_range, step = 1, sep = ""),
            numericInput("forecast_yrs", "Forecast years ahead:",
                         value = 5, min = 1, max = 10, step = 1),
            hr(),
            helpText("Select at least one industry to enable the charts.")
          ),
          mainPanel(
            width = 9,
            uiOutput("emissions_kpi_row"),

            div(class = "viz-box",
                h4("Industry-wise Emissions Bubble Chart"),
                p(class = "graph-desc",
                  "Each bubble represents one industry in one year.
                   Bubble area is proportional to total emission volume."),
                plotlyOutput("emissions_bubble", height = "400px")
            ),

            div(class = "viz-box",
                h4("Emissions Share Over Time (Stacked Area)"),
                p(class = "graph-desc",
                  "Shows each industry's contribution to the total — useful for
                   spotting whether a sector's share is growing or shrinking even
                   when absolute values change."),
                plotlyOutput("emissions_area", height = "360px")
            ),

            div(class = "viz-box",
                h4("Total Emissions Trend with Forecast"),
                p(class = "graph-desc",
                  "Solid lines = historical data. Dashed lines = linear forecast.
                   Shaded ribbon = 95% confidence interval around the forecast."),
                plotlyOutput("emissions_trend", height = "400px")
            )
          )
        )
      ),

      # ======================================================================
      # Tab 3: Key Insights
      # ======================================================================
      tabPanel("Key Insights",
        div(style = "padding: 10px 0;",

          p(style = "font-size:15px; color:var(--muted); margin-bottom:24px;",
            "The findings below are drawn directly from the data in this dashboard.
             No technical knowledge is needed to read them — each card explains
             what the numbers mean in plain English."),

          # Section: Global Air Quality
          div(class = "insight-section-title", "Global Air Quality"),

          fluidRow(
            column(6,
              div(class = "insight-card",
                  h4("Wealthier countries breathe cleaner air"),
                  p(paste0(
                    "Cities in countries with a GDP per capita above $20,000 have an
                     average AQI of ", gdp_high_aqi, ", compared to ", gdp_low_aqi,
                    " in lower-income countries. That means people in poorer countries
                     are breathing air that is roughly ", gdp_ratio, "x more polluted
                     on average."
                  )),
                  span(class = "insight-highlight",
                       paste0(gdp_ratio, "x more polluted in low-income countries"))
              )
            ),
            column(6,
              div(class = "insight-card",
                  h4("Longer life expectancy goes hand-in-hand with cleaner air"),
                  p(paste0(
                    "Across all cities in the dataset, countries with higher life
                     expectancy tend to have ", le_direction, " AQI values
                     (correlation = ", round(le_cor, 2), "). This does not mean
                     pollution alone determines lifespan, but it is consistent with
                     decades of public health research linking air quality to
                     long-term health outcomes."
                  ))
              )
            )
          ),

          fluidRow(
            column(6,
              div(class = "insight-card",
                  h4("Most polluted countries (3+ cities in dataset)"),
                  tags$table(
                    style = "width:100%; border-collapse:collapse;",
                    tags$tr(
                      tags$th(style="text-align:left; padding:4px 8px; border-bottom:1px solid var(--border);", "Country"),
                      tags$th(style="text-align:right; padding:4px 8px; border-bottom:1px solid var(--border);", "Avg AQI")
                    ),
                    lapply(seq_len(nrow(most_polluted)), function(i) {
                      tags$tr(
                        tags$td(style="padding:4px 8px;", most_polluted$country[i]),
                        tags$td(style="padding:4px 8px; text-align:right; font-weight:700; color:#FF4136;",
                                most_polluted$avg_aqi[i])
                      )
                    })
                  )
              )
            ),
            column(6,
              div(class = "insight-card",
                  h4("Least polluted countries (3+ cities in dataset)"),
                  tags$table(
                    style = "width:100%; border-collapse:collapse;",
                    tags$tr(
                      tags$th(style="text-align:left; padding:4px 8px; border-bottom:1px solid var(--border);", "Country"),
                      tags$th(style="text-align:right; padding:4px 8px; border-bottom:1px solid var(--border);", "Avg AQI")
                    ),
                    lapply(seq_len(nrow(least_polluted)), function(i) {
                      tags$tr(
                        tags$td(style="padding:4px 8px;", least_polluted$country[i]),
                        tags$td(style="padding:4px 8px; text-align:right; font-weight:700; color:#2ECC40;",
                                least_polluted$avg_aqi[i])
                      )
                    })
                  )
              )
            )
          ),

          # Section: UK Emissions
          div(class = "insight-section-title", "UK Industrial Emissions"),

          fluidRow(
            column(6,
              div(class = "insight-card",
                  h4("UK emissions have fallen significantly"),
                  p(paste0(
                    "Between ", uk_first_yr, " and ", uk_last_yr,
                    ", total UK industrial emissions across all sectors and
                     pollutants tracked in this dataset changed by ",
                    uk_pct_change, "%. This represents a ",
                    ifelse(uk_pct_change < 0, "reduction", "increase"),
                    " of ", format(abs(uk_first_val - uk_last_val), big.mark = ","),
                    " units — driven by a combination of tighter environmental
                     regulation, improved industrial efficiency, and a shift away
                     from heavy manufacturing."
                  )),
                  span(class = "insight-highlight",
                       paste0(uk_pct_change, "% change since ", uk_first_yr))
              )
            ),
            column(6,
              div(class = "insight-card",
                  h4("Which industry reduced emissions the most?"),
                  p(paste0(
                    strong(top_reducer), " achieved the largest absolute reduction,
                     cutting emissions by approximately ",
                    format(top_reducer_drop, big.mark = ","),
                    " units over the period covered. This highlights how
                     targeted policy and technological investment in specific
                     sectors can deliver meaningful improvements in air quality."
                  )),
                  span(class = "insight-highlight",
                       paste0(top_reducer, ": -", format(top_reducer_drop, big.mark = ",")))
              )
            )
          ),

          # Section: What you can do
          div(class = "insight-section-title", "What This Means for You"),

          div(class = "insight-card",
              style = "border-left-color: var(--accent2);",
              h4("Understanding AQI in everyday life"),
              p("The Air Quality Index (AQI) is a number that summarises how clean
                or polluted the air is at any given moment. It is calculated from
                measurements of key pollutants — the higher the number, the greater
                the health risk. Governments around the world use AQI to issue health
                advisories and inform policy decisions."),
              tags$ul(
                tags$li(strong("0-50 (Good):"), " Safe for everyone."),
                tags$li(strong("51-100 (Moderate):"), " Acceptable, but unusually sensitive people may be affected."),
                tags$li(strong("101-150 (Unhealthy for Sensitive Groups):"),
                        " Children, elderly, and people with lung or heart conditions should limit outdoor time."),
                tags$li(strong("151-200 (Unhealthy):"), " Everyone may experience health effects."),
                tags$li(strong("201-300 (Very Unhealthy):"), " Health alert — avoid prolonged outdoor activity."),
                tags$li(strong("301+ (Hazardous):"), " Emergency conditions. Stay indoors.")
              ),
              p("Use the Global Air Quality tab to explore where each city falls on
                this scale, and the Health Risk panel to see personalised advice
                for whichever category you select.")
          ),

          div(class = "insight-card",
              style = "border-left-color: var(--accent2);",
              h4("Why industrial emissions matter"),
              p("Industrial activity is one of the largest sources of air pollutants
                including particulate matter, nitrogen oxides, and volatile organic
                compounds such as Benzene. When industries cut emissions — through
                cleaner technology, stricter regulation, or switching energy sources
                — the effect on local and regional air quality can be substantial.
                The UK Emissions tab shows this in action: several sectors have made
                dramatic cuts over the decades covered by this dataset, demonstrating
                that policy and investment can work.")
          )
        )
      )   # end Tab 3
    ),    # end navbarPage

    # Footer ------------------------------------------------------------------
    div(class = "footer",
        p(strong("Data Exploration and Visualisation Project")),
        p("Created by: Stuti Baj  |  Student ID: 31981259"),
        p(paste("Last Updated:", format(Sys.Date(), "%B %Y")))
    )
  )
)

# 4. Server -------------------------------------------------------------------
server <- function(input, output, session) {

  # Dark mode -----------------------------------------------------------------
  observeEvent(input$darkMode, {
    theme <- if (input$darkMode) "dark" else "light"
    runjs(sprintf(
      "document.documentElement.setAttribute('data-bs-theme', '%s');", theme
    ))
  })

  # Selection caps ------------------------------------------------------------
  observe({
    if (length(input$countries) > 5)
      updateSelectInput(session, "countries", selected = tail(input$countries, 5))
  })
  observeEvent(input$reset_countries,  {
    updateSelectInput(session, "countries",  selected = character(0))
  })
  observe({
    if (length(input$industries) > 5)
      updateSelectInput(session, "industries", selected = tail(input$industries, 5))
  })
  observeEvent(input$reset_industries, {
    updateSelectInput(session, "industries", selected = character(0))
  })

  # ── Tab 1 reactive data ───────────────────────────────────────────────────
  filtered_data <- reactive({
    req(input$socio_factor, input$gas, input$aqi_cat)
    df <- merged_data
    if (length(input$countries) > 0)
      df <- df %>% filter(country %in% input$countries)
    if (input$gas != "All") {
      df <- df %>%
        filter(gas == input$gas) %>%
        mutate(display_aqi_category = aqi_category,
               display_aqi_value    = aqi_value)
    } else {
      df <- df %>%
        mutate(display_aqi_category = overall_aqi_category,
               display_aqi_value    = overall_aqi_value)
    }
    if (input$aqi_cat != "All")
      df <- df %>% filter(display_aqi_category == input$aqi_cat)
    df
  })

  # ── KPI cards (Tab 1) ─────────────────────────────────────────────────────
  output$kpi_row <- renderUI({
    df <- filtered_data()
    n_cities    <- length(unique(df$city))
    n_countries <- length(unique(df$country))
    avg_aqi     <- round(mean(df$display_aqi_value, na.rm = TRUE), 1)
    pct_good    <- round(mean(df$display_aqi_category == "Good", na.rm = TRUE) * 100, 1)

    div(class = "kpi-row",
        div(class = "kpi-card",
            div(class = "kpi-val", format(n_cities, big.mark = ",")),
            div(class = "kpi-lbl", "Cities")),
        div(class = "kpi-card",
            div(class = "kpi-val", n_countries),
            div(class = "kpi-lbl", "Countries")),
        div(class = "kpi-card",
            div(class = "kpi-val", avg_aqi),
            div(class = "kpi-lbl", "Avg AQI")),
        div(class = "kpi-card",
            div(class = "kpi-val", paste0(pct_good, "%")),
            div(class = "kpi-lbl", "Cities with Good AQI"))
    )
  })

  # ── Health Risk explainer panel ───────────────────────────────────────────
  output$health_panel <- renderUI({
    # Show panel only when a specific category is selected
    cat_sel <- input$aqi_cat
    if (cat_sel == "All" || is.null(cat_sel)) {
      return(div(
        class = "health-panel",
        style = paste0("border-color:#dee2e6; background:#f8f9fa; color:#2c3e50;"),
        h4("Health Risk Guide"),
        p("Select a specific AQI category from the sidebar to see personalised
           health advice for that pollution level.")
      ))
    }

    info <- HEALTH_RISK[[cat_sel]]
    if (is.null(info)) return(NULL)

    text_col <- if (cat_sel == "Moderate") "#2c3e50" else "#ffffff"

    div(
      class = "health-panel",
      style = paste0(
        "border-color:", info$colour, ";",
        "background:", info$colour, ";",
        "color:", text_col, ";"
      ),
      h4(paste("Health Risk Level:", cat_sel)),
      div(class = "health-who",
          paste("Who is most at risk:", info$who)),
      div(class = "health-risk", info$risk),
      div(class = "health-advice",
          strong("Recommended action: "), info$advice)
    )
  })

  # ── Download filtered data ────────────────────────────────────────────────
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("aqi_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  # ── Leaflet map ───────────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No cities match the current filters."))

    pal_factor <- colorFactor(palette = AQI_COLORS, levels = names(AQI_COLORS))

    if (input$map_type == "dots") {
      # City dot map
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          ~lng, ~lat,
          color       = ~pal_factor(display_aqi_category),
          radius      = 5,
          fillOpacity = 0.75,
          stroke      = FALSE,
          label       = ~lapply(paste0(
            "<b>", city, ", ", country, "</b><br/>",
            "AQI Category: ", display_aqi_category, "<br/>",
            "AQI Value: ", round(display_aqi_value, 1), "<br/>",
            input$socio_factor, ": ", get(input$socio_factor)
          ), HTML)
        ) %>%
        addLegend(position = "bottomright", pal = pal_factor,
                  values = names(AQI_COLORS), title = "AQI Category", opacity = 1)

    } else {
      # Country choropleth — aggregate to country level
      country_avg <- data %>%
        group_by(country) %>%
        summarise(avg_aqi = mean(display_aqi_value, na.rm = TRUE),
                  lat     = mean(lat, na.rm = TRUE),
                  lng     = mean(lng, na.rm = TRUE),
                  n_cities = n(),
                  .groups = "drop")

      pal_num <- colorNumeric(palette = c("#2ECC40","#FFDC00","#FF851B","#FF4136","#85144b"),
                              domain = country_avg$avg_aqi)

      # Rescale radius without external dependency
      min_aqi <- min(country_avg$avg_aqi, na.rm = TRUE)
      max_aqi <- max(country_avg$avg_aqi, na.rm = TRUE)
      country_avg <- country_avg %>%
        mutate(radius_val = 6 + (avg_aqi - min_aqi) /
                 (max(max_aqi - min_aqi, 1e-9)) * 12)

      leaflet(country_avg) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          ~lng, ~lat,
          fillColor   = ~pal_num(avg_aqi),   # data-driven fill
          fillOpacity = 0.85,
          color       = "white",             # stroke only — no duplicate
          weight      = 1,
          stroke      = TRUE,
          radius      = ~radius_val,
          label       = ~lapply(paste0(
            "<b>", country, "</b><br/>",
            "Average AQI: ", round(avg_aqi, 1), "<br/>",
            "Cities in dataset: ", n_cities
          ), HTML)
        ) %>%
        addLegend(position = "bottomright", pal = pal_num,
                  values = ~avg_aqi, title = "Avg AQI", opacity = 1)
    }
  })

  # ── AQI distribution bar ──────────────────────────────────────────────────
  output$aqi_dist <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No data to display."))

    counts <- data %>%
      count(display_aqi_category) %>%
      mutate(display_aqi_category = factor(
        display_aqi_category, levels = names(AQI_COLORS))) %>%
      arrange(display_aqi_category)

    plot_ly(counts, x = ~display_aqi_category, y = ~n,
            type = "bar", color = ~display_aqi_category, colors = AQI_COLORS,
            hovertemplate = "%{x}: %{y} cities<extra></extra>") %>%
      layout(xaxis = list(title = "AQI Category"),
             yaxis = list(title = "Number of Cities"),
             showlegend = FALSE,
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  # ── Socio-economic scatter with Pearson r ─────────────────────────────────
  output$socio_correlation <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No data to display."))
    x_col <- input$socio_factor

    plot_ly(data,
            x     = as.formula(paste0("~`", x_col, "`")),
            y     = ~display_aqi_value,
            type  = "scatter", mode = "markers",
            color = ~display_aqi_category, colors = AQI_COLORS,
            marker = list(size = 7, opacity = 0.7),
            hovertemplate = paste0(
              "<b>%{text}</b><br>", x_col, ": %{x}<br>AQI: %{y}<extra></extra>"
            ),
            text = ~paste(city, country, sep = ", ")) %>%
      layout(xaxis = list(title = x_col),
             yaxis = list(title = "AQI Value"),
             showlegend = TRUE,
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  # Pearson r label below the scatter
  output$pearson_label <- renderUI({
    data <- filtered_data()
    x_col <- input$socio_factor
    vals  <- data[[x_col]]
    if (is.numeric(vals) && sum(!is.na(vals)) > 3) {
      r  <- round(cor(vals, data$display_aqi_value, use = "complete.obs"), 3)
      strength <- dplyr::case_when(
        abs(r) >= 0.7 ~ "strong",
        abs(r) >= 0.4 ~ "moderate",
        TRUE          ~ "weak"
      )
      direction <- if (r < 0) "negative" else "positive"
      div(style = "font-size:13px; color:var(--muted); margin-top:6px; padding:8px;
                   background:var(--box-bg); border-radius:4px; border:1px solid var(--border);",
          strong(paste0("Pearson r = ", r)),
          paste0(" — ", strength, " ", direction, " relationship between ",
                 x_col, " and AQI.")
      )
    } else {
      div(style = "font-size:13px; color:var(--muted); margin-top:6px;",
          "Correlation not available for non-numeric or insufficient data.")
    }
  })

  # ── Top 10 most polluted cities table ─────────────────────────────────────
  output$top10_table <- renderDT({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "No data to display."))

    top10 <- data %>%
      group_by(city, country) %>%
      summarise(avg_aqi      = round(mean(display_aqi_value, na.rm = TRUE), 1),
                aqi_category = first(display_aqi_category),
                .groups      = "drop") %>%
      arrange(desc(avg_aqi)) %>%
      head(10) %>%
      rename(City = city, Country = country,
             "Average AQI" = avg_aqi, "AQI Category" = aqi_category)

    datatable(top10,
              options  = list(dom = "t", pageLength = 10, ordering = FALSE),
              rownames = FALSE,
              class    = "stripe hover") %>%
      formatStyle("Average AQI",
                  background = styleColorBar(range(top10$"Average AQI"), "#FF4136"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ── Tab 2: emissions reactive data ────────────────────────────────────────
  emissions_filtered <- reactive({
    req(length(input$industries) > 0)
    base <- uk_emissions %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             Industry %in% input$industries)
    if (input$pollutants != "All")
      base <- base %>% filter(Gas == input$pollutants)
    base
  })

  # ── Emissions KPI row (Tab 2) ─────────────────────────────────────────────
  output$emissions_kpi_row <- renderUI({
    validate(need(length(input$industries) > 0, ""))
    base <- emissions_filtered()
    validate(need(nrow(base) > 0, ""))

    total_by_yr <- base %>%
      group_by(year) %>%
      summarise(total = sum(Emission_Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)

    n_yrs <- nrow(total_by_yr)
    if (n_yrs >= 2) {
      first_v <- total_by_yr$total[1]
      last_v  <- total_by_yr$total[n_yrs]
      yoy_pct <- round((last_v - first_v) / abs(first_v) * 100, 1)
      yoy_lbl <- paste0(ifelse(yoy_pct > 0, "+", ""), yoy_pct, "%")
      yoy_col <- ifelse(yoy_pct < 0, "#2ECC40", "#FF4136")
    } else {
      yoy_lbl <- "N/A"
      yoy_col <- "var(--accent)"
    }

    total_emissions <- round(sum(base$Emission_Value, na.rm = TRUE), 0)
    n_industries    <- length(unique(base$Industry))

    div(class = "kpi-row",
        div(class = "kpi-card",
            div(class = "kpi-val", format(total_emissions, big.mark = ",")),
            div(class = "kpi-lbl", "Total Emissions (selected period)")),
        div(class = "kpi-card",
            div(class = "kpi-val", style = paste0("color:", yoy_col), yoy_lbl),
            div(class = "kpi-lbl", paste0("Change from ", min(base$year), " to ", max(base$year)))),
        div(class = "kpi-card",
            div(class = "kpi-val", n_industries),
            div(class = "kpi-lbl", "Industries Selected"))
    )
  })

  # ── Bubble chart ──────────────────────────────────────────────────────────
  output$emissions_bubble <- renderPlotly({
    validate(need(length(input$industries) > 0, "Please select at least one industry."))
    base <- emissions_filtered()
    validate(need(nrow(base) > 0, "No data for the current selection."))

    is_benzene <- input$pollutants == "Benzene"

    data <- base %>%
      group_by(year, Industry) %>%
      summarise(Emission_Value = sum(Emission_Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        size_val   = if (is_benzene) log10(Emission_Value + 1) else Emission_Value,
        hover_text = paste0(
          "Year: ", year, "<br>Industry: ", Industry, "<br>Emissions: ",
          format(round(Emission_Value, 2), big.mark = ","),
          if (is_benzene) " (log-scaled bubble)" else ""
        ),
        y_pos = as.numeric(as.factor(Industry))
      )

    max_sz  <- max(data$size_val, na.rm = TRUE)
    sizeref <- max_sz / (30^2)

    plot_ly(data, x = ~year, y = ~y_pos, size = ~size_val,
            color = ~Industry, colors = INDUSTRY_COLORS,
            type = "scatter", mode = "markers",
            marker = list(sizemode = "area", sizeref = sizeref, sizemin = 4,
                          opacity = 0.75, line = list(color = "white", width = 0.5)),
            hoverinfo = "text", text = ~hover_text) %>%
      layout(
        xaxis = list(title = "Year", tickmode = "linear", dtick = 1),
        yaxis = list(title = "", tickvals = unique(data$y_pos),
                     ticktext = levels(droplevels(data$Industry)),
                     showticklabels = TRUE),
        showlegend = TRUE, hovermode = "closest",
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  # ── Stacked area chart ────────────────────────────────────────────────────
  output$emissions_area <- renderPlotly({
    validate(need(length(input$industries) > 0, "Please select at least one industry."))
    base <- emissions_filtered()
    validate(need(nrow(base) > 0, "No data for the current selection."))

    area_data <- base %>%
      group_by(year, Industry) %>%
      summarise(Emission_Value = sum(Emission_Value, na.rm = TRUE), .groups = "drop")

    industries <- levels(droplevels(area_data$Industry))
    colors_use <- INDUSTRY_COLORS[seq_along(industries)]

    plot_ly() %>%
      {
        p <- .
        for (i in seq_along(industries)) {
          ind_data <- area_data %>% filter(Industry == industries[i])
          p <- add_trace(p,
            data       = ind_data,
            x          = ~year,
            y          = ~Emission_Value,
            name       = industries[i],
            type       = "scatter",
            mode       = "none",
            fill       = if (i == 1) "tozeroy" else "tonexty",
            fillcolor  = paste0(colors_use[i], "bb"),
            line       = list(color = colors_use[i]),
            hovertemplate = paste0(
              industries[i], "<br>Year: %{x}<br>Emissions: %{y:.2f}<extra></extra>"
            )
          )
        }
        p
      } %>%
      layout(
        xaxis     = list(title = "Year"),
        yaxis     = list(title = "Emissions"),
        hovermode = "x unified",
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  # ── Trend chart with forecast ─────────────────────────────────────────────
  output$emissions_trend <- renderPlotly({
    validate(need(length(input$industries) > 0, "Please select at least one industry."))
    base <- emissions_filtered()
    validate(need(nrow(base) > 0, "No data for the current selection."))

    hist_data <- base %>%
      group_by(year, Industry) %>%
      summarise(Emission_Value = sum(Emission_Value, na.rm = TRUE), .groups = "drop")

    industries <- levels(droplevels(hist_data$Industry))
    fwd        <- input$forecast_yrs
    max_yr     <- max(hist_data$year)
    future_yrs <- seq(max_yr + 1, max_yr + fwd)
    colors_use <- INDUSTRY_COLORS[seq_along(industries)]

    p <- plot_ly()

    for (i in seq_along(industries)) {
      ind   <- industries[i]
      col   <- colors_use[i]
      d     <- hist_data %>% filter(Industry == ind) %>% arrange(year)

      # Historical line
      p <- add_trace(p, data = d, x = ~year, y = ~Emission_Value,
                     name = ind, type = "scatter", mode = "lines+markers",
                     line   = list(color = col, width = 2),
                     marker = list(color = col, size = 7),
                     hovertemplate = paste0(
                       ind, "<br>Year: %{x}<br>Emissions: %{y:.2f}<extra></extra>"
                     ))

      # Fit linear model for forecast (need at least 3 points)
      if (nrow(d) >= 3) {
        fit      <- lm(Emission_Value ~ year, data = d)
        new_df   <- data.frame(year = future_yrs)
        pred     <- predict(fit, new_df, interval = "confidence")
        fcast_df <- data.frame(year = future_yrs,
                               fit  = pred[, "fit"],
                               lwr  = pred[, "lwr"],
                               upr  = pred[, "upr"])

        # Dashed forecast line
        p <- add_trace(p, data = fcast_df, x = ~year, y = ~fit,
                       name = paste(ind, "(forecast)"),
                       type = "scatter", mode = "lines",
                       line = list(color = col, width = 2, dash = "dash"),
                       showlegend = FALSE,
                       hovertemplate = paste0(
                         ind, " forecast<br>Year: %{x}<br>Est: %{y:.2f}<extra></extra>"
                       ))

        # Confidence ribbon
        p <- add_ribbons(p, data = fcast_df,
                         x     = ~year, ymin = ~lwr, ymax = ~upr,
                         name  = paste(ind, "95% CI"),
                         fillcolor = paste0(col, "33"),
                         line  = list(color = "transparent"),
                         showlegend = FALSE,
                         hoverinfo = "skip")
      }
    }

    p %>% layout(
      xaxis     = list(title = "Year"),
      yaxis     = list(title = "Total Emissions"),
      hovermode = "x unified",
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
    )
  })

}  # end server

# 5. Launch -------------------------------------------------------------------
shinyApp(ui, server)
