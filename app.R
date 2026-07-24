rm(list = ls())
gc()

# ============================================================
# PACKAGES
# ============================================================

required_packages <- c(
  "shiny", "shinyWidgets", "dplyr", "tidyr", "ggplot2",
  "leaflet", "sf", "DT", "plotly", "stringr"
)

missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running the app: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(sf)
library(DT)
library(plotly)
library(stringr)

# ============================================================
# FILE PATHS
# Keep all data files in the same folder as app.R.
# The municipality shapefile should remain inside:
# ./gadm36_PRT_shp/gadm36_PRT_2.shp
# ============================================================

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

hospital_data_file <- file.path(app_dir, "df_shiny4.RData")
test_file          <- file.path(app_dir, "test.rds")
results_file       <- file.path(app_dir, "df_resultados.rds")
results2_file      <- file.path(app_dir, "df_resultados2.rds")
rr_file            <- file.path(app_dir, "rr_shiny_data.rds")
population_file_1  <- file.path(app_dir, "pop2010_2012.csv")
population_file_2  <- file.path(app_dir, "pop2013_2018.csv")
shape_file         <- file.path(app_dir, "gadm36_PRT_shp", "gadm36_PRT_2.shp")

files_to_check <- c(
  hospital_data_file,
  test_file,
  results_file,
  results2_file,
  rr_file,
  population_file_1,
  population_file_2,
  shape_file
)

missing_files <- files_to_check[!file.exists(files_to_check)]

if (length(missing_files) > 0) {
  stop(
    "The following required files were not found:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ============================================================
# LOAD AND PREPARE DATA
# ============================================================

hospital_env <- new.env(parent = emptyenv())
load(hospital_data_file, envir = hospital_env)

if (!exists("df_shiny4", envir = hospital_env, inherits = FALSE)) {
  stop("The file df_shiny4.RData must contain an object named 'df_shiny4'.")
}

df <- get("df_shiny4", envir = hospital_env) %>%
  mutate(
    Grupo_Diag_Homog = trimws(as.character(Grupo_Diag_Homog)),
    GCD               = trimws(as.character(GCD)),
    concelho.y        = trimws(as.character(concelho.y)),
    ano               = as.integer(ano),
    discharge         = as.character(discharge)
  )

rm(hospital_env)
gc()

population_data <- bind_rows(
  read.csv(
    population_file_1,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ),
  read.csv(
    population_file_2,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
) %>%
  transmute(
    concelho.y = trimws(as.character(concelho.y)),
    ano = as.integer(ano),
    Population = suppressWarnings(as.numeric(Pop))
  ) %>%
  filter(
    !is.na(concelho.y),
    nzchar(concelho.y),
    is.finite(ano),
    is.finite(Population),
    Population > 0
  ) %>%
  distinct(concelho.y, ano, .keep_all = TRUE)

required_population_years <- sort(unique(df$ano))
available_population_years <- sort(unique(population_data$ano))
missing_population_years <- setdiff(
  required_population_years,
  available_population_years
)

if (length(missing_population_years) > 0) {
  warning(
    "Population data are missing for the following years: ",
    paste(missing_population_years, collapse = ", ")
  )
}

pt_cont <- st_read(shape_file, quiet = TRUE) %>%
  filter(!NAME_1 %in% c("Madeira", "Azores"))

test <- readRDS(test_file)
df_resultados <- readRDS(results_file)
df_resultados2 <- readRDS(results2_file)

rr_data <- readRDS(rr_file)

required_rr_columns <- c(
  "city_id", "NAME_2", "myear", "date", "year", "month_lab",
  "RR4", "RR5", "RR6", "PP4", "PP5", "PP6",
  "density4", "density5", "density6"
)

missing_rr_columns <- setdiff(required_rr_columns, names(rr_data))

if (length(missing_rr_columns) > 0) {
  stop(
    "The file rr_shiny_data.rds is missing the following columns: ",
    paste(missing_rr_columns, collapse = ", ")
  )
}

rr_data <- rr_data %>%
  mutate(
    city_id = as.integer(city_id),
    NAME_2 = trimws(as.character(NAME_2)),
    myear = as.character(myear),
    date = as.Date(date),
    year = as.integer(year),
    month_lab = factor(
      as.character(month_lab),
      levels = month.abb,
      ordered = TRUE
    )
  )

rr_outcome_choices <- c(
  "Respiratory cancer hospitalizations" = "RR4",
  "High disease severity" = "RR5",
  "In-hospital deaths" = "RR6"
)

rr_outcome_label <- function(variable) {
  names(rr_outcome_choices)[match(variable, rr_outcome_choices)]
}

# ============================================================
# HELPERS
# ============================================================

analysis_choices <- c(
  "Hospital admissions",
  "In-hospital mortality proportion (%)",
  "Mean length of stay (days)"
)

map_analysis_choices <- c(
  "Annual hospitalization rate per 1,000 inhabitants",
  "Hospital admissions",
  "In-hospital mortality proportion (%)",
  "Mean length of stay (days)"
)

all_diseases_label <- "All diseases"
all_gcd_label <- "All major diagnostic categories"

metric_axis_label <- function(metric) {
  switch(
    metric,
    "Annual hospitalization rate per 1,000 inhabitants" = "Annual hospitalization rate per 1,000 inhabitants",
    "Hospital admissions" = "Number of admissions",
    "In-hospital mortality proportion (%)" = "In-hospital mortality (%)",
    "Mean length of stay (days)" = "Mean length of stay (days)"
  )
}

metric_suffix <- function(metric) {
  switch(
    metric,
    "Annual hospitalization rate per 1,000 inhabitants" = " per 1,000 inhabitants",
    "Hospital admissions" = " admissions",
    "In-hospital mortality proportion (%)" = "% deaths",
    "Mean length of stay (days)" = " days"
  )
}

classify_patient <- function(id) {
  info <- test[id, c("Y1", "Y2")]

  outcome <- if_else(info$Y1 == 1, "death", "discharge")
  severity <- if_else(info$Y2 == 1, "high severity", "low severity")

  paste0("Patient ", id, " — ", severity, " and ", outcome)
}

prepare_patient <- function(id) {
  res1 <- df_resultados %>%
    filter(id_pat == id)

  res2 <- df_resultados2 %>%
    filter(id_pat == id) %>%
    select(id_pat, time, lower2, resultado2, upper2)

  res1 %>%
    left_join(res2, by = c("id_pat", "time")) %>%
    rename(res_model2 = resultado2) %>%
    pivot_longer(
      cols = c(resultado, res_model2),
      names_to = "Model",
      values_to = "Value"
    ) %>%
    mutate(
      Model = factor(
        Model,
        levels = c("resultado", "res_model2"),
        labels = c("Model IX", "Model III")
      ),
      IC_lower = if_else(Model == "Model IX", lower, lower2),
      IC_upper = if_else(Model == "Model IX", upper, upper2),
      patient = classify_patient(id)
    )
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f6f8fb;
        color: #243447;
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }

      .container-fluid {
        padding-left: 28px;
        padding-right: 28px;
      }

      .app-header {
        margin-top: 20px;
        margin-bottom: 18px;
      }

      .app-title {
        font-weight: 700;
        color: #243447;
        margin: 0;
      }

      .app-subtitle {
        color: #6b7785;
        margin-top: 6px;
        margin-bottom: 0;
      }

      .nav-tabs {
        border-bottom: 1px solid #dfe5ec;
        margin-bottom: 22px;
      }

      .nav-tabs > li > a {
        color: #5c6977;
        font-weight: 700;
        border: 0;
        border-radius: 10px 10px 0 0;
        padding: 12px 18px;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #243447;
        background-color: white;
        border: 1px solid #e1e6ec;
        border-bottom-color: white;
      }

      .sidebar-panel,
      .content-panel,
      .plot-panel {
        background: white;
        border-radius: 16px;
        padding: 22px;
        box-shadow: 0 4px 18px rgba(0,0,0,0.08);
        border: 1px solid #e6e9ef;
        margin-bottom: 22px;
      }

      .sidebar-panel {
        position: sticky;
        top: 16px;
      }

      label {
        font-weight: 700;
        color: #243447;
      }

      .form-control,
      .btn,
      .dropdown-toggle {
        border-radius: 10px !important;
        border: 1px solid #cfd6df;
      }

      .btn-default {
        background-color: #ffffff;
        color: #243447;
      }

      .btn-default:hover {
        background-color: #edf2f7;
      }

      .bootstrap-select .dropdown-menu {
        border-radius: 12px;
        box-shadow: 0 6px 22px rgba(0,0,0,0.12);
      }

      .dropdown-menu > li > a {
        padding: 8px 14px;
      }

      .dropdown-menu > li.selected > a {
        background-color: #243447 !important;
        color: white !important;
      }

      .help-text {
        font-size: 13px;
        color: #6b7785;
        margin-top: -4px;
        margin-bottom: 18px;
        line-height: 1.45;
      }

      .section-title {
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #6b7785;
        font-weight: 800;
        margin-bottom: 14px;
      }

      .panel-title {
        font-size: 18px;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 16px;
        color: #243447;
      }

      table.dataTable thead th {
        background-color: #243447;
        color: white;
      }

      .leaflet-container {
        border-radius: 12px;
      }

      .density-layout {
        display: flex;
        gap: 22px;
        align-items: flex-start;
      }

      .density-filter-panel {
        flex: 0 0 245px;
        background: #f7f9fc;
        border: 1px solid #e1e6ec;
        border-radius: 12px;
        padding: 18px;
      }

      .density-plot-panel {
        flex: 1 1 auto;
        min-width: 0;
      }

      @media (max-width: 900px) {
        .density-layout {
          display: block;
        }

        .density-filter-panel {
          width: 100%;
          margin-bottom: 18px;
        }
      }

      .app-footer {
        background-color: #162635;
        color: #c7d0d9;
        padding: 12px 22px;
        margin-top: 12px;
        font-size: 14px;
        border-radius: 12px 12px 0 0;
      }
    "))
  ),

  div(
    class = "app-header",
    h2("BDMH Explorer", class = "app-title"),
    p(
      "Explore hospital admissions, mortality, length of stay, spatial patterns, and patient-specific hazard predictions.",
      class = "app-subtitle"
    )
  ),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      title = "Diagnostic Groups",
      value = "diagnostic_groups",
      fluidRow(
        column(
          width = 3,
          div(
            class = "sidebar-panel",
            div("Filters", class = "section-title"),
            selectInput(
              "gcd_select",
              "Major diagnostic category",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              "gdh_select",
              "Diagnostic subgroup (GDH)",
              choices = NULL,
              selected = NULL
            ),
            radioButtons(
              "analysis_type",
              "Outcome measure",
              choices = analysis_choices,
              selected = "Hospital admissions"
            )
          )
        ),
        column(
          width = 9,
          div(
            class = "content-panel",
            h3("Diagnostic subgroup summary", class = "panel-title"),
            DTOutput("subgroup_table")
          ),
          div(
            class = "plot-panel",
            h3("Leading diagnostic subgroups", class = "panel-title"),
            plotOutput("subgroup_plot", height = "520px")
          ),
          div(
            class = "plot-panel",
            h3("Annual trends for the leading subgroups", class = "panel-title"),
            plotlyOutput("temporal_plot", height = "620px")
          )
        )
      )
    ),

    tabPanel(
      title = "Interactive Map",
      value = "interactive_map",
      fluidRow(
        column(
          width = 3,
          div(
            class = "sidebar-panel",
            div("Filters", class = "section-title"),
            selectInput(
              "gcd_map",
              "Major diagnostic category",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              "year_map",
              "Year",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              "disease_map",
              "Diagnostic subgroup",
              choices = NULL,
              selected = NULL
            ),
            radioButtons(
              "analysis_type_map",
              "Outcome measure",
              choices = map_analysis_choices,
              selected = "Annual hospitalization rate per 1,000 inhabitants"
            ),
            div(
              "Hospitalization rates require annual municipality population data.",
              class = "help-text"
            )
          )
        ),
        column(
          width = 9,
          div(
            class = "plot-panel",
            h3("Annual municipality-level outcomes", class = "panel-title"),
            leafletOutput("interactive_map", height = "760px")
          )
        )
      )
    ),

    tabPanel(
      title = "Respiratory Cancer Risk",
      value = "respiratory_cancer_risk",
      fluidRow(
        column(
          width = 3,
          div(
            class = "sidebar-panel",
            div("Filters", class = "section-title"),
            selectInput(
              "rr_outcome",
              "Outcome",
              choices = rr_outcome_choices,
              selected = "RR4"
            ),
            selectInput(
              "rr_year",
              "Year",
              choices = sort(unique(rr_data$year)),
              selected = min(rr_data$year, na.rm = TRUE)
            ),
            selectInput(
              "rr_month",
              "Month",
              choices = month.abb,
              selected = "Jan"
            ),
            pickerInput(
              inputId = "rr_cities",
              label = "Municipalities for temporal trends",
              choices = sort(unique(rr_data$NAME_2)),
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                selectedTextFormat = "count > 2",
                noneSelectedText = "Highlight persistent-risk municipalities"
              )
            ),
          )
        ),
        column(
          width = 9,
          div(
            class = "plot-panel",
            h3("Posterior relative-risk map", class = "panel-title"),
            leafletOutput("rr_map", height = "650px")
          ),
          div(
            class = "content-panel",
            h3("Municipalities with RR > 1 throughout 2010–2017", class = "panel-title"),
            DTOutput("rr_persistent_table")
          ),
          div(
            class = "plot-panel",
            h3("Monthly relative-risk trajectories", class = "panel-title"),
            plotlyOutput("rr_time_plot", height = "580px")
          ),
          div(
            class = "plot-panel",
            h3("Posterior relative-risk densities", class = "panel-title"),
            div(
              class = "density-layout",
              div(
                class = "density-filter-panel",
                div("Density filters", class = "section-title"),
                selectInput(
                  "rr_density_city",
                  "Municipality",
                  choices = sort(unique(rr_data$NAME_2)),
                  selected = if (
                    "Lisboa" %in% rr_data$NAME_2
                  ) {
                    "Lisboa"
                  } else {
                    sort(unique(rr_data$NAME_2))[[1]]
                  }
                ),
                selectInput(
                  "rr_density_year",
                  "Year",
                  choices = sort(unique(rr_data$year)),
                  selected = min(rr_data$year, na.rm = TRUE)
                ),
                selectInput(
                  "rr_density_month",
                  "Month",
                  choices = c("All months", month.abb),
                  selected = "All months"
                ),
                div(
                  paste0(
                    "Only months with posterior probability ",
                    "P(RR > 1 | y) ≥ 0.90 are displayed."
                  ),
                  class = "help-text"
                ),
                div(
                  paste0(
                    "Relative risk above 1 indicates risk above ",
                    "the national reference level."
                  ),
                  class = "help-text"
                )
              ),
              div(
                class = "density-plot-panel",
                plotOutput("rr_density_plot", height = "620px")
              )
            )
          )
        )
      )
    ),

    tabPanel(
      title = "Hazard Predictions",
      value = "hazard_predictions",
      fluidRow(
        column(
          width = 4,
          div(
            class = "sidebar-panel",
            div("Filters", class = "section-title"),
            selectInput(
              "outcome_filter",
              "Patient outcome",
              choices = c("All", "death", "discharge"),
              selected = "All"
            ),
            selectInput(
              "severity_filter",
              "Disease severity",
              choices = c("All", "high severity", "low severity"),
              selected = "All"
            ),
            div(
              "Use the filters to narrow the patient list. Patients already selected remain displayed.",
              class = "help-text"
            ),
            pickerInput(
              inputId = "ids",
              label = "Choose patients",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                selectedTextFormat = "count > 2",
                noneSelectedText = "No patient selected",
                countSelectedText = "{0} patients selected"
              )
            )
          )
        ),
        column(
          width = 8,
          div(
            class = "plot-panel",
            h3("Predicted hazard trajectories", class = "panel-title"),
            plotOutput("hazard_plot", height = "720px")
          )
        )
      )
    )
  ),

  tags$footer(
    class = "app-footer",
    fluidRow(
      column(
        width = 12,
        align = "center",
        HTML(
          "© 2026 Felipe Barletta | All Rights Reserved |
          <a href='https://www.linkedin.com/in/felipe-emanoel-barletta-mendes-b54914b2/' target='_blank' rel='noopener noreferrer'
             style='color:#9ec3ff; text-decoration:none; margin-left:8px; margin-right:8px;'>
             LinkedIn
          </a> |
          <a href='YOUR_GITHUB_LINK' target='_blank'
             style='color:#9ec3ff; text-decoration:none; margin-left:8px;'>
             GitHub
          </a>"
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # TAB 1: DIAGNOSTIC GROUPS
  # ----------------------------------------------------------

  observe({
    gcd_choices <- sort(unique(df$GCD))
    updateSelectInput(
      session,
      "gcd_select",
      choices = gcd_choices,
      selected = gcd_choices[[1]]
    )
  })

  observeEvent(input$gcd_select, {
    req(input$gcd_select)

    gdh_choices <- df %>%
      filter(GCD == input$gcd_select) %>%
      distinct(Grupo_Diag_Homog) %>%
      arrange(Grupo_Diag_Homog) %>%
      pull(Grupo_Diag_Homog)

    updateSelectInput(
      session,
      "gdh_select",
      choices = c(all_diseases_label, gdh_choices),
      selected = all_diseases_label
    )
  }, ignoreInit = FALSE)

  subgroup_summary <- reactive({
    req(input$gcd_select, input$gdh_select, input$analysis_type)

    filtered_data <- df %>%
      filter(GCD == input$gcd_select)

    if (input$gdh_select != all_diseases_label) {
      filtered_data <- filtered_data %>%
        filter(Grupo_Diag_Homog == input$gdh_select)
    }

    if (input$analysis_type == "Hospital admissions") {
      filtered_data %>%
        group_by(Grupo_Diag_Homog) %>%
        summarise(Value = n(), .groups = "drop") %>%
        arrange(desc(Value))

    } else if (input$analysis_type == "In-hospital mortality proportion (%)") {
      filtered_data %>%
        group_by(Grupo_Diag_Homog) %>%
        summarise(
          Value = round(mean(discharge == "Death", na.rm = TRUE) * 100, 2),
          .groups = "drop"
        ) %>%
        arrange(desc(Value))

    } else {
      filtered_data %>%
        group_by(Grupo_Diag_Homog) %>%
        summarise(
          Value = round(mean(los, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        arrange(desc(Value))
    }
  })

  output$subgroup_table <- renderDT({
    subgroup_summary() %>%
      rename(
        `Diagnostic subgroup` = Grupo_Diag_Homog,
        !!metric_axis_label(input$analysis_type) := Value
      )
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)

  output$subgroup_plot <- renderPlot({
    plot_data <- subgroup_summary() %>%
      slice_head(n = 20)

    validate(need(nrow(plot_data) > 0, "No data are available for this selection."))

    ggplot(
      plot_data,
      aes(x = reorder(Grupo_Diag_Homog, Value), y = Value)
    ) +
      geom_col(fill = "#243447", width = 0.72) +
      coord_flip() +
      labs(
        title = paste(input$analysis_type, "—", input$gcd_select),
        x = NULL,
        y = metric_axis_label(input$analysis_type)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", color = "#243447"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "#243447"),
        panel.grid.minor = element_blank()
      )
  })

  temporal_summary <- reactive({
    req(input$gcd_select, input$gdh_select, input$analysis_type)

    filtered_data <- df %>%
      filter(GCD == input$gcd_select)

    if (input$gdh_select != all_diseases_label) {
      filtered_data <- filtered_data %>%
        filter(Grupo_Diag_Homog == input$gdh_select)
    }

    if (input$analysis_type == "Hospital admissions") {
      top_diseases <- filtered_data %>%
        count(Grupo_Diag_Homog, name = "total") %>%
        arrange(desc(total)) %>%
        slice_head(n = 8) %>%
        pull(Grupo_Diag_Homog)

      filtered_data %>%
        filter(Grupo_Diag_Homog %in% top_diseases) %>%
        group_by(ano, Grupo_Diag_Homog) %>%
        summarise(Value = n(), .groups = "drop")

    } else if (input$analysis_type == "In-hospital mortality proportion (%)") {
      top_diseases <- filtered_data %>%
        group_by(Grupo_Diag_Homog) %>%
        summarise(
          metric = mean(discharge == "Death", na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        arrange(desc(metric)) %>%
        slice_head(n = 8) %>%
        pull(Grupo_Diag_Homog)

      filtered_data %>%
        filter(Grupo_Diag_Homog %in% top_diseases) %>%
        group_by(ano, Grupo_Diag_Homog) %>%
        summarise(
          Value = mean(discharge == "Death", na.rm = TRUE) * 100,
          .groups = "drop"
        )

    } else {
      top_diseases <- filtered_data %>%
        group_by(Grupo_Diag_Homog) %>%
        summarise(metric = mean(los, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(metric)) %>%
        slice_head(n = 8) %>%
        pull(Grupo_Diag_Homog)

      filtered_data %>%
        filter(Grupo_Diag_Homog %in% top_diseases) %>%
        group_by(ano, Grupo_Diag_Homog) %>%
        summarise(Value = mean(los, na.rm = TRUE), .groups = "drop")
    }
  })

  output$temporal_plot <- renderPlotly({
    plot_data <- temporal_summary()

    validate(need(nrow(plot_data) > 0, "No data are available for this selection."))

    y_axis <- if (input$analysis_type == "In-hospital mortality proportion (%)") {
      list(title = metric_axis_label(input$analysis_type), range = c(0, 100))
    } else {
      max_value <- max(plot_data$Value, na.rm = TRUE)
      list(
        title = metric_axis_label(input$analysis_type),
        rangemode = "tozero",
        range = c(0, max_value * 1.1)
      )
    }

    plot_ly(
      plot_data,
      x = ~ano,
      y = ~Value,
      color = ~Grupo_Diag_Homog,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "Year: ", ano,
        "<br>Diagnostic subgroup: ", Grupo_Diag_Homog,
        "<br>", metric_axis_label(input$analysis_type), ": ", round(Value, 2)
      )
    ) %>%
      layout(
        title = paste("Annual trend —", input$gcd_select),
        xaxis = list(title = "Year", dtick = 1),
        yaxis = y_axis,
        legend = list(orientation = "h", x = 0, y = -0.18),
        margin = list(b = 140)
      )
  })

  # ----------------------------------------------------------
  # TAB 2: INTERACTIVE MAP
  # ----------------------------------------------------------

  observe({
    gcd_choices <- sort(unique(df$GCD))
    year_choices <- sort(unique(df$ano))

    updateSelectInput(
      session,
      "gcd_map",
      choices = c(all_gcd_label, gcd_choices),
      selected = all_gcd_label
    )

    updateSelectInput(
      session,
      "year_map",
      choices = year_choices,
      selected = max(year_choices, na.rm = TRUE)
    )
  })

  observeEvent(input$gcd_map, {
    req(input$gcd_map)

    diseases <- if (identical(input$gcd_map, all_gcd_label)) {
      character(0)
    } else {
      df %>%
        filter(GCD == input$gcd_map) %>%
        distinct(Grupo_Diag_Homog) %>%
        arrange(Grupo_Diag_Homog) %>%
        pull(Grupo_Diag_Homog)
    }

    updateSelectInput(
      session,
      "disease_map",
      choices = c(all_diseases_label, diseases),
      selected = all_diseases_label
    )
  }, ignoreInit = FALSE)

  map_data <- reactive({
    req(
      input$year_map,
      input$gcd_map,
      input$disease_map,
      input$analysis_type_map
    )

    filtered_data <- df %>%
      filter(ano == as.integer(input$year_map))

    if (!identical(input$gcd_map, all_gcd_label)) {
      filtered_data <- filtered_data %>%
        filter(GCD == input$gcd_map)
    }

    if (input$disease_map != all_diseases_label) {
      filtered_data <- filtered_data %>%
        filter(Grupo_Diag_Homog == input$disease_map)
    }

    if (
      input$analysis_type_map ==
        "Annual hospitalization rate per 1,000 inhabitants"
    ) {
      selected_year <- as.integer(input$year_map)

      admission_counts <- filtered_data %>%
        count(concelho.y, name = "Admissions")

      annual_population <- population_data %>%
        filter(ano == selected_year) %>%
        select(concelho.y, Population)

      rate_data <- admission_counts %>%
        left_join(
          annual_population,
          by = "concelho.y"
        ) %>%
        mutate(
          Value = 1000 * Admissions / Population
        )

      validate(
        need(
          any(is.finite(rate_data$Value)),
          paste0(
            "No municipality population values could be matched for ",
            selected_year,
            ". Check municipality names in the population CSV files."
          )
        )
      )

      rate_data %>%
        select(concelho.y, Admissions, Population, Value)

    } else if (input$analysis_type_map == "Hospital admissions") {
      filtered_data %>%
        group_by(concelho.y) %>%
        summarise(Value = n(), .groups = "drop")

    } else if (
      input$analysis_type_map ==
        "In-hospital mortality proportion (%)"
    ) {
      filtered_data %>%
        group_by(concelho.y) %>%
        summarise(
          Value = round(
            mean(discharge == "Death", na.rm = TRUE) * 100,
            2
          ),
          .groups = "drop"
        )

    } else {
      filtered_data %>%
        group_by(concelho.y) %>%
        summarise(
          Value = round(mean(los, na.rm = TRUE), 2),
          .groups = "drop"
        )
    }
  })

  output$interactive_map <- renderLeaflet({
    map_summary <- map_data()
    validate(need(nrow(map_summary) > 0, "No data are available for this selection."))

    shape_data <- pt_cont %>%
      left_join(map_summary, by = c("NAME_2" = "concelho.y"))

    domain_values <- map_summary$Value
    domain_values <- domain_values[is.finite(domain_values)]

    validate(need(length(domain_values) > 0, "No municipality-level values are available."))

    if (length(unique(domain_values)) == 1L) {
      domain_values <- c(0, domain_values[[1]])
      if (domain_values[[1]] == domain_values[[2]]) {
        domain_values <- c(0, 1)
      }
    }

    pal <- leaflet::colorNumeric(
      palette = "Blues",
      domain = domain_values,
      na.color = "#eef1f4"
    )

    disease_title <- if (
      identical(input$gcd_map, all_gcd_label)
    ) {
      "All major diagnostic categories"
    } else if (
      identical(input$disease_map, all_diseases_label)
    ) {
      paste("All diagnostic subgroups in", input$gcd_map)
    } else {
      input$disease_map
    }

    labels <- if (
      input$analysis_type_map ==
        "Annual hospitalization rate per 1,000 inhabitants"
    ) {
      sprintf(
        paste0(
          "<strong>%s</strong>",
          "<br/>Year: %s",
          "<br/>Hospital admissions: %s",
          "<br/>Population: %s",
          "<br/>Annual hospitalization rate: %s per 1,000 inhabitants"
        ),
        shape_data$NAME_2,
        input$year_map,
        ifelse(
          is.na(shape_data$Admissions),
          "No data",
          format(shape_data$Admissions, big.mark = ",", trim = TRUE)
        ),
        ifelse(
          is.na(shape_data$Population),
          "No data",
          format(shape_data$Population, big.mark = ",", trim = TRUE)
        ),
        ifelse(
          is.na(shape_data$Value),
          "No data",
          format(round(shape_data$Value, 2), nsmall = 2, trim = TRUE)
        )
      )
    } else {
      sprintf(
        "<strong>%s</strong><br/>%s: %s%s",
        shape_data$NAME_2,
        input$analysis_type_map,
        ifelse(
          is.na(shape_data$Value),
          "No data",
          format(round(shape_data$Value, 2), trim = TRUE)
        ),
        ifelse(
          is.na(shape_data$Value),
          "",
          metric_suffix(input$analysis_type_map)
        )
      )
    }

    labels <- labels %>% lapply(htmltools::HTML)

    leaflet::leaflet(shape_data) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addPolygons(
        fillColor = ~pal(Value),
        weight = 1,
        color = "white",
        fillOpacity = 0.85,
        label = labels,
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "#5f6b76",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = domain_values,
        title = paste(
          if (
            input$analysis_type_map ==
              "Annual hospitalization rate per 1,000 inhabitants"
          ) {
            "Annual hospitalization rate<br>per 1,000 inhabitants"
          } else {
            input$analysis_type_map
          },
          disease_title,
          input$year_map,
          sep = "<br>"
        ),
        opacity = 0.85
      )
  })

  # ----------------------------------------------------------
  # TAB 3: RESPIRATORY CANCER RELATIVE RISK
  # ----------------------------------------------------------

  rr_selected_data <- reactive({
    req(input$rr_outcome, input$rr_year, input$rr_month)

    rr_data %>%
      filter(
        year == as.integer(input$rr_year),
        as.character(month_lab) == input$rr_month
      ) %>%
      mutate(RR = .data[[input$rr_outcome]])
  })

  rr_persistent <- reactive({
    req(input$rr_outcome)

    rr_data %>%
      filter(year >= 2010, year <= 2017) %>%
      st_drop_geometry() %>%
      group_by(city_id, NAME_2) %>%
      summarise(
        Months = sum(is.finite(.data[[input$rr_outcome]])),
        `Minimum RR` = min(.data[[input$rr_outcome]], na.rm = TRUE),
        `Mean RR` = mean(.data[[input$rr_outcome]], na.rm = TRUE),
        `RR > 1 in all months` = all(.data[[input$rr_outcome]] > 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(`RR > 1 in all months`) %>%
      arrange(desc(`Minimum RR`))
  })

  output$rr_persistent_table <- renderDT({
    rr_persistent() %>%
      select(
        Municipality = NAME_2,
        Months,
        `Minimum RR`,
        `Mean RR`
      ) %>%
      mutate(
        `Minimum RR` = round(`Minimum RR`, 3),
        `Mean RR` = round(`Mean RR`, 3)
      )
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)

  output$rr_map <- renderLeaflet({
    selected <- rr_selected_data()

    validate(need(nrow(selected) > 0, "No relative-risk results are available."))

    map_sf <- pt_cont %>%
      left_join(
        selected %>% st_drop_geometry() %>% select(NAME_2, RR),
        by = "NAME_2"
      )

    rr_values <- map_sf$RR[is.finite(map_sf$RR)]
    validate(need(length(rr_values) > 0, "No finite relative-risk values are available."))

    upper <- max(3, stats::quantile(rr_values, 0.98, na.rm = TRUE))
    breaks <- unique(c(0, 0.6, 0.8, 1, 2, 3, upper + .Machine$double.eps))

    pal <- leaflet::colorBin(
      palette = c("#FFFFCC", "#FED976", "#FEB24C", "#FFFFFF", "#FD8D3C", "#E31A1C"),
      bins = breaks,
      domain = rr_values,
      na.color = "#eef1f4",
      right = FALSE
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>Relative risk: %s",
      map_sf$NAME_2,
      ifelse(
        is.na(map_sf$RR),
        "No data",
        format(round(map_sf$RR, 3), trim = TRUE)
      )
    ) %>% lapply(htmltools::HTML)

    leaflet(map_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(RR),
        weight = 0.8,
        color = "white",
        fillOpacity = 0.88,
        label = labels,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#5f6b76",
          fillOpacity = 0.97,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = rr_values,
        title = paste(
          rr_outcome_label(input$rr_outcome),
          input$rr_month,
          input$rr_year,
          "Relative risk",
          sep = "<br>"
        ),
        opacity = 0.9
      )
  })

  rr_time_data <- reactive({
    req(input$rr_outcome)

    persistent_names <- rr_persistent()$NAME_2
    selected_names <- input$rr_cities

    highlighted <- if (length(selected_names) > 0) {
      selected_names
    } else {
      persistent_names
    }

    rr_data %>%
      filter(year >= 2010, year <= 2017) %>%
      transmute(
        NAME_2,
        date,
        RR = .data[[input$rr_outcome]],
        Series = if_else(NAME_2 %in% highlighted, NAME_2, "Other municipalities")
      )
  })

  output$rr_time_plot <- renderPlotly({
    dat <- rr_time_data()
    validate(need(nrow(dat) > 0, "No temporal relative-risk results are available."))

    background <- dat %>% filter(Series == "Other municipalities")
    highlighted <- dat %>% filter(Series != "Other municipalities")

    p <- ggplot() +
      geom_line(
        data = background,
        aes(x = date, y = RR, group = NAME_2),
        color = "grey80",
        linewidth = 0.25,
        alpha = 0.35
      ) +
      geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.6) +
      geom_line(
        data = highlighted,
        aes(
          x = date,
          y = RR,
          color = Series,
          group = NAME_2,
          text = paste0(
            "Municipality: ", NAME_2,
            "<br>Date: ", format(date, "%b %Y"),
            "<br>RR: ", round(RR, 3)
          )
        ),
        linewidth = 1
      ) +
      labs(
        x = "Month",
        y = "Posterior mean relative risk",
        color = "Municipality"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold")
      )

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h"))
  })


  rr_density_data <- reactive({
    req(
      input$rr_outcome,
      input$rr_density_city,
      input$rr_density_year,
      input$rr_density_month
    )

    suffix <- sub("RR", "", input$rr_outcome)
    pp_col <- paste0("PP", suffix)
    density_col <- paste0("density", suffix)

    dat <- rr_data %>%
      filter(
        NAME_2 == input$rr_density_city,
        year == as.integer(input$rr_density_year)
      )

    if (input$rr_density_month != "All months") {
      dat <- dat %>%
        filter(as.character(month_lab) == input$rr_density_month)
    }

    dat <- dat %>%
      filter(.data[[pp_col]] >= 0.90)

    validate(
      need(
        nrow(dat) > 0,
        "No months meet P(RR > 1 | y) ≥ 0.90 for this selection."
      )
    )

    density_frames <- lapply(seq_len(nrow(dat)), function(i) {
      dens <- dat[[density_col]][[i]]

      if (is.null(dens) || nrow(dens) == 0) {
        return(NULL)
      }

      dens %>%
        transmute(
          x = x,
          y = y,
          month = format(dat$date[i], "%b %Y"),
          PP_RR_gt1 = dat[[pp_col]][i]
        )
    })

    bind_rows(density_frames) %>%
      mutate(
        month = factor(
          month,
          levels = format(sort(unique(dat$date)), "%b %Y")
        )
      )
  })

  output$rr_density_plot <- renderPlot({
    dens_df <- rr_density_data()

    validate(
      need(
        nrow(dens_df) > 0,
        "No posterior density is available for this selection."
      )
    )

    dens_area <- dens_df %>%
      filter(x > 1)

    n_months <- nlevels(droplevels(dens_df$month))
    facet_cols <- if (n_months <= 3) n_months else if (n_months <= 6) 3 else 4

    ggplot(dens_df, aes(x = x, y = y)) +
      geom_area(
        data = dens_area,
        alpha = 0.35
      ) +
      geom_line(linewidth = 0.55) +
      geom_vline(
        xintercept = 1,
        linetype = 2,
        linewidth = 0.5
      ) +
      facet_wrap(
        ~month,
        ncol = facet_cols,
        scales = "free_y"
      ) +
      labs(
        subtitle = paste(
          rr_outcome_label(input$rr_outcome),
          "—",
          input$rr_density_city,
          input$rr_density_year
        ),
        x = "Relative risk",
        y = "Posterior density"
      ) +
      theme_bw(base_size = 11) +
      theme(
        strip.text = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 11, face = "bold"),
        panel.grid.minor = element_blank()
      )
  })

  # ----------------------------------------------------------
  # TAB 4: HAZARD PREDICTIONS
  # ----------------------------------------------------------

  patient_info <- reactive({
    tibble(
      id_pat = as.numeric(rownames(test)),
      Y1 = test$Y1,
      Y2 = test$Y2
    ) %>%
      mutate(
        outcome = if_else(Y1 == 1, "death", "discharge"),
        severity = if_else(Y2 == 1, "high severity", "low severity"),
        label = paste0(
          "Patient ", id_pat,
          " — ", severity,
          " and ", outcome
        )
      ) %>%
      semi_join(
        df_resultados %>% distinct(id_pat),
        by = "id_pat"
      )
  })

  filtered_patients <- reactive({
    info <- patient_info()

    if (input$outcome_filter != "All") {
      info <- info %>%
        filter(outcome == input$outcome_filter)
    }

    if (input$severity_filter != "All") {
      info <- info %>%
        filter(severity == input$severity_filter)
    }

    info
  })

  observeEvent(
    list(input$outcome_filter, input$severity_filter),
    {
      filtered_ids <- filtered_patients()
      current_selected <- input$ids

      choices_filtered <- setNames(
        as.character(filtered_ids$id_pat),
        filtered_ids$label
      )

      selected_labels <- patient_info() %>%
        filter(as.character(id_pat) %in% current_selected)

      choices_selected <- setNames(
        as.character(selected_labels$id_pat),
        selected_labels$label
      )

      choices <- c(choices_selected, choices_filtered)
      choices <- choices[!duplicated(names(choices))]

      updatePickerInput(
        session,
        inputId = "ids",
        choices = choices,
        selected = current_selected
      )
    },
    ignoreInit = FALSE
  )

  hazard_data <- reactive({
    req(input$ids)

    bind_rows(
      lapply(
        as.numeric(input$ids),
        prepare_patient
      )
    )
  })

  output$hazard_plot <- renderPlot({
    plot_data <- hazard_data()

    validate(need(nrow(plot_data) > 0, "Select at least one patient."))

    y_values <- exp(c(
      plot_data$IC_lower,
      plot_data$IC_upper,
      plot_data$Value
    ))

    y_values <- y_values[is.finite(y_values)]

    validate(need(length(y_values) > 0, "No finite hazard predictions are available."))

    ggplot(plot_data, aes(x = time * 365.25)) +
      geom_line(
        aes(y = exp(Value), color = Model),
        linewidth = 1
      ) +
      geom_line(
        aes(y = exp(IC_lower), color = Model),
        linetype = "dashed",
        linewidth = 0.5
      ) +
      geom_line(
        aes(y = exp(IC_upper), color = Model),
        linetype = "dashed",
        linewidth = 0.5
      ) +
      facet_wrap(
        ~patient,
        ncol = 2,
        scales = "free_x"
      ) +
      scale_y_continuous(
        limits = range(y_values, na.rm = TRUE),
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      labs(
        x = "Time (days)",
        y = "Predicted hazard",
        color = "Model"
      ) +
      scale_color_manual(
        values = c(
          "Model IX" = "black",
          "Model III" = "gray70"
        )
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", color = "white"),
        strip.background = element_rect(fill = "#243447", color = "#243447"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "#243447")
      )
  })
}

shinyApp(ui, server)
