shinyUI(bootstrapPage(
  fluidPage(
    theme = "bootstrap.css",
    titlePanel(
      "Using SEIR models to estimate the impacts of public health interventions on COVID-19 dynamics."
    ),
    br(),
    sidebarLayout(
      sidebarPanel(
        width = 3,

        tabsetPanel(
          tabPanel(
            "Data Setup",
            br(),
            h4("Option 1: Use Existing Data"),
            # -----
            selectInput("county", "Select County:",
              choices = tt2,
              selected = "Wake"
            ),
            br(),
            h4("Option 2: Upload Data"),
            # ---
            fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),

            numericInput("population",
              "Specify Population (only used if uploading data):", 1023811,
              min = 1
            ),

            br(),
            downloadButton(
              "download_template",
              "Download Data Template (Wake County)"
            ),
          ),

          tabPanel(
            "Scenario Settings",
            br(),
            # -----
            selectInput(
              "beta",
              label = tags$span(
                "Select β Value:",
                tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  style = "color:#0072B2;",
                  title = "β is the disease transmission parameter of an SEIR model and can be modified to match the predicted impact of infection control policies. You can select for the model with β calculated based on the selected county's data, which changes over time, or with different static values."
                )
              ),
              choices = list(
                "Static β: .225" = .225,
                "Static β: .25" = .25,
                "Static β: .275" = .275,
                "Calculated β from COVID-19 Epidemic Curve" = 1
              ),
              selected = 1
            ),

            # -----
            sliderInput("cm_slider_start",
              label = tags$span(
                "Case Multiplier - Start (Days 1-90)",
                tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  style = "color:#0072B2;",
                  title = "Use this to estimate the number of unreported cases and infections in your data. This estimate is important as it modifies the proportion susceptible at every time point. The best way to estimate the case multiplier is through a longitudinal serology study, comparing the number of SARS-CoV-2 seroconverted individuals to the number of reported cases at different periods of time. For many places, the case multiplier needed for accurate estimates is higher at the beginning of the epidemic and has decreased over time as testing has become more readily available."
                )
              ),
              min = 1, max = 12, value = 10
            ),
            sliderInput(
              "cm_slider_end",
              label = tags$span(
                "Case Multiplier - End (Day 270+)",
                tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  style = "color:#0072B2;",
                  title = "For this scenario, the starting case multiplier is used for the first 90 days after the first reported case. The multiplier then linear drops off from the start value to the end value over the next six months. Finally, any day after day 270 will use the ending case multiplier."
                )
              ),
              min = 1, max = 12, value = 4
            ),

            numericInput("modification_length", "Modification Length (in days)",
              min = 1, max = 60, value = 30
            ),
            sliderInput(
              "modification_multiplier",
              label = tags$span(
                "Modification Intensity",
                tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  style = "color:#0072B2;",
                  title = "This is the value by which β will be multiplied, and equates to the intensity of the modification to disease transmission. An intensity of 1 is no change to baseline. An intensity of 2 will double β. An intensity of 0.5 will reduce β by 50%."
                )
              ),
              min = .1, max = 3, value = .90
            ),
            sliderInput(
              "modification_day1",
              "Scenario 1 Start Date (days since first reported infection)",
              min = 1, max = 400, value = 30
            ),

            conditionalPanel(
              condition = "input.tabs1 != 'Scenario Analysis'",
              sliderInput(
                "modification_day2",
                "Scenario 2 Start Date (days since first reported infection)",
                min = 1, max = 400, value = 100
              )
            ),

            sliderInput(
              "add_days",
              "Projection Length (days to run model after last reported case)",
              min = 0, max = 120, value = 30
            ),

            br(),
            h3("Download Data:"),
            selectInput(
              "download_file",
              "Select Scenario:",
              choices = list(
                "Baseline" = "baseline",
                "Scenario 1" = "scenario1",
                "Scenario 2" = "scenario2"
              ),
              selected = "baseline"
            ),
            downloadButton("download_data", "Download Data"),
            downloadButton("download_parameters", "Download Parameters")
          )
        )
      ),
      # -------------------------------------------------------------------
      mainPanel(
        br(),
        tabsetPanel(
          id = "tabs1",
          tabPanel(
            "Scenario Plots",
            fluidRow(
              column(10, plotlyOutput("main_plot")),
              column(
                2, selectInput("main_plot_type", "View Plot:",
                  choices = list(
                    "Cases" = "cases",
                    "Cumulative Cases" = "cumulative_cases",
                    "Infections" = "infections",
                    "Cumulative Infections" = "cumulative_infections"
                  )
                )
              )
            ),
            fluidRow(
              column(10, plotlyOutput("secondary_plot")),
              column(
                2, selectInput("secondary_plot_type", "View Plot:",
                  choices = list(
                    "β Values" = "beta",
                    "Susceptibility" = "susceptible",
                    "Susceptible * Infected" = "si"
                  )
                )
              )
            )
          ),

          tabPanel(
            "Scenario Analysis",
            p(text1),
            fluidRow(
              column(9, plotlyOutput("reduction_graphic", height = 600)),
              column(
                3, selectInput("y_axis", "Main Outcome of Interest:",
                  choices = list(
                    "Cumulative Cases" = "cases_reduction",
                    "Cumulative Infections" = "infections_reduction",
                    "Number of cases at highest peak" = "peak_reduction_cases",
                    "Number of infections at highest peak" =
                      "peak_reduction_infections"
                  ),
                  selected = "infections_reduction"
                ),
                selectInput("z_axis", "Z-Axis",
                  choices = list(
                    "β Value" = "beta",
                    "Susceptible Proportion" = "susceptible",
                    "Susceptible * Infectious" = "si"
                  ),
                  selected = "susceptible"
                )
              )
            ),
            htmlOutput("reduction_text")
          ),

          tabPanel(
            "Project Abstract",
            "Public health interventions have the potential to delay, decrease, or unintentionally increase disease transmission. Restrictive policies, e.g., shelter-in-place, pose limitations and should be reserved for their greatest potential impact. The theoretical impacts of start day, length, and intensity of interventions on disease transmission were estimated and illustrated on COVID-19 dynamics in Wake County, North Carolina.
A SEIR model was used to estimate epidemic curves with modifications to the disease transmission parameter (β). Modifications were designed to simulate events/interventions likely to increase (long weekends, holiday seasons) or to decrease (shelter-in-place, mask mandates) transmission.  The resultant curves’ shape, timing and cumulative case count was compared to baseline and across other modified curves.  
Changes to COVID-19 dynamics, including moving the epidemic’s peak location, height, and width and flattening the curve, were observed. In addition to the length and intensity of modifications, both the case count and proportion susceptible, at its start day, influenced the modification’s impact. Modifications at early phases of the epidemic shifted the curve, and those near the peak modified chape and case count. Those applied when there were minimal cases or <50% of the population was susceptible, showed minimal impact. In in some β decrease models, the cumulative count increased over baseline. This model was developed into a public tool, whereby users can modify the parameters and estimate impacts of different interventions on transmission. This tool could aid in evaluating public health intervention options, prior to their use, and predicting case increases from specific events. 
"          )
        ),
      br(),
      h5("Disclaimer:"),
      br(),
      "Code for this work is publicly available", tags$a(href="https://github.com/RTIInternational/nc-facility-abm", "here."),
      br(),
      br(),
      "This material is based upon work supported by the National Science Foundation under Grant Number 2027802",
      br(),
      br(),
      "Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.",
      br(),
      br()
      )
    )
  )
))