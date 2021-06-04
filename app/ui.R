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
        # -----
        selectInput("county", "Select County:",
          choices = tt2,
          selected = "Wake"
        ),

        # -----
        selectInput(
          "beta",
          "Select β Value:",
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
          label = h6("Case Multiplier - Start"),
          min = 1, max = 12, value = 10
        ),
        sliderInput(
          "cm_slider_end",
          label = h6("Case Multiplier - End"),
          min = 1, max = 12, value = 4
        ),

        br(),
        # -----
        h3("Scenario Settings:"),
        numericInput("modification_length", "Modification Length (in days)",
          min = 1, max = 60, value = 30
        ),
        sliderInput(
          "modification_multiplier", "Modification Intensity",
          min = .1, max = 3, value = .90
        ),
        sliderInput(
          "modification_day1", "Scenario 1 Start Date (days since first reported infection)",
          min = 1, max = 400, value = 30
        ),
        sliderInput(
          "modification_day2", "Scenario 2 Start Date (days since first reported infection)",
          min = 1, max = 400, value = 100
        ),
        sliderInput(
          "add_days", "Projection Length (days to run model after last reported case)",
          min = 0, max = 120, value = 30
        ),

        br(),
        h3("Download Data:"),
        selectInput(
          "download_file",
          "Select Scenario:",
          choices = list("Baseline" = "baseline", "Scenario 1" = "scenario1", "Scenario 2" = "scenario2"
          ),
          selected = "baseline"
        ),
        downloadButton('downloadData', 'Download Data'),
        downloadButton('downloadData2', 'Download Parameters')
      ),
      # -------------------------------------------------------------------
      mainPanel(
        br(),
        tabsetPanel(
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
                3, selectInput("y_axis", "Reduction Type:",
                  choices = list(
                    "Cases" = "cases_reduction",
                    "Infections" = "infections_reduction",
                    "Peak Cases" = "peak_reduction_cases",
                    "Peak Infections" = "peak_reduction_infections"
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
"
          )
        )
      )
    )
  )
))