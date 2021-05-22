shinyUI(bootstrapPage(
  fluidPage(
    theme = "bootstrap.css",
    navbarPage(
      "Estimating COVID-19 Cases Using SEIR Models",
      tabPanel("Scenario Modeling", sidebarLayout(
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
            "Select Beta shape:",
            choices = list(
              "Static Beta: .225" = .225,
              "Static Beta: .25" = .25,
              "Static Beta: .275" = .275,
              "Calculated Beta from COVID-19 Pandemic" = 1
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
          numericInput("shock_length", "Shock Length (In Days)?",
            min = 1, max = 60, value = 30
          ),
          sliderInput(
            "shock_multiplier", "Shock Severity",
            min = .1, max = 3, value = .90
          ),
          sliderInput(
            "shock_day1", "Scenario 1 Start Date",
            min = 1, max = 400, value = 30
          ),
          sliderInput(
            "shock_day2", "Scenario 2 Start Date",
            min = 1, max = 400, value = 100
          )
        ),
        # -------------------------------------------------------------------
        mainPanel(
          p("Estimated: Estimated number of infections for baseline model"),
          p("Scenario: The number of infections/cases modeled by the
        calibrated SEIR model given the selected scenario parameters."),
          br(), br(),
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
                      "Beta" = "beta",
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
                      "Beta Value" = "beta",
                      "Susceptible Proportion" = "susceptible",
                      "Susceptible * Infectious" = "si"
                    ),
                    selected = "susceptible"
                  )
                )
              ),
              htmlOutput("reduction_text")
            )
          )
        )
      ))
    )
  )
))