shinyServer(function(input, output, session) {
  cv <- reactive({
    cv <- calc_beta(
      county = input$county,
      cm_end = input$cm_slider_end,
      cm_start = input$cm_slider_start
    )
    if (input$beta != 1) {
      cv$data$beta <- as.numeric(input$beta)
    }
    cv
  })
  # ---------------------------------------------------------------------------
  # ----- Download
  # ---------------------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$download_file == "baseline") {
      }
      paste0(input$county, "-", input$download_file, "-", Sys.Date(), '.csv')
    },
    content = function(con) {
      if (input$download_file == "baseline") {
        data = model_default()
      } else if(input$download_file == "scenario1") {
        data = scenario1_df()
      } else {
        data = scenario2_df()
      }
      write.csv(data, con)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0(tolower(input$county), "-", input$download_file, "-", "meta_data", '.txt')
    },
    content = function(con) {
      a <- c(paste0("County: ", input$county))
      a <- c(a, paste0("Beta: ", ifelse(input$beta==1, "Beta Calculated From Epi Curve", input$beta)))
      a <- c(a, paste0("Case Multiplier Value 1: ", input$cm_slider_start))
      a <- c(a, paste0("Case Multiplier Value 2: ", input$cm_slider_end))
      a <- c(a, paste0("Modification Intensity: ", input$modification_multiplier))
      a <- c(a, paste0("Scenario 1 Modification Day: ", input$modification_day1))
      a <- c(a, paste0("Scenario 1 Modification Length: ", input$modification_length1))
      a <- c(a, paste0("Scenario 2 Modification Day: ", input$modification_day2))
      a <- c(a, paste0("Scenario 2 Modification Length: ", input$modification_length2))
      a <- c(a, paste0("Days Projected: ", input$add_days))
      write.csv(c(1, 2, 3), "test.csv")
      writeLines(a, con)
    }
  )

  # ---------------------------------------------------------------------------
  # ----- Scenarios
  # ---------------------------------------------------------------------------
  model_default <- reactive({
    run_scenario(cv(), 1, 1, 1, add_days = input$add_days)
  })
  scenario1_df <- reactive({
    start <- input$modification_day1
    length <- input$modification_length
    sm <- input$modification_multiplier
    run_scenario(cv(), start, length, mod_multiplier = sm, add_days = input$add_days)
  })
  scenario2_df <- reactive({
    start <- input$modification_day2
    length <- input$modification_length
    sm <- input$modification_multiplier
    run_scenario(cv(), start, length, mod_multiplier = sm, add_days = input$add_days)
  })

  # ---------------------------------------------------------------------------
  # ----- Analysis
  # ---------------------------------------------------------------------------
  analysis_df <- reactive({
    length <- input$modification_length
    days <- 370 - length
    sm <- input$modification_multiplier
    prepare_analysis_df(cv(), days, length, sm)
  })

  output$reduction_graphic <- renderPlotly({
    reduction_graphic(analysis_df(), input$y_axis, input$z_axis)
  })

  output$reduction_text <- renderUI({
    a_df <- analysis_df()
    day <- which.max(a_df[[input$y_axis]])
    value <- round(max(a_df[[input$y_axis]]), 2)
    value <- format(value, nsmall = 1, big.mark = ",")
    s1 <- paste("Optimal day to start the modification: Day ", day)
    s2 <- paste("Reducing selected metric by a value of:", value)
    HTML(paste(s1, s2, sep = "<br/>"))
  })

  # ---------------------------------------------------------------------------
  # ----- Line Graphics
  # ---------------------------------------------------------------------------
  output$main_plot <- renderPlotly({
    df <- model_default()
    if (input$main_plot_type == "infections") {
      p <- count_graphic(df,
        df$scenario_infections,
        scenario1_df()$scenario_infections,
        scenario2_df()$scenario_infections,
        type = "Infections"
      )
    } else if (input$main_plot_type == "cumulative_infections") {
      p <- count_graphic(df,
        df$scenario_infections_cum,
        scenario1_df()$scenario_infections_cum,
        scenario2_df()$scenario_infections_cum,
        type = "Cumulative Infections"
      )
    } else if (input$main_plot_type == "cumulative_cases") {
      p <- count_graphic(df,
        df$scenario_cases_cum,
        scenario1_df()$scenario_cases_cum,
        scenario2_df()$scenario_cases_cum,
        type = "Cumulative Cases"
      )
    } else {
      p <- count_graphic(df,
        df$scenario_cases,
        scenario1_df()$scenario_cases,
        scenario2_df()$scenario_cases,
        type = "Cases"
      )
    }
    p
  })
  output$secondary_plot <- renderPlotly({
    df <- model_default()
    if (input$secondary_plot_type == "susceptible") {
      s1 <- scenario1_df()$susceptible
      s2 <- scenario2_df()$susceptible
      p <- proportion_graphic(df, df$susceptible, s1, s2)
    } else if (input$secondary_plot_type == "si") {
      s1 <- scenario1_df()$si
      s2 <- scenario2_df()$si
      p <- proportion_graphic(
        df, df$si, s1, s2,
        "Susceptible * Infection"
      )
    } else {
      s1 <- scenario1_df()$beta
      s2 <- scenario2_df()$beta
      p <- proportion_graphic(df, df$beta, s1, s2, "Estimated Beta Value")
    }
    p
  })
  # Cases
  output$cases_plot <- renderPlotly({
    df <- model_default()
    count_graphic(df,
      df$scenario_cases,
      scenario1_df()$scenario_cases,
      scenario2_df()$scenario_cases,
      type = "Cases"
    )
  })


  # ---------------------------------------------------------------------------
  # ----- Bar Graphics
  # ---------------------------------------------------------------------------
  output$bar_graph_infections <- renderPlotly({
    df <- model_default()
    s1 <- scenario1_df()
    s2 <- scenario2_df()
    df_ci <- tail(cumsum(df$scenario_infections), 1)
    s1_ci <- tail(cumsum(s1$scenario_infections), 1)
    s2_ci <- tail(cumsum(s2$scenario_infections), 1)
    bar_graph(df_ci, s1_ci, s2_ci, "Infections")
  })
  output$bar_graph_cases <- renderPlotly({
    s1 <- scenario1_df()
    s2 <- scenario2_df()
    r_ci <- tail(cumsum(s1$cases), 1)
    m_ci <- tail(cumsum(s1$pred_cases), 1)
    s1_ci <- tail(cumsum(s1$scenario_cases), 1)
    s2_ci <- tail(cumsum(s2$scenario_cases), 1)
    bar_graph(r_ci, m_ci, s1_ci, s2_ci, "Cases")
  })
})