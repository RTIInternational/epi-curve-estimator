library(plotly)

vline <- function(x = 0, color = "grey") {
    list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = "dot", width = 1)
    )
}

# -----------------------------------------------------------------------------
count_graphic <-
    function(df, baseline, s1, s2, type = "Infections") {
        add_days <- sum(is.na(df$Day))
        if (add_days > 0) {
            shapes <- list(vline(max(df$Date) - add_days))
        } else {
            shapes <- list()
        }
        fig <-
            plot_ly(
                df,
                x = ~Date,
                y = s1,
                name = "Scenario 1",
                type = "scatter",
                mode = "lines",
                line = list(color = "#e41a1c")
            ) %>%
            add_trace(
                y = s2, name = "Scenario 2",
                line = list(color = "#4daf4a")
            ) %>%
            add_trace(
                y = baseline, name = "Baseline",
                line = list(color = "#000000")
            ) %>%
            config(displayModeBar = F) %>%
            layout(
                yaxis = list(title = type),
                title = list(
                    text = paste("Estimated", type, "by Scenario"),
                    y = .975
                ),
                shapes = shapes
            )
        fig
    }


# -----------------------------------------------------------------------------
proportion_graphic <-
    function(df, baseline, s1, s2, title = "Proportion Susceptible") {
        add_days <- sum(is.na(df$Day))
        if (add_days > 0) {
            shapes <- list(vline(max(df$Date) - add_days))
        } else {
            shapes <- list()
        }

        fig <-
            plot_ly(
                df,
                x = ~Date,
                y = s1,
                name = "Scenario 1",
                type = "scatter",
                mode = "lines",
                line = list(color = "#e41a1c")
            ) %>%
            add_trace(
                y = s2, name = "Scenario 2",
                line = list(color = "#4daf4a")
            ) %>%
            add_trace(
                y = baseline, name = "Baseline",
                line = list(color = "#000000")
            ) %>%
            config(displayModeBar = F) %>%
            layout(
                yaxis = list(title = title),
                title = list(
                    text = paste(title, "by Scenario"),
                    y = .975
                ),
                shapes = shapes
            )
        fig
    }

# -----------------------------------------------------------------------------
bar_graph <- function(m_ci, s1_ci, s2_ci, type = "Infections") {
    fig <- plot_ly(
        x = c("Baseline", "Scenario 1", "Scenario 2"),
        y = c(m_ci, s1_ci, s2_ci),
        name = "Comparing Model Output",
        type = "bar"
    ) %>%
        config(displayModeBar = F) %>%
        layout(
            yaxis = list(title = paste("Total", type)),
            title = list(text = "Comparing Model Outputs", y = .975)
        )
    fig
}

# -----------------------------------------------------------------------------
reduction_graphic <- function(analysis_df, y, z) {
    xx <- list(title = "Start Day of Modification (x)")
    y1 <- analysis_df[[y]]
    z1 <- analysis_df[[z]]
    if (y == "cases_reduction") {
        yx <- list(title = "Change in Cases<sup>*</sup> (y)")
    } else if (y == "peak_reduction_cases") {
        yx <- list(title = "Change in Peak Cases<sup>*</sup> (y)")
    } else if (y == "infections_reduction") {
        yx <- list(title = "Change in Infections<sup>*</sup> (y)")
    } else if (y == "peak_reduction_infections") {
        yx <- list(title = "Change in Peak Infections<sup>*</sup> (y)")
    }
    if (z == "susceptible") {
        zx <- list(title = "Proportion Susceptible (z)")
    } else if (z == "si") {
        zx <- list(title = "Susceptible * Infectious (z)")
    } else if (z == "beta") {
        zx <- list(title = "Beta (z)")
    }
    fig <- plot_ly(analysis_df,
        x = ~day, y = y1, z = z1, type = "scatter3d", mode = "lines",
        opacity = 1, line = list(width = 8, reverscale = FALSE)
    ) %>%
        layout(scene = list(xaxis = xx, yaxis = yx, zaxis = zx)) %>%
        config(displayModeBar = F)
    fig
}