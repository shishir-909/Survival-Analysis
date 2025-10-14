library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(evd)
library(goftest)
library(promises)
library(future)

# Set up future for async processing
plan(multisession)

# Define UI
ui <- page_navbar(
  title = "Extreme Value Analysis",
  theme = bs_theme(version = 5),

  # Input Tab
  nav_panel(
    "Input",
    layout_columns(
      col_widths = c(6, 6),

      # Left side - Data Input (full height)
      card(
        card_header("Data Input"),
        # File upload option
        fileInput(
          "wall_thickness_file",
          "Upload Wall Thickness Data (.txt file):",
          accept = ".txt"
        ),
        # OR text input option
        div(
          style = "margin-top: 10px; margin-bottom: 10px; text-align: center;",
          strong("OR")
        ),
        textAreaInput(
          "wall_thickness_data",
          "Minimum Wall Thickness Values (one per line):",
          height = "100px",
          placeholder = "Enter minimum wall thickness values, one per line..."
        ),
        numericInput(
          "nominal_thickness",
          "Nominal Wall Thickness:",
          value = 0.095,
          min = 0,
          step = 0.001
        ),
        numericInput(
          "n_tubes",
          "Total Number of Tubes (N):",
          value = 1000,
          min = 1,
          step = 1
        ),
        dateInput(
          "start_operation",
          "Start of Operation:",
          value = Sys.Date() - 365
        ),
        dateInput(
          "inspection_date",
          "Inspection Date:",
          value = Sys.Date()
        ),
        actionButton(
          "analyze",
          "Run Analysis",
          class = "btn-primary"
        )
      ),

      # Right side - Data Preview on top, Histogram on bottom
      div(
        card(
          card_header("Data Preview"),
          DTOutput("data_preview")
        ),

        br(),

        card(
          card_header("Wall Thickness Histogram"),
          plotOutput("thickness_histogram", height = "400px")
        )
      )
    )
  ),

  # Results Tab
  nav_panel(
    "Results",
    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Gumbel Distribution Fit"),
        verbatimTextOutput("gumbel_fit")
      ),

      card(
        card_header("Minimum Wall Thickness Estimate"),
        verbatimTextOutput("min_thickness_estimate")
      )
    ),

    br(),

    card(
      card_header("Goodness of Fit Tests"),
      layout_columns(
        col_widths = c(6, 6),

        div(
          h5("Kolmogorov-Smirnov Test"),
          verbatimTextOutput("ks_test")
        ),

        div(
          h5("Anderson-Darling Test"),
          verbatimTextOutput("ad_test")
        )
      )
    )
  ),

  # Plots Tab
  nav_panel(
    "Plots",
    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Probability Plot"),
        plotOutput("prob_plot", height = "400px")
      ),

      card(
        card_header("Cumulative Probability Plot"),
        plotOutput("cdf_plot", height = "400px")
      ),

      card(
        card_header("Quantile Plot"),
        plotOutput("quantile_plot", height = "400px")
      ),

      card(
        card_header("Exceedance Probability Plot"),
        plotOutput("exceedance_plot", height = "400px")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values to store analysis results
  values <- reactiveValues(
    wall_thickness = NULL,
    max_wall_loss = NULL,
    fit = NULL,
    plot_data = NULL,
    plot_data2 = NULL,
    loc = NULL,
    scale = NULL,
    n = NULL,
    x_N = NULL,
    se_x_N = NULL,
    bootstrap_complete = FALSE
  )

  # Reactive expression to get wall thickness data from either file or text input
  get_wall_thickness_data <- reactive({
    # Priority: file upload first, then manual text input
    if (!is.null(input$wall_thickness_file)) {
      file_path <- input$wall_thickness_file$datapath
      if (!is.null(file_path) && file.exists(file_path)) {
        data_lines <- readLines(file_path, warn = FALSE)
        data_lines <- data_lines[data_lines != ""]
        wall_thickness <- suppressWarnings(as.numeric(data_lines))
        wall_thickness <- wall_thickness[!is.na(wall_thickness)]
        return(wall_thickness)
      }
    }

    # Fall back to manual text input
    if (
      !is.null(input$wall_thickness_data) &&
        trimws(input$wall_thickness_data) != ""
    ) {
      data_text <- trimws(input$wall_thickness_data)
      data_lines <- strsplit(data_text, "\n")[[1]]
      data_lines <- data_lines[data_lines != ""]
      wall_thickness <- suppressWarnings(as.numeric(data_lines))
      wall_thickness <- wall_thickness[!is.na(wall_thickness)]
      return(wall_thickness)
    }

    return(NULL)
  })

  # Data preview
  output$data_preview <- renderDT({
    wall_thickness <- get_wall_thickness_data()

    if (is.null(wall_thickness) || length(wall_thickness) == 0) {
      return(NULL)
    }

    data.frame(
      Index = 1:length(wall_thickness),
      `Wall Thickness` = wall_thickness
    ) %>%
      datatable(options = list(pageLength = 10, scrollY = "200px"))
  })

  # Wall thickness histogram
  output$thickness_histogram <- renderPlot({
    wall_thickness <- get_wall_thickness_data()

    if (is.null(wall_thickness) || length(wall_thickness) == 0) {
      return(NULL)
    }

    req(input$nominal_thickness)

    ggplot(
      data.frame(min_wall_thickness = wall_thickness),
      aes(x = min_wall_thickness)
    ) +
      geom_histogram(
        binwidth = 0.002,
        fill = "lightblue",
        color = "black",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = input$nominal_thickness,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = input$nominal_thickness + 0.002,
        y = max(table(cut(
          wall_thickness,
          breaks = seq(min(wall_thickness), max(wall_thickness), by = 0.002)
        ))) *
          0.8,
        label = "Nominal Thickness",
        color = "red",
        angle = 90,
        vjust = -0.5,
        size = 6
      ) +
      annotate(
        "text",
        x = input$nominal_thickness - 0.002,
        y = max(table(cut(
          wall_thickness,
          breaks = seq(min(wall_thickness), max(wall_thickness), by = 0.002)
        ))) *
          0.95,
        label = paste("Tubes sampled:", length(wall_thickness)),
        color = "black",
        hjust = 1,
        size = 6
      ) +
      annotate(
        "text",
        x = input$nominal_thickness - 0.002,
        y = max(table(cut(
          wall_thickness,
          breaks = seq(min(wall_thickness), max(wall_thickness), by = 0.002)
        ))) *
          0.85,
        label = paste("Total tubes:", input$n_tubes),
        color = "black",
        hjust = 1,
        size = 6
      ) +
      labs(
        title = "Histogram of Minimum Wall Thickness",
        x = "Minimum Wall Thickness (inches)",
        y = "Frequency"
      ) +
      theme_minimal()
  })

  # Main analysis
  observeEvent(input$analyze, {
    req(input$nominal_thickness, input$n_tubes)

    # Get wall thickness data from reactive expression
    wall_thickness <- get_wall_thickness_data()

    if (is.null(wall_thickness) || length(wall_thickness) == 0) {
      showNotification(
        "Please enter or upload wall thickness data",
        type = "error"
      )
      return()
    }

    if (length(wall_thickness) < 3) {
      showNotification(
        "Please provide at least 3 valid wall thickness values",
        type = "error"
      )
      return()
    }

    if (length(wall_thickness) > input$n_tubes) {
      showNotification(
        "Number of readings cannot be greater than the total number of tubes in the heat exchanger. Please check Total Number of Tubes (N) in your input data.",
        type = "error"
      )
      return()
    }

    if (any(wall_thickness > input$nominal_thickness)) {
      showNotification(
        "Measured minimum wall thickness cannot be greater than nominal thickness. Please check your input data.",
        type = "error"
      )
      return()
    }

    # Calculate max wall loss
    max_wall_loss <- input$nominal_thickness - wall_thickness
    n <- length(max_wall_loss)
    N <- input$n_tubes

    # Fit Gumbel distribution
    fit <- try(fgev(max_wall_loss, shape = 0), silent = TRUE)

    if (inherits(fit, "try-error")) {
      showNotification("Failed to fit Gumbel distribution", type = "error")
      return()
    }

    loc <- as.numeric(fit$estimate[1])
    scale <- as.numeric(fit$estimate[2])

    # Calculate return level
    x_N <- scale * (-log(-log(1 - (1 / N)))) + loc

    # Calculate standard error using expected information matrix (Dr Wang's paper)
    se_x_N <- scale *
      sqrt(
        (1.109 +
          0.514 * (-log(-log(1 - (1 / N)))) +
          (0.608 * ((-log(-log(1 - (1 / N))))**2))) /
          n
      )

    # Create plot data
    plot_data <- data.frame(
      order_stat = 1:n,
      max_wall_loss = sort(max_wall_loss),
      emp_cdf = (1:n) / (n + 1),
      theo_cdf = pgumbel(sort(max_wall_loss), loc = loc, scale = scale)
    ) %>%
      mutate(
        emp_cdf_betaMedian = qbeta(0.5, order_stat, n - order_stat + 1),
        theo_quantile_betaMedian = qgumbel(
          emp_cdf_betaMedian,
          loc = loc,
          scale = scale
        )
      )

    plot_data2 <- data.frame(
      max_wall_loss = seq(0, max(max_wall_loss) * 1.5, length.out = 100),
      theo_cdf = pgumbel(
        seq(0, max(max_wall_loss) * 1.5, length.out = 100),
        loc = loc,
        scale = scale
      )
    )

    # Store initial results
    values$wall_thickness <- wall_thickness
    values$max_wall_loss <- max_wall_loss
    values$fit <- fit
    values$plot_data <- plot_data
    values$plot_data2 <- plot_data2
    values$loc <- loc
    values$scale <- scale
    values$n <- n
    values$x_N <- x_N
    values$se_x_N <- se_x_N
    values$bootstrap_complete <- FALSE

    showNotification(
      "Analysis completed! Computing confidence intervals...",
      type = "message"
    )

    # Start bootstrap simulation in background
    future_promise({
      # Simulation based bootstrap confidence intervals
      n_boot <- 1000
      all_cdfs <- list()
      all_vals <- list()

      for (i in 1:n_boot) {
        # Simulate data
        sim_data <- rgev(n, loc = loc, scale = scale, shape = 0)

        # Order statistics of the simulated data
        sim_data_sorted <- sort(sim_data)

        all_cdfs[[i]] <- evd::pgev(
          sim_data_sorted,
          loc = loc,
          scale = scale,
          shape = 0
        )

        all_vals[[i]] <- sim_data_sorted
      }

      # Combine the CDFs into a matrix for easier calculation
      cdf_matrix <- do.call(cbind, all_cdfs) %>%
        as.data.frame()

      # Combine the sorted values into a matrix for easier calculation
      vals_matrix <- do.call(cbind, all_vals) %>%
        as.data.frame()

      # Calculate the 95% confidence interval
      cdf_lower_bootstrap <- apply(cdf_matrix, 1, quantile, probs = 0.025)
      cdf_upper_bootstrap <- apply(cdf_matrix, 1, quantile, probs = 0.975)
      vals_lower_bootstrap <- apply(vals_matrix, 1, quantile, probs = 0.025)
      vals_upper_bootstrap <- apply(vals_matrix, 1, quantile, probs = 0.975)

      list(
        cdf_lower = cdf_lower_bootstrap,
        cdf_upper = cdf_upper_bootstrap,
        vals_lower = vals_lower_bootstrap,
        vals_upper = vals_upper_bootstrap
      )
    }) %...>%
      {
        # Update plot_data with bootstrap results
        bootstrap_results <- .
        values$plot_data <- values$plot_data %>%
          mutate(
            cdf_lower_bootstrap = bootstrap_results$cdf_lower,
            cdf_upper_bootstrap = bootstrap_results$cdf_upper,
            vals_lower_bootstrap = bootstrap_results$vals_lower,
            vals_upper_bootstrap = bootstrap_results$vals_upper
          )
        values$bootstrap_complete <- TRUE
        showNotification(
          "Confidence intervals computed successfully!",
          type = "message"
        )
      }
  })

  # Results outputs
  output$gumbel_fit <- renderPrint({
    req(values$fit, values$loc, values$scale)
    cat("Location parameter:", round(values$loc, 6), "\n")
    cat("Scale parameter:", round(values$scale, 6), "\n")
    cat("\nParameters estimated using maximum likelihood.")
  })

  output$min_thickness_estimate <- renderPrint({
    req(values$x_N, values$se_x_N, input$nominal_thickness)

    # Confidence interval using Student's t-distribution
    alpha <- 0.05
    t_value <- qt(1 - alpha / 2, df = values$n - 1)
    x_N_lower <- values$x_N - t_value * values$se_x_N
    x_N_upper <- values$x_N + t_value * values$se_x_N

    min_thickness_estimate <- input$nominal_thickness - x_N_upper

    cat(
      "Estimated Maximum Wall Loss for",
      input$n_tubes,
      "tubes:",
      round(values$x_N, 6),
      "\n"
    )
    cat("Standard Error:", round(values$se_x_N, 6), "\n")
    cat(
      "95% CI for Max Wall Loss: [",
      round(x_N_lower, 6),
      ",",
      round(x_N_upper, 6),
      "]\n"
    )
    cat(
      "\nEstimated Minimum Wall Thickness:",
      round(min_thickness_estimate, 6),
      "\n"
    )
    cat("\nStart of Operation:", as.character(input$start_operation), "\n")
    cat("Inspection Date:", as.character(input$inspection_date), "\n")
    cat(
      "Time in Service:",
      as.numeric(input$inspection_date - input$start_operation),
      "days\n"
    )
  })

  output$ks_test <- renderPrint({
    req(values$max_wall_loss, values$loc, values$scale)
    ks.test(
      values$max_wall_loss,
      "pgumbel",
      loc = values$loc,
      scale = values$scale
    )
  })

  output$ad_test <- renderPrint({
    req(values$max_wall_loss, values$loc, values$scale)
    ad.test(
      values$max_wall_loss,
      "pgumbel",
      loc = values$loc,
      scale = values$scale
    )
  })

  # Plot outputs
  output$prob_plot <- renderPlot({
    req(values$plot_data)

    plot <- ggplot(
      values$plot_data,
      aes(x = emp_cdf_betaMedian, y = theo_cdf)
    ) +
      geom_point(color = "red", size = 2) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "blue",
        linetype = "dashed",
        linewidth = 1
      ) +
      labs(
        title = "Probability Plot",
        x = "Empirical CDF (Beta Median)",
        y = "Theoretical CDF (Gumbel)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14))

    # Add confidence intervals if bootstrap is complete
    if (
      values$bootstrap_complete &&
        !is.null(values$plot_data$cdf_lower_bootstrap) &&
        !is.null(values$plot_data$cdf_upper_bootstrap)
    ) {
      plot <- plot +
        geom_point(
          aes(y = cdf_lower_bootstrap),
          color = "black",
          shape = 95,
          size = 5
        ) +
        geom_point(
          aes(y = cdf_upper_bootstrap),
          color = "black",
          shape = 95,
          size = 5
        )
    }

    plot
  })

  output$cdf_plot <- renderPlot({
    req(values$plot_data, values$plot_data2)

    ggplot(values$plot_data, aes(x = max_wall_loss)) +
      geom_line(
        data = values$plot_data2,
        aes(x = max_wall_loss, y = theo_cdf),
        color = "blue",
        linewidth = 1
      ) +
      geom_point(aes(y = emp_cdf_betaMedian), color = "red", size = 2) +
      labs(
        title = "Empirical vs Theoretical CDF",
        x = "Max Wall Loss",
        y = "CDF"
      ) +
      scale_x_continuous(
        limits = c(
          min(values$max_wall_loss) * 0.75,
          max(values$max_wall_loss) * 1.25
        )
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14))
  })

  output$quantile_plot <- renderPlot({
    req(values$plot_data)

    plot <- ggplot(
      values$plot_data,
      aes(x = theo_quantile_betaMedian, y = max_wall_loss)
    ) +
      geom_point(color = "red", size = 2) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "blue",
        linetype = "dashed",
        linewidth = 1
      ) +
      labs(
        title = "Quantile Plot",
        x = "Theoretical Quantile (Beta Median)",
        y = "Max Wall Loss (Empirical)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14))

    # Add confidence intervals if bootstrap is complete
    if (
      values$bootstrap_complete &&
        !is.null(values$plot_data$vals_lower_bootstrap) &&
        !is.null(values$plot_data$vals_upper_bootstrap)
    ) {
      plot <- plot +
        geom_point(
          aes(y = vals_lower_bootstrap),
          color = "black",
          shape = 95,
          size = 5
        ) +
        geom_point(
          aes(y = vals_upper_bootstrap),
          color = "black",
          shape = 95,
          size = 5
        )
    }

    plot
  })

  output$exceedance_plot <- renderPlot({
    req(values$plot_data, values$plot_data2)

    ggplot(values$plot_data, aes(x = max_wall_loss)) +
      geom_line(
        data = values$plot_data2,
        aes(x = max_wall_loss, y = log(1 - theo_cdf)),
        color = "blue",
        linewidth = 1
      ) +
      geom_point(
        aes(y = log(1 - emp_cdf_betaMedian)),
        color = "red",
        size = 2
      ) +
      labs(
        title = "Empirical vs Theoretical Exceedance Probability",
        x = "Max Wall Loss",
        y = "Log(Exceedance Probability)"
      ) +
      scale_x_continuous(
        limits = c(
          min(values$max_wall_loss) * 0.75,
          max(values$max_wall_loss) * 1.25
        )
      ) +
      scale_y_continuous(
        limits = c(floor(min(log(1 - values$plot_data$emp_cdf_betaMedian))), 0)
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
