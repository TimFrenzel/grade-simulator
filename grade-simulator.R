#----------------- SETUP -----------------
# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

# Load necessary libraries
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(testthat)

#----------------- DATA VALIDATION FUNCTION -----------------
# Function to validate the format of the grades dataframe
validate_grades_df <- function(grades_df) {
  if (!is.character(grades_df[[1]])) return("First column must be strings.")
  
  for (i in 2:ncol(grades_df)) {
    if (!all(sapply(grades_df[[i]], function(x) is.na(x) || is.numeric(x)))) 
      return(paste("Column", i, "must have numeric values or NA."))
    
    grades_df[[i]] <- sapply(grades_df[[i]], function(x) ifelse(!is.na(x) && x <= 1, x * 100, ifelse(x > 100, NA, x)))
  }
  
  grades_df
}

#----------------- DATA PREPARATION -----------------
# Read and validate CSV files
weights_df <- read.csv('weights.csv')
grades_df <- read.csv('grades.csv')
grades_df <- validate_grades_df(grades_df)
total_sims = 1000

#----------------- SIMULATION FUNCTIONS -----------------
# Functions related to grade simulation, waiver, and parameter computation
simulate_grades <- function(grades, weights, mean_grades, sd_grades, n_simulations = total_sims) {
  n_assignments <- length(weights)
  simulated_grades <- matrix(NA, nrow = n_assignments, ncol = n_simulations)
  
  for (j in 1:n_assignments) {
    if (is.na(grades[j])) {
      simulated_grades[j, ] <- pmin(pmax(rnorm(n_simulations, mean = mean_grades[j], sd = sd_grades[j]), 0), 100)
    } else {
      simulated_grades[j, ] <- grades[j]
    }
  }
  
  simulated_grades
}

waive_assignment <- function(weights, assignment_index) {
  updated_weights <- weights  # Copy original weights
  waived_weight <- updated_weights[assignment_index]
  updated_weights[assignment_index] <- 0  # Set waived weight to zero
  
  # Distribute this weight equally among other assignments
  n_other_assignments <- sum(updated_weights > 0)
  if (n_other_assignments > 0) {
    updated_weights[updated_weights > 0] <- updated_weights[updated_weights > 0] + waived_weight / n_other_assignments
  }
  
  return(data.frame(t(updated_weights)))
}

compute_grade_parameters <- function(student_grades, input) {
  n <- ncol(student_grades)
  mean_grades <- numeric(n)
  sd_grades <- numeric(n)
  
  for (i in 1:n) {
    if (is.na(student_grades[[i]])) {
      # Use input values if available, otherwise use default values
      mean_grades[i] <- if(!is.null(input[[paste("mean", i, sep = "_")]])) {
        as.numeric(input[[paste("mean", i, sep = "_")]])
      } else {
        80  # Default mean
      }
      sd_grades[i] <- if(!is.null(input[[paste("stdev", i, sep = "_")]])) {
        as.numeric(input[[paste("stdev", i, sep = "_")]])
      } else {
        10  # Default standard deviation
      }
    } else {
      # For non-missing grades, use the actual grade and sd = 0
      mean_grades[i] <- student_grades[[i]]
      sd_grades[i] <- 0
    }
  }
  
  list(mean_grades = mean_grades, sd_grades = sd_grades)
}

clean_grades_and_compute_params <- function(grades, default_mean = 80, default_sd = 10, min_sd = 5) {
  if (length(grades) > 1) {
    Q1 <- quantile(grades, 0.25)
    Q3 <- quantile(grades, 0.75)
    IQR <- Q3 - Q1
    filtered_grades <- grades[grades >= (Q1 - 1.5 * IQR) & grades <= (Q3 + 1.5 * IQR)]
  } else {
    filtered_grades <- grades
  }
  
  user_mean <- ifelse(length(filtered_grades) > 0, mean(filtered_grades), default_mean)
  user_sd <- max(ifelse(length(filtered_grades) > 1, sd(filtered_grades), default_sd), min_sd)
  
  list(mean = user_mean, sd = user_sd)
}

adjust_params_by_scenario <- function(scenario, mean, sd, mean_adjust_factor = 5, sd_reduce_factor = 2, min_sd = 5) {
  adjust_factor <- switch(scenario,
                          "Default" = 0,
                          "Pessimistic 1" = -1,
                          "Pessimistic 2" = -2,
                          "Optimistic 1" = 1,
                          "Optimistic 2" = 2,
                          0)  # Default case if scenario does not match
  
  adjusted_mean <- mean + (adjust_factor * mean_adjust_factor)
  adjusted_sd <- max(sd - (adjust_factor * sd_reduce_factor), min_sd)
  
  list(mean = adjusted_mean, sd = adjusted_sd)
}

#----------------- TABLE GENERATION FUNCTION -----------------
# Function to create a simulation table based on grades and weights
create_simulation_table <- function(grades_df, results, weights) {
  grades <- round(results[, ncol(results)], 2)
  
  # Identify the lowest grade
  lowest_grade <- min(grades)
  
  # Generate the HTML table
  table_content <- tags$table(class = 'table table-striped',
                              # Header row
                              tags$thead(
                                tags$tr(
                                  tags$th("Assignment"),
                                  tags$th("Weight"),
                                  tags$th("Grade")
                                )
                              ),
                              # Table body with dynamic rows
                              tags$tbody(
                                lapply(1:ncol(grades_df[-1]), function(i) {
                                  # Check if the assignment is waived (weight is zero)
                                  waived <- weights[i] == 0
                                  btn_class <- if (waived) "btn btn-waived btn-fixed-width" else "btn btn-primary btn-fixed-width"
                                  
                                  # Apply color based on the grade value
                                  grade_color <- if (grades[i] == lowest_grade) {
                                    "color: orange;"
                                  } else if (grades[i] < 70) {
                                    "color: red;"
                                  } else {
                                    ""
                                  }
                                  
                                  tags$tr(
                                    tags$td(actionButton(inputId = paste0("waiveAssignment", i),
                                                         label = colnames(grades_df)[-1][i],
                                                         class = btn_class)),
                                    tags$td(style = if (waived) "color: lightgrey; font-style: italic;" else "",
                                            sprintf("%.1f%%", weights[i] * 100)),
                                    tags$td(style = grade_color, grades[i])
                                  )
                                })
                              )
  )
  
  return(table_content)
}

#----------------- PLOTTING FUNCTIONS -----------------
# Functions for creating different types of plots
createGradeAnalyticsPlot <- function(existing_grades, simulated_grades) {
  n_assignments <- length(existing_grades)
  median_simulated <- apply(simulated_grades, 1, median)
  
  combined_grades <- ifelse(is.na(existing_grades), median_simulated, existing_grades)
  
  data <- data.frame(Assignment = 1:n_assignments,
                     Grades = combined_grades)
  
  gg <- ggplot(data, aes(x = Assignment, y = Grades)) +
    geom_line(color = "black") +
    geom_point(aes(color = ifelse(Grades > 90, "green", ifelse(Grades < 70, "red", "black")))) +  # Conditional coloring
    scale_color_identity(guide = "none") +  # Suppress legend
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "orange", linetype = "dashed") +
    labs(x = "", y = "Grade") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), legend.position = "none")
  
  ggplotly(gg)
}

createHistogramPlot <- function(final_grades_simulation, student_id) {
  valid_grades <- final_grades_simulation[final_grades_simulation >= 0 & final_grades_simulation <= 100]
  
  # Define grade letters and breaks
  grade_letters <- c("F", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")
  breaks <- c(0, 60, 63, 67, 70, 73, 77, 80, 83, 87, 90, 94, 100)
  
  # Count grades in each range for positioning the annotations
  grade_counts <- cut(valid_grades, breaks = breaks, labels = grade_letters, include.lowest = TRUE)
  grade_counts_summary <- as.data.frame(table(grade_counts))
  
  # Create ggplot histogram
  gg_hist <- ggplot(data.frame(Grade = valid_grades), aes(x = Grade)) +
    geom_histogram(breaks = breaks,
                   fill = c("#FF0000", "#FF6666", "#FF9999", "#FFCCCC",
                            "#FFFF66", "#FFFF33", "#FFFF00",
                            "#99FF99", "#66FF66", "#33FF33",
                            "#66CCFF", "#0000FF"),
                   color = "black",
                   aes(y = ..count.., text = paste("Probability: ", sprintf("%.1f%%", 100 * ..count../sum(..count..))))) +
    labs(x = "Final Grade", y = NULL) +
    xlim(50, 100) +
    theme(plot.margin = margin(t = 30, r = 10, b = 10, l = 10))  # Adjusting margins
  
  # Convert ggplot to plotly for interactivity
  plotly_hist <- ggplotly(gg_hist, tooltip = "text")
  
  # Add annotations for grade letters
  for (i in 1:nrow(grade_counts_summary)) {
    label_color <- ifelse(grade_counts_summary$grade_counts[i] %in% c("F", "D-", "D", "D+"), "red", "black")
    grade_midpoint <- (breaks[i] + breaks[i + 1]) / 2
    max_count <- max(grade_counts_summary$Freq[grade_counts_summary$grade_counts == grade_counts_summary$grade_counts[i]])
    
    plotly_hist <- plotly_hist %>%
      add_annotations(
        x = grade_midpoint,
        y = max_count + 1,  # Position above the highest bar in the range
        text = grade_counts_summary$grade_counts[i],
        font = list(family = "Arial", size = 12, color = label_color),
        showarrow = FALSE,
        xref = "x",
        yref = "y",
        yanchor = "bottom",
        textangle = 0,
        bold = TRUE
      )
  }
  
  return(plotly_hist)
}

#----------------- UI COMPONENT FUNCTIONS -----------------
# Functions to create UI components like sliders
createGradeSliders <- function(student_grades, user_mean, user_sd) {
  # Pre-calculate the inputs for sliders
  slider_inputs <- lapply(1:ncol(student_grades), function(i) {
    grade_value <- student_grades[[i]]
    if (is.na(grade_value)) {
      list(
        input_id_mean = paste("mean", i, sep = "_"),
        label_mean = paste("Mean for", colnames(student_grades)[i]),
        input_id_sd = paste("stdev", i, sep = "_"),
        label_sd = paste("St. Dev for", colnames(student_grades)[i])
      )
    } else {
      NULL
    }
  })
  
  # Filter out NULL elements
  slider_inputs <- Filter(Negate(is.null), slider_inputs)
  
  # Use lapply only for UI element creation
  ui_elements <- lapply(slider_inputs, function(x) {
    list(
      sliderInput(inputId = x$input_id_mean,
                  label = x$label_mean,
                  min = 0, max = 100, value = user_mean),
      sliderInput(inputId = x$input_id_sd,
                  label = x$label_sd,
                  min = 0, max = 20, value = user_sd)
    )
  })
  
  do.call(tagList, unlist(ui_elements, recursive = FALSE))
}

#----------------- SHINY UI DEFINITION -----------------
# Define the Shiny user interface
ui <- dashboardPage(
  dashboardHeader(title = "Student Grade Simulator"),
  
  dashboardSidebar(
    selectInput("selectedStudent", "Select a Student:", choices = grades_df$USER, selected = grades_df$USER[1]),
    selectInput("scenario", "Select a Scenario:", choices = c("Default", "Pessimistic 1", "Pessimistic 2", "Optimistic 1", "Optimistic 2")),
    actionButton("resetWeights", "Reset Weights"),
    uiOutput("assignmentSliders")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .median-score-style {
        text-align: center;
        font-weight: bold;
        font-size: 1.2em;
      }
    "))
    ),
    fluidRow(
      box(plotlyOutput("gradeHistogram"), width = 12),
      box(
        div(textOutput("medianScore"), class = "median-score-style"),
        width = 12, status = "primary", solidHeader = FALSE, background = "aqua"
      ),
      box(plotlyOutput("gradeAnalytics"), width = 12),
      box(uiOutput("simulationTable"), width = 12)
    )
  )
)

#----------------- SHINY SERVER LOGIC -----------------
# Define the server logic for the Shiny app
server <- function(input, output, session) {
  # Reactive values for user-specific mean and standard deviation
  reactive_values <- reactiveValues(mean = NULL, sd = NULL, weights_df = weights_df)
  weights_vector <- reactive({ as.numeric(reactive_values$weights_df) })
  
  # Modularized function to update scenario and student changes
  update_scenario_and_student <- function(student_id, scenario, grades_df, reactive_values) {
    student_grades <- grades_df[grades_df$USER == student_id, -1]
    existing_grades <- student_grades[!is.na(student_grades)]
    
    grade_params <- clean_grades_and_compute_params(existing_grades)
    adjusted_params <- adjust_params_by_scenario(scenario, grade_params$mean, grade_params$sd)
    
    reactive_values$mean <- adjusted_params$mean
    reactive_values$sd <- adjusted_params$sd
  }
  
  # Modularized function to update plots and scores
  update_plots_and_scores <- function(simulation_results, weights_vector, student_id, output) {
    results <- simulation_results()
    
    output$gradeHistogram <- renderPlotly({
      createHistogramPlot(colSums(results * weights_vector() / sum(weights_vector())), student_id)
    })
    
    output$medianScore <- renderText({
      paste("Median Score:", round(median(colSums(results * weights_vector()) / sum(weights_vector())), 2))
    })
  }
  
  # Observers for each assignment waiver button
  observe({
    lapply(1:ncol(grades_df[-1]), function(i) {
      observeEvent(input[[paste0("waiveAssignment", i)]], {
        reactive_values$weights_df <- waive_assignment(weights_vector(), i)
        update_plots_and_scores(simulation_results, weights_vector, input$selectedStudent, output)
      })
    })
  })
  
  # Observer for resetting weights and recalculating when a new student is selected or the reset button is clicked
  observeEvent(c(input$resetWeights, input$selectedStudent), {
    reactive_values$weights_df <<- weights_df
    update_plots_and_scores(simulation_results, weights_vector, input$selectedStudent, output)
  })
  
  # Observe changes in selected student or scenario and update reactive values
  observe({
    student_id <- input$selectedStudent
    scenario <- input$scenario
    update_scenario_and_student(student_id, scenario, grades_df, reactive_values)
  })
  
  # Create sliders based on selected student
  output$assignmentSliders <- renderUI({
    student_id <- input$selectedStudent
    student_grades <- grades_df[grades_df$USER == student_id, -1]
    createGradeSliders(student_grades, reactive_values$mean, reactive_values$sd)
  })
  
  # Reactive expression for simulation results
  simulation_results <- reactive({
    student_id <- input$selectedStudent
    student_grades <- grades_df[grades_df$USER == student_id, -1]
    grade_params <- compute_grade_parameters(student_grades, input)
    simulate_grades(as.numeric(student_grades), weights_df, grade_params$mean_grades, grade_params$sd_grades)
  })
  
  # Observe changes in simulation results to update outputs
  observe({
    update_plots_and_scores(simulation_results, weights_vector, input$selectedStudent, output)
    output$simulationTable <- renderUI({
      results <- simulation_results()
      weights <- weights_vector()
      create_simulation_table(grades_df, results, weights)
    })
  })
  
  output$gradeAnalytics <- renderPlotly({
    student_id <- input$selectedStudent
    student_grades <- grades_df[grades_df$USER == student_id, -1]
    existing_grades <- student_grades[!is.na(student_grades)]
    results <- simulation_results()
    createGradeAnalyticsPlot(existing_grades, results)
  })
}

# Run the app
shinyApp(ui, server)