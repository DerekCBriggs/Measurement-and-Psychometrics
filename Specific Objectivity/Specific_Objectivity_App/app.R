# Specific Objectivity Interactive Shiny App
# This app demonstrates the concept of specific objectivity by comparing
# the Rasch model with the 2PL model

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================================
# Helper Functions
# ============================================================================

# Calculate probability of correct response
calc_prob <- function(theta, b, a = 1, c = 0) {
  c + (1 - c) / (1 + exp(-a * (theta - b)))
}

# Calculate logit (log odds) of correct response
calc_logit <- function(theta, b, a = 1, c = 0) {
  P <- calc_prob(theta, b, a, c)
  log((P - c) / (1 - P))
}

# Generate ICC data for plotting
generate_icc_data <- function(b, a = 1, c = 0, model_name = "Model") {
  theta <- seq(-4, 4, 0.1)
  P <- calc_prob(theta, b, a, c)
  data.frame(theta = theta, P = P, b = b, a = a, model = model_name)
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(

  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      .highlight-box {
        padding: 15px;
        border-radius: 8px;
        margin: 10px 0;
      }
      .rasch-highlight {
        background-color: #d4edda;
        border: 2px solid #28a745;
      }
      .twopl-highlight {
        background-color: #fff3cd;
        border: 2px solid #ffc107;
      }
      .insight-box {
        background-color: #cce5ff;
        border: 2px solid #007bff;
        padding: 15px;
        border-radius: 8px;
        margin: 15px 0;
      }
      .title-header {
        background-color: #343a40;
        color: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 8px;
      }
      .nav-tabs > li.active > a {
        font-weight: bold;
      }
      .constant-diff {
        color: #28a745;
        font-weight: bold;
      }
      .varying-diff {
        color: #dc3545;
        font-weight: bold;
      }
    "))
  ),

  # Title
  div(class = "title-header",
      h1("Exploring Specific Objectivity"),
      h4("Comparing the Rasch Model and the 2-Parameter Logistic (2PL) Model")
  ),

  # Main layout
  sidebarLayout(

    # Sidebar with controls
    sidebarPanel(
      width = 3,

      h4("Item Parameters"),
      hr(),

      h5("Item 1 (Blue)"),
      sliderInput("b1", "Difficulty (b):", min = -3, max = 3, value = -0.5, step = 0.1),
      sliderInput("a1_2pl", "Discrimination (a) for 2PL:", min = 0.5, max = 3, value = 1, step = 0.1),

      hr(),

      h5("Item 2 (Magenta)"),
      sliderInput("b2", "Difficulty (b):", min = -3, max = 3, value = 0, step = 0.1),
      sliderInput("a2_2pl", "Discrimination (a) for 2PL:", min = 0.5, max = 3, value = 2, step = 0.1),

      hr(),
      h4("Student Abilities"),
      hr(),

      h5("Low-Ability Pair"),
      sliderInput("theta_larry", "Larry's ability (theta):", min = -3, max = 3, value = -0.5, step = 0.1),
      sliderInput("theta_kevin", "Kevin's ability (theta):", min = -3, max = 3, value = -1.0, step = 0.1),

      hr(),

      h5("High-Ability Pair"),
      sliderInput("theta_celia", "Celia's ability (theta):", min = -3, max = 3, value = 1.5, step = 0.1),
      sliderInput("theta_temple", "Temple's ability (theta):", min = -3, max = 3, value = 1.0, step = 0.1),

      hr(),

      actionButton("reset", "Reset to Defaults", class = "btn-warning btn-block")
    ),

    # Main panel with tabs
    mainPanel(
      width = 9,

      tabsetPanel(

        # Tab 1: Introduction
        tabPanel("Introduction",
                 br(),
                 div(class = "insight-box",
                     h4("What is Specific Objectivity?"),
                     p("Georg Rasch (1960, 1967) proposed that good measurement should have a special property:"),
                     tags$ul(
                       tags$li(strong("Person comparisons"), " should be ", strong("invariant"),
                               " over the specific items used to measure them."),
                       tags$li(strong("Item comparisons"), " should be ", strong("invariant"),
                               " over the specific persons used to calibrate them.")
                     ),
                     p("In other words, if Larry is 0.5 logits more able than Kevin, this difference should be the same ",
                       "regardless of which item we use to compare them. This is what Rasch called ",
                       strong("Specific Objectivity"), ".")
                 ),

                 h4("The Key Question"),
                 p("Does the difference in predicted performance between two people depend on which item we use?"),
                 tags$ul(
                   tags$li("Under the ", strong("Rasch model"), ": NO - differences are constant (specific objectivity holds)"),
                   tags$li("Under the ", strong("2PL model"), ": YES - differences vary by item (specific objectivity violated)")
                 ),

                 h4("How to Use This App"),
                 tags$ol(
                   tags$li("Explore the ", strong("ICC Comparison"), " tab to see how the item response curves differ between models."),
                   tags$li("Complete the ", strong("Activity Worksheet"), " tab by calculating probabilities and logits for each student."),
                   tags$li("Check your work in the ", strong("Answer Key"), " tab."),
                   tags$li("Explore the ", strong("Key Insight"), " tab to see the demonstration of specific objectivity."),
                   tags$li("Adjust the sliders to experiment with different item parameters and student abilities.")
                 )
        ),

        # Tab 2: ICC Comparison
        tabPanel("ICC Comparison",
                 br(),
                 fluidRow(
                   column(6,
                          h4("2-Parameter Logistic (2PL) Model"),
                          div(class = "twopl-highlight highlight-box",
                              p("Items can have ", strong("different discriminations"), " (slopes)."),
                              p("Notice how the curves can ", strong("cross"), " each other.")
                          ),
                          plotOutput("icc_2pl", height = "350px")
                   ),
                   column(6,
                          h4("Rasch Model"),
                          div(class = "rasch-highlight highlight-box",
                              p("All items have the ", strong("same discrimination"), " (a = 1)."),
                              p("Curves are ", strong("parallel"), " - they never cross.")
                          ),
                          plotOutput("icc_rasch", height = "350px")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(6,
                          h4("2PL: Logit Scale"),
                          plotOutput("logit_2pl", height = "300px")
                   ),
                   column(6,
                          h4("Rasch: Logit Scale"),
                          plotOutput("logit_rasch", height = "300px")
                   )
                 )
        ),

        # Tab 3: Activity Worksheet (blank tables for students to fill in)
        tabPanel("Activity Worksheet",
                 br(),
                 h4("Activity: Compare Student Pairs"),
                 p("Using the item parameters and student abilities shown in the sidebar, ",
                   strong("calculate the predicted performance"), " on each item under both models."),
                 p("Fill in the blank cells below, then check your answers in the ",
                   strong("Answer Key"), " tab."),

                 hr(),

                 # Formula reminder box
                 div(class = "insight-box",
                     h4("Formulas to Use"),
                     fluidRow(
                       column(6,
                              h5("Probability of Correct Response:"),
                              p(style = "text-align: center; font-size: 16px; font-family: monospace;",
                                "P = 1 / (1 + exp(-a * (theta - b)))")
                       ),
                       column(6,
                              h5("Logit (Log Odds) of Correct Response:"),
                              p(style = "text-align: center; font-size: 16px; font-family: monospace;",
                                "Logit = a * (theta - b)")
                       )
                     ),
                     p(style = "text-align: center;",
                       em("Note: For the Rasch model, a = 1 for all items."))
                 ),

                 br(),

                 # Current parameter values display
                 div(class = "highlight-box", style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
                     h4("Current Parameter Values"),
                     fluidRow(
                       column(6,
                              h5("Item Parameters:"),
                              uiOutput("param_display_items")
                       ),
                       column(6,
                              h5("Student Abilities (theta):"),
                              uiOutput("param_display_students")
                       )
                     )
                 ),

                 br(),

                 # Blank worksheet tables
                 fluidRow(
                   column(6,
                          h4("2PL Model", style = "color: #856404;"),
                          h5("Low-Ability Pair (Larry & Kevin)"),
                          tableOutput("worksheet_2pl_low"),
                          h5("High-Ability Pair (Celia & Temple)"),
                          tableOutput("worksheet_2pl_high")
                   ),
                   column(6,
                          h4("Rasch Model", style = "color: #155724;"),
                          h5("Low-Ability Pair (Larry & Kevin)"),
                          tableOutput("worksheet_rasch_low"),
                          h5("High-Ability Pair (Celia & Temple)"),
                          tableOutput("worksheet_rasch_high")
                   )
                 ),

                 br(),
                 div(class = "insight-box",
                     h4("Questions to Consider After Completing the Tables"),
                     tags$ol(
                       tags$li("What do you notice about the differences in ", strong("probabilities"),
                               " across items within each model?"),
                       tags$li("What do you notice about the differences in ", strong("logits"),
                               " across items within each model?"),
                       tags$li("Which model shows constant differences? Why might this matter for measurement?")
                     )
                 )
        ),

        # Tab 4: Answer Key (populated tables)
        tabPanel("Answer Key",
                 br(),
                 h4("Answer Key: Completed Comparison Tables"),
                 p("Compare your calculations from the Activity Worksheet with the answers below."),
                 p("Pay attention to the ", strong("differences"), " in both probability and logits."),

                 fluidRow(
                   column(6,
                          h4("2PL Model Results", style = "color: #856404;"),
                          h5("Low-Ability Pair (Larry & Kevin)"),
                          tableOutput("table_2pl_low"),
                          h5("High-Ability Pair (Celia & Temple)"),
                          tableOutput("table_2pl_high")
                   ),
                   column(6,
                          h4("Rasch Model Results", style = "color: #155724;"),
                          h5("Low-Ability Pair (Larry & Kevin)"),
                          tableOutput("table_rasch_low"),
                          h5("High-Ability Pair (Celia & Temple)"),
                          tableOutput("table_rasch_high")
                   )
                 ),

                 br(),
                 div(class = "insight-box",
                     h4("Key Observations"),
                     tags$ul(
                       tags$li(strong("2PL Model:"), " The logit differences ",
                               span(class = "varying-diff", "VARY"),
                               " depending on which item is used and where on the ability scale the students are located."),
                       tags$li(strong("Rasch Model:"), " The logit differences are ",
                               span(class = "constant-diff", "CONSTANT"),
                               " regardless of which item is used or where on the ability scale the students are located."),
                       tags$li("This constant difference property is what Rasch called ",
                               strong("Specific Objectivity"), ".")
                     )
                 )
        ),

        # Tab 4: Key Insight
        tabPanel("Key Insight",
                 br(),
                 h3("The Specific Objectivity Demonstration"),

                 fluidRow(
                   column(6,
                          div(class = "twopl-highlight highlight-box",
                              h4("2PL Model"),
                              h5("Logit Differences for Low-Ability Pair:"),
                              uiOutput("diff_2pl_low"),
                              h5("Logit Differences for High-Ability Pair:"),
                              uiOutput("diff_2pl_high"),
                              br(),
                              p(class = "varying-diff",
                                "Notice: Differences VARY across items and student pairs!")
                          )
                   ),
                   column(6,
                          div(class = "rasch-highlight highlight-box",
                              h4("Rasch Model"),
                              h5("Logit Differences for Low-Ability Pair:"),
                              uiOutput("diff_rasch_low"),
                              h5("Logit Differences for High-Ability Pair:"),
                              uiOutput("diff_rasch_high"),
                              br(),
                              p(class = "constant-diff",
                                "Notice: Differences are CONSTANT across items and student pairs!")
                          )
                   )
                 ),

                 br(),

                 div(class = "insight-box",
                     h4("Why Does This Matter?"),
                     p("Under the Rasch model, the difference in logits between two people is:"),
                     p(style = "text-align: center; font-size: 18px;",
                       tags$code("logit(Larry) - logit(Kevin) = (theta_Larry - b) - (theta_Kevin - b) = theta_Larry - theta_Kevin")),
                     p("The item difficulty (b) ", strong("cancels out"), "! This means:"),
                     tags$ul(
                       tags$li("Person comparisons don't depend on which items we use"),
                       tags$li("We can meaningfully say 'Larry is 0.5 logits more able than Kevin' as a ",
                               strong("general statement")),
                       tags$li("This is the foundation for ", strong("interval-level measurement"),
                               " in the Rasch tradition")
                     ),
                     p("In the 2PL model, different discriminations mean the item parameter does NOT cancel out, ",
                       "so comparisons depend on which items are used.")
                 ),

                 br(),

                 h4("Interactive Visualization"),
                 p("The plot below shows the logit differences across the full ability range:"),
                 plotOutput("diff_plot", height = "400px")
        )
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # Reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "b1", value = -0.5)
    updateSliderInput(session, "b2", value = 0)
    updateSliderInput(session, "a1_2pl", value = 1)
    updateSliderInput(session, "a2_2pl", value = 2)
    updateSliderInput(session, "theta_larry", value = -0.5)
    updateSliderInput(session, "theta_kevin", value = -1.0)
    updateSliderInput(session, "theta_celia", value = 1.5)
    updateSliderInput(session, "theta_temple", value = 1.0)
  })

  # ICC Plot for 2PL
  output$icc_2pl <- renderPlot({
    theta <- seq(-4, 4, 0.1)

    df <- rbind(
      data.frame(theta = theta, P = calc_prob(theta, input$b1, input$a1_2pl), Item = "Item 1"),
      data.frame(theta = theta, P = calc_prob(theta, input$b2, input$a2_2pl), Item = "Item 2")
    )

    students <- data.frame(
      theta = c(input$theta_larry, input$theta_kevin, input$theta_celia, input$theta_temple),
      name = c("Larry", "Kevin", "Celia", "Temple")
    )

    ggplot(df, aes(x = theta, y = P, color = Item)) +
      geom_line(size = 1.2) +
      geom_vline(data = students, aes(xintercept = theta), linetype = "dashed", alpha = 0.5) +
      geom_text(data = students, aes(x = theta, y = 0.05, label = name),
                inherit.aes = FALSE, angle = 90, hjust = 0, size = 3) +
      scale_color_manual(values = c("Item 1" = "blue", "Item 2" = "magenta")) +
      labs(x = "Ability (theta)", y = "P(Correct)",
           title = paste0("Item 1: a=", input$a1_2pl, ", b=", input$b1,
                          "  |  Item 2: a=", input$a2_2pl, ", b=", input$b2)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      coord_cartesian(xlim = c(-4, 4), ylim = c(0, 1))
  })

  # ICC Plot for Rasch
  output$icc_rasch <- renderPlot({
    theta <- seq(-4, 4, 0.1)

    df <- rbind(
      data.frame(theta = theta, P = calc_prob(theta, input$b1, 1), Item = "Item 1"),
      data.frame(theta = theta, P = calc_prob(theta, input$b2, 1), Item = "Item 2")
    )

    students <- data.frame(
      theta = c(input$theta_larry, input$theta_kevin, input$theta_celia, input$theta_temple),
      name = c("Larry", "Kevin", "Celia", "Temple")
    )

    ggplot(df, aes(x = theta, y = P, color = Item)) +
      geom_line(size = 1.2) +
      geom_vline(data = students, aes(xintercept = theta), linetype = "dashed", alpha = 0.5) +
      geom_text(data = students, aes(x = theta, y = 0.05, label = name),
                inherit.aes = FALSE, angle = 90, hjust = 0, size = 3) +
      scale_color_manual(values = c("Item 1" = "blue", "Item 2" = "magenta")) +
      labs(x = "Ability (theta)", y = "P(Correct)",
           title = paste0("Item 1: a=1, b=", input$b1, "  |  Item 2: a=1, b=", input$b2)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      coord_cartesian(xlim = c(-4, 4), ylim = c(0, 1))
  })

  # Logit Plot for 2PL
  output$logit_2pl <- renderPlot({
    theta <- seq(-4, 4, 0.1)

    df <- rbind(
      data.frame(theta = theta, logit = calc_logit(theta, input$b1, input$a1_2pl), Item = "Item 1"),
      data.frame(theta = theta, logit = calc_logit(theta, input$b2, input$a2_2pl), Item = "Item 2")
    )

    ggplot(df, aes(x = theta, y = logit, color = Item)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Item 1" = "blue", "Item 2" = "magenta")) +
      labs(x = "Ability (theta)", y = "Logit of P(Correct)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      coord_cartesian(xlim = c(-4, 4), ylim = c(-6, 6))
  })

  # Logit Plot for Rasch
  output$logit_rasch <- renderPlot({
    theta <- seq(-4, 4, 0.1)

    df <- rbind(
      data.frame(theta = theta, logit = calc_logit(theta, input$b1, 1), Item = "Item 1"),
      data.frame(theta = theta, logit = calc_logit(theta, input$b2, 1), Item = "Item 2")
    )

    ggplot(df, aes(x = theta, y = logit, color = Item)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Item 1" = "blue", "Item 2" = "magenta")) +
      labs(x = "Ability (theta)", y = "Logit of P(Correct)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      coord_cartesian(xlim = c(-4, 4), ylim = c(-6, 6))
  })

  # Parameter displays for worksheet
  output$param_display_items <- renderUI({
    HTML(paste0(
      "<strong>Item 1 (Blue):</strong> b = ", input$b1,
      ", a = ", input$a1_2pl, " (2PL) / a = 1 (Rasch)<br>",
      "<strong>Item 2 (Magenta):</strong> b = ", input$b2,
      ", a = ", input$a2_2pl, " (2PL) / a = 1 (Rasch)"
    ))
  })

  output$param_display_students <- renderUI({
    HTML(paste0(
      "<strong>Larry:</strong> theta = ", input$theta_larry, "<br>",
      "<strong>Kevin:</strong> theta = ", input$theta_kevin, "<br>",
      "<strong>Celia:</strong> theta = ", input$theta_celia, "<br>",
      "<strong>Temple:</strong> theta = ", input$theta_temple
    ))
  })

  # Helper function to create blank worksheet table
  create_worksheet_table <- function(theta1, theta2, name1, name2) {
    data.frame(
      Student = c(name1, name2, "Difference"),
      Theta = c(theta1, theta2, theta1 - theta2),
      `P(Item 1)` = c("___", "___", "___"),
      `Logit(Item 1)` = c("___", "___", "___"),
      `P(Item 2)` = c("___", "___", "___"),
      `Logit(Item 2)` = c("___", "___", "___"),
      check.names = FALSE
    )
  }

  # Worksheet tables (blank)
  output$worksheet_2pl_low <- renderTable({
    create_worksheet_table(input$theta_larry, input$theta_kevin, "Larry", "Kevin")
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$worksheet_2pl_high <- renderTable({
    create_worksheet_table(input$theta_celia, input$theta_temple, "Celia", "Temple")
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$worksheet_rasch_low <- renderTable({
    create_worksheet_table(input$theta_larry, input$theta_kevin, "Larry", "Kevin")
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$worksheet_rasch_high <- renderTable({
    create_worksheet_table(input$theta_celia, input$theta_temple, "Celia", "Temple")
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Helper function to create comparison table
  create_comparison_table <- function(theta1, theta2, name1, name2, b1, b2, a1, a2) {
    data.frame(
      Student = c(name1, name2, "Difference"),
      Theta = c(theta1, theta2, theta1 - theta2),
      `P(Item 1)` = c(
        round(calc_prob(theta1, b1, a1), 3),
        round(calc_prob(theta2, b1, a1), 3),
        round(calc_prob(theta1, b1, a1) - calc_prob(theta2, b1, a1), 3)
      ),
      `Logit(Item 1)` = c(
        round(calc_logit(theta1, b1, a1), 2),
        round(calc_logit(theta2, b1, a1), 2),
        round(calc_logit(theta1, b1, a1) - calc_logit(theta2, b1, a1), 2)
      ),
      `P(Item 2)` = c(
        round(calc_prob(theta1, b2, a2), 3),
        round(calc_prob(theta2, b2, a2), 3),
        round(calc_prob(theta1, b2, a2) - calc_prob(theta2, b2, a2), 3)
      ),
      `Logit(Item 2)` = c(
        round(calc_logit(theta1, b2, a2), 2),
        round(calc_logit(theta2, b2, a2), 2),
        round(calc_logit(theta1, b2, a2) - calc_logit(theta2, b2, a2), 2)
      ),
      check.names = FALSE
    )
  }

  # 2PL Tables
  output$table_2pl_low <- renderTable({
    create_comparison_table(
      input$theta_larry, input$theta_kevin, "Larry", "Kevin",
      input$b1, input$b2, input$a1_2pl, input$a2_2pl
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$table_2pl_high <- renderTable({
    create_comparison_table(
      input$theta_celia, input$theta_temple, "Celia", "Temple",
      input$b1, input$b2, input$a1_2pl, input$a2_2pl
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Rasch Tables
  output$table_rasch_low <- renderTable({
    create_comparison_table(
      input$theta_larry, input$theta_kevin, "Larry", "Kevin",
      input$b1, input$b2, 1, 1
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$table_rasch_high <- renderTable({
    create_comparison_table(
      input$theta_celia, input$theta_temple, "Celia", "Temple",
      input$b1, input$b2, 1, 1
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Key Insight: Difference displays
  output$diff_2pl_low <- renderUI({
    diff1 <- round(calc_logit(input$theta_larry, input$b1, input$a1_2pl) -
                     calc_logit(input$theta_kevin, input$b1, input$a1_2pl), 2)
    diff2 <- round(calc_logit(input$theta_larry, input$b2, input$a2_2pl) -
                     calc_logit(input$theta_kevin, input$b2, input$a2_2pl), 2)

    HTML(paste0(
      "Item 1: <strong>", diff1, "</strong> logits<br>",
      "Item 2: <strong>", diff2, "</strong> logits"
    ))
  })

  output$diff_2pl_high <- renderUI({
    diff1 <- round(calc_logit(input$theta_celia, input$b1, input$a1_2pl) -
                     calc_logit(input$theta_temple, input$b1, input$a1_2pl), 2)
    diff2 <- round(calc_logit(input$theta_celia, input$b2, input$a2_2pl) -
                     calc_logit(input$theta_temple, input$b2, input$a2_2pl), 2)

    HTML(paste0(
      "Item 1: <strong>", diff1, "</strong> logits<br>",
      "Item 2: <strong>", diff2, "</strong> logits"
    ))
  })

  output$diff_rasch_low <- renderUI({
    diff1 <- round(calc_logit(input$theta_larry, input$b1, 1) -
                     calc_logit(input$theta_kevin, input$b1, 1), 2)
    diff2 <- round(calc_logit(input$theta_larry, input$b2, 1) -
                     calc_logit(input$theta_kevin, input$b2, 1), 2)

    HTML(paste0(
      "Item 1: <strong>", diff1, "</strong> logits<br>",
      "Item 2: <strong>", diff2, "</strong> logits"
    ))
  })

  output$diff_rasch_high <- renderUI({
    diff1 <- round(calc_logit(input$theta_celia, input$b1, 1) -
                     calc_logit(input$theta_temple, input$b1, 1), 2)
    diff2 <- round(calc_logit(input$theta_celia, input$b2, 1) -
                     calc_logit(input$theta_temple, input$b2, 1), 2)

    HTML(paste0(
      "Item 1: <strong>", diff1, "</strong> logits<br>",
      "Item 2: <strong>", diff2, "</strong> logits"
    ))
  })

  # Difference Plot
  output$diff_plot <- renderPlot({
    theta <- seq(-3, 3, 0.1)
    diff_amount <- input$theta_larry - input$theta_kevin  # Use actual difference

    # For a fixed difference, calculate how logit differences vary
    # Compare person at theta vs person at theta - diff_amount

    df <- rbind(
      data.frame(
        theta = theta,
        diff = calc_logit(theta, input$b1, input$a1_2pl) - calc_logit(theta - diff_amount, input$b1, input$a1_2pl),
        Item = "2PL Item 1",
        Model = "2PL"
      ),
      data.frame(
        theta = theta,
        diff = calc_logit(theta, input$b2, input$a2_2pl) - calc_logit(theta - diff_amount, input$b2, input$a2_2pl),
        Item = "2PL Item 2",
        Model = "2PL"
      ),
      data.frame(
        theta = theta,
        diff = calc_logit(theta, input$b1, 1) - calc_logit(theta - diff_amount, input$b1, 1),
        Item = "Rasch Item 1",
        Model = "Rasch"
      ),
      data.frame(
        theta = theta,
        diff = calc_logit(theta, input$b2, 1) - calc_logit(theta - diff_amount, input$b2, 1),
        Item = "Rasch Item 2",
        Model = "Rasch"
      )
    )

    ggplot(df, aes(x = theta, y = diff, color = Item, linetype = Model)) +
      geom_line(size = 1.2) +
      geom_hline(yintercept = diff_amount, linetype = "dashed", color = "darkgreen", size = 1) +
      annotate("text", x = 2.5, y = diff_amount + 0.15,
               label = paste0("True difference = ", round(diff_amount, 2)),
               color = "darkgreen", size = 4) +
      scale_color_manual(values = c("2PL Item 1" = "blue", "2PL Item 2" = "magenta",
                                    "Rasch Item 1" = "darkblue", "Rasch Item 2" = "purple")) +
      labs(x = "Higher-ability person's theta",
           y = "Logit difference between persons",
           title = "Logit Differences Across Ability Range",
           subtitle = paste0("Comparing two persons with ability difference of ",
                             round(diff_amount, 2), " logits")) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      coord_cartesian(xlim = c(-3, 3), ylim = c(-1, 3))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
