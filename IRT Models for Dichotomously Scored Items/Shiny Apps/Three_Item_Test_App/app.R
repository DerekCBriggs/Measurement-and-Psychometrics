# Three-Item Test IRT Activity
# Interactive Shiny App for exploring ICCs and response probabilities
# Derek Briggs - EDUC 8720

library(shiny)
library(ggplot2)

# ============================================================================
# Helper Functions
# ============================================================================

# Calculate probability of correct response (3PL model)
calc_prob <- function(theta, a, b, c) {
  c + (1 - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
}

# Calculate probability of a response vector
calc_vector_prob <- function(theta, responses, a, b, c) {
  prob <- 1
  for (i in seq_along(responses)) {
    p_i <- calc_prob(theta, a[i], b[i], c[i])
    if (responses[i] == 1) {
      prob <- prob * p_i
    } else {
      prob <- prob * (1 - p_i)
    }
  }
  return(prob)
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(

  # Custom CSS
  tags$head(
    tags$style(HTML("
      .title-header {
        background-color: #2c3e50;
        color: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 8px;
      }
      .info-box {
        background-color: #ecf0f1;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
      }
      .result-box {
        background-color: #d5f5e3;
        padding: 15px;
        border-radius: 8px;
        border: 2px solid #27ae60;
      }
      .question-box {
        background-color: #fdebd0;
        padding: 15px;
        border-radius: 8px;
        border: 2px solid #f39c12;
        margin-top: 15px;
      }
    "))
  ),

  # Title
  div(class = "title-header",
      h1("Three-Item Test: ICC Exploration"),
      h4("Interactive activity for understanding Item Characteristic Curves")
  ),

  # Main layout
  sidebarLayout(

    # Sidebar with controls
    sidebarPanel(
      width = 4,

      h4("Item Parameters"),
      helpText("Adjust the parameters for each item to see how the ICCs change."),
      hr(),

      # Item 1
      h5("Item 1", style = "color: #3498db; font-weight: bold;"),
      fluidRow(
        column(4, numericInput("a1", "a:", value = 1, min = 0.1, max = 3, step = 0.1)),
        column(4, numericInput("b1", "b:", value = -1.5, min = -3, max = 3, step = 0.1)),
        column(4, numericInput("c1", "c:", value = 0.2, min = 0, max = 0.5, step = 0.05))
      ),

      hr(),

      # Item 2
      h5("Item 2", style = "color: #e74c3c; font-weight: bold;"),
      fluidRow(
        column(4, numericInput("a2", "a:", value = 1, min = 0.1, max = 3, step = 0.1)),
        column(4, numericInput("b2", "b:", value = 0, min = -3, max = 3, step = 0.1)),
        column(4, numericInput("c2", "c:", value = 0.2, min = 0, max = 0.5, step = 0.05))
      ),

      hr(),

      # Item 3
      h5("Item 3", style = "color: #27ae60; font-weight: bold;"),
      fluidRow(
        column(4, numericInput("a3", "a:", value = 2, min = 0.1, max = 3, step = 0.1)),
        column(4, numericInput("b3", "b:", value = 0.2, min = -3, max = 3, step = 0.1)),
        column(4, numericInput("c3", "c:", value = 0.3, min = 0, max = 0.5, step = 0.05))
      ),

      hr(),

      # Theta value for probability calculation
      h4("Person Ability"),
      sliderInput("theta",
                  label = expression(theta),
                  min = -4, max = 4, value = 0, step = 0.1),

      hr(),

      actionButton("reset", "Reset to Defaults", class = "btn-warning btn-block")
    ),

    # Main panel
    mainPanel(
      width = 8,

      tabsetPanel(

        # Tab 1: ICC Plot
        tabPanel("Item Characteristic Curves",
                 br(),
                 plotOutput("icc_plot", height = "400px"),

                 div(class = "info-box",
                     h4("Reading the Plot"),
                     tags$ul(
                       tags$li("The x-axis shows ability (theta) in logits"),
                       tags$li("The y-axis shows the probability of a correct response"),
                       tags$li("Each curve represents one item"),
                       tags$li("The vertical dashed line shows the current theta value")
                     )
                 )
        ),

        # Tab 2: Individual Probabilities
        tabPanel("Item Probabilities",
                 br(),
                 h4("Probability of Correct Response at Current Theta"),

                 div(class = "result-box",
                     fluidRow(
                       column(4,
                              h5("Item 1", style = "color: #3498db;"),
                              h3(textOutput("prob1"))
                       ),
                       column(4,
                              h5("Item 2", style = "color: #e74c3c;"),
                              h3(textOutput("prob2"))
                       ),
                       column(4,
                              h5("Item 3", style = "color: #27ae60;"),
                              h3(textOutput("prob3"))
                       )
                     )
                 ),

                 br(),

                 h4("Probability Table Across Theta Values"),
                 tableOutput("prob_table"),

                 div(class = "question-box",
                     h4("Questions to Consider"),
                     tags$ol(
                       tags$li("At what theta value does each item have P = 0.5?"),
                       tags$li("Which item is easiest? Which is hardest?"),
                       tags$li("How does the discrimination (a) affect the steepness of the curve?"),
                       tags$li("How does the guessing parameter (c) affect the lower asymptote?")
                     )
                 )
        ),

        # Tab 3: Response Vector Probabilities
        tabPanel("Response Patterns",
                 br(),
                 h4("Probability of Response Patterns"),
                 helpText("Each response pattern shows P(Item1, Item2, Item3) where 0 = incorrect, 1 = correct."),

                 tableOutput("pattern_table"),

                 br(),

                 h4("Visualization"),
                 plotOutput("pattern_plot", height = "350px"),

                 div(class = "question-box",
                     h4("Activity"),
                     p("For the current theta value, which response pattern is most likely?"),
                     p("Move the theta slider and observe how the most likely pattern changes."),
                     tags$ul(
                       tags$li("At very low theta, which pattern dominates?"),
                       tags$li("At very high theta, which pattern dominates?"),
                       tags$li("At what theta is the pattern '010' most likely?")
                     )
                 )
        ),

        # Tab 4: Practice Questions (without answers)
        tabPanel("Practice Questions",
                 br(),

                 div(class = "info-box",
                     h4("Question 1: Compare Item 1 vs. Item 2"),
                     p("With the default parameters:"),
                     tags$ul(
                       tags$li("Item 1: a = 1, b = -1.5, c = 0.2"),
                       tags$li("Item 2: a = 1, b = 0, c = 0.2")
                     ),
                     p(strong("Which item is easier? Why?"))
                 ),

                 br(),

                 div(class = "info-box",
                     h4("Question 2: Compare Item 2 vs. Item 3"),
                     p("With the default parameters:"),
                     tags$ul(
                       tags$li("Item 2: a = 1, b = 0, c = 0.2"),
                       tags$li("Item 3: a = 2, b = 0.2, c = 0.3")
                     ),
                     p(strong("Which item is easier? Is it the same at all ability levels?"))
                 ),

                 br(),

                 div(class = "info-box",
                     h4("Question 3: The Effect of Discrimination"),
                     p("Try changing Item 3's discrimination (a) from 2 to 0.5."),
                     p(strong("What happens to the ICC? What does this mean for measurement?"))
                 )
        ),

        # Tab 5: Answers
        tabPanel("Answers",
                 br(),

                 div(class = "result-box",
                     h4("Answer 1: Compare Item 1 vs. Item 2"),
                     p("Item 1 is easier because it has a lower b parameter.
                       Since both items have the same discrimination (a = 1) and guessing (c = 0.2),
                       the item with the lower b value is easier at all ability levels.")
                 ),

                 br(),

                 div(class = "result-box",
                     h4("Answer 2: Compare Item 2 vs. Item 3"),
                     p("This comparison is more complex because the items have different
                       discrimination and guessing parameters. The ICCs cross, meaning:"),
                     tags$ul(
                       tags$li("At low ability, Item 3 appears easier (higher c parameter)"),
                       tags$li("At high ability, the relative difficulty depends on where the curves intersect")
                     ),
                     p("Adjust the theta slider to see how the relative probabilities change!")
                 ),

                 br(),

                 div(class = "result-box",
                     h4("Answer 3: The Effect of Discrimination"),
                     p("Lower discrimination means a flatter ICC. This means:"),
                     tags$ul(
                       tags$li("The item doesn't differentiate well between ability levels"),
                       tags$li("The probability changes slowly as theta changes"),
                       tags$li("The item provides less information about a person's ability")
                     )
                 )
        ),

        # Tab 5: Formulas
        tabPanel("Formulas",
                 br(),

                 h4("The 3-Parameter Logistic (3PL) Model"),

                 div(class = "info-box",
                     p("The probability of a correct response is given by:"),
                     withMathJax(),
                     p("$$P(X_i = 1 | \\theta) = c_i + (1 - c_i) \\frac{\\exp(a_i(\\theta - b_i))}{1 + \\exp(a_i(\\theta - b_i))}$$"),

                     h5("Parameters:"),
                     tags$ul(
                       tags$li(strong("a (discrimination):"), " Controls the slope/steepness of the curve"),
                       tags$li(strong("b (difficulty):"), " The location parameter; for 1PL/2PL, this is where P = 0.5"),
                       tags$li(strong("c (guessing):"), " The lower asymptote; minimum probability even with very low ability")
                     )
                 ),

                 br(),

                 h4("Probability of a Response Pattern"),

                 div(class = "info-box",
                     p("Assuming local independence, the probability of a response pattern is:"),
                     p("$$P(X_1, X_2, X_3 | \\theta) = \\prod_{i=1}^{3} P_i^{X_i} (1 - P_i)^{1-X_i}$$"),

                     p("For example, for pattern (0, 1, 0):"),
                     p("$$P(0,1,0 | \\theta) = (1 - P_1) \\times P_2 \\times (1 - P_3)$$")
                 ),

                 br(),

                 h4("Special Cases"),

                 div(class = "info-box",
                     tags$ul(
                       tags$li(strong("1PL/Rasch Model:"), " Set a = 1 for all items and c = 0"),
                       tags$li(strong("2PL Model:"), " Allow a to vary, but set c = 0")
                     )
                 )
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
    updateNumericInput(session, "a1", value = 1)
    updateNumericInput(session, "b1", value = -1.5)
    updateNumericInput(session, "c1", value = 0.2)
    updateNumericInput(session, "a2", value = 1)
    updateNumericInput(session, "b2", value = 0)
    updateNumericInput(session, "c2", value = 0.2)
    updateNumericInput(session, "a3", value = 2)
    updateNumericInput(session, "b3", value = 0.2)
    updateNumericInput(session, "c3", value = 0.3)
    updateSliderInput(session, "theta", value = 0)
  })

  # Get item parameters as reactive
  item_params <- reactive({
    list(
      a = c(input$a1, input$a2, input$a3),
      b = c(input$b1, input$b2, input$b3),
      c = c(input$c1, input$c2, input$c3)
    )
  })

  # ICC Plot
  output$icc_plot <- renderPlot({
    params <- item_params()
    theta_range <- seq(-4, 4, 0.05)

    # Calculate probabilities
    p1 <- calc_prob(theta_range, params$a[1], params$b[1], params$c[1])
    p2 <- calc_prob(theta_range, params$a[2], params$b[2], params$c[2])
    p3 <- calc_prob(theta_range, params$a[3], params$b[3], params$c[3])

    # Create data frame for ggplot
    df <- data.frame(
      theta = rep(theta_range, 3),
      prob = c(p1, p2, p3),
      Item = factor(rep(c("Item 1", "Item 2", "Item 3"), each = length(theta_range)))
    )

    ggplot(df, aes(x = theta, y = prob, color = Item)) +
      geom_line(linewidth = 1.5) +
      geom_vline(xintercept = input$theta, linetype = "dashed", color = "gray40", linewidth = 1) +
      geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray60") +
      scale_color_manual(values = c("Item 1" = "#3498db", "Item 2" = "#e74c3c", "Item 3" = "#27ae60")) +
      labs(x = expression(theta ~ "(ability)"),
           y = "P(Correct)",
           title = "Item Characteristic Curves") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      coord_cartesian(xlim = c(-4, 4), ylim = c(0, 1))
  })

  # Individual probabilities
  output$prob1 <- renderText({
    params <- item_params()
    p <- calc_prob(input$theta, params$a[1], params$b[1], params$c[1])
    sprintf("%.3f", p)
  })

  output$prob2 <- renderText({
    params <- item_params()
    p <- calc_prob(input$theta, params$a[2], params$b[2], params$c[2])
    sprintf("%.3f", p)
  })

  output$prob3 <- renderText({
    params <- item_params()
    p <- calc_prob(input$theta, params$a[3], params$b[3], params$c[3])
    sprintf("%.3f", p)
  })

  # Probability table across theta values
  output$prob_table <- renderTable({
    params <- item_params()
    theta_vals <- c(-4, -2, -1, 0, 1, 2, 4)

    data.frame(
      Theta = theta_vals,
      `P(Item 1)` = sapply(theta_vals, function(t) round(calc_prob(t, params$a[1], params$b[1], params$c[1]), 3)),
      `P(Item 2)` = sapply(theta_vals, function(t) round(calc_prob(t, params$a[2], params$b[2], params$c[2]), 3)),
      `P(Item 3)` = sapply(theta_vals, function(t) round(calc_prob(t, params$a[3], params$b[3], params$c[3]), 3)),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Response pattern table
  output$pattern_table <- renderTable({
    params <- item_params()

    # All possible patterns
    patterns <- list(
      c(0, 0, 0), c(0, 0, 1), c(0, 1, 0), c(0, 1, 1),
      c(1, 0, 0), c(1, 0, 1), c(1, 1, 0), c(1, 1, 1)
    )
    pattern_names <- c("000", "001", "010", "011", "100", "101", "110", "111")

    # Calculate for multiple theta values
    theta_vals <- c(-4, -1.1, 0, 1.6)

    result <- data.frame(Pattern = pattern_names)
    for (t in theta_vals) {
      probs <- sapply(patterns, function(p) {
        calc_vector_prob(t, p, params$a, params$b, params$c)
      })
      result[[paste0("Theta = ", t)]] <- round(probs, 4)
    }

    result
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Response pattern plot
  output$pattern_plot <- renderPlot({
    params <- item_params()

    patterns <- list(
      c(0, 0, 0), c(0, 0, 1), c(0, 1, 0), c(0, 1, 1),
      c(1, 0, 0), c(1, 0, 1), c(1, 1, 0), c(1, 1, 1)
    )
    pattern_names <- c("000", "001", "010", "011", "100", "101", "110", "111")

    probs <- sapply(patterns, function(p) {
      calc_vector_prob(input$theta, p, params$a, params$b, params$c)
    })

    df <- data.frame(
      Pattern = factor(pattern_names, levels = pattern_names),
      Probability = probs
    )

    ggplot(df, aes(x = Pattern, y = Probability, fill = Pattern)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Response Pattern",
           y = "Probability",
           title = paste0("Response Pattern Probabilities at theta = ", input$theta)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(0, max(probs) * 1.1))
  })
}

# ============================================================================
# Run the app
# ============================================================================

shinyApp(ui = ui, server = server)
