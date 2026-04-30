library(shiny)
source("./helpers.R")

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("weibull",
           numericInput("shape", "Shape parameter", 0.7, min = 0),
           numericInput("scale", "Scale parameter", 200, min = 0)
  ),
  tabPanel("exponential",
           numericInput("rate", "Rate parameter", 0.005, min = 0),
  )
)

ic_tabs <- tabsetPanel(
  id = "ic_width",
  type = "hidden",
  tabPanel("No"),
  tabPanel("Yes", 
           numericInput("width", "Censoring interval width", 30, 
                        min = 1, max = 1000))
)

rc_tabs <- tabsetPanel(
  id = "rc_day",
  type = "hidden",
  tabPanel("No"),
  tabPanel("Yes",
           numericInput("followup", "Censor after day:", 365, min = 365))
)


ui <- fluidPage(
  tags$style("h2 {text-align: center; font-family: Helvetica; padding: 10px 5px 15px; background-color: hsl(0, 0%, 94%);}"),
  titlePanel("Coarsening and interval-censoring in time-to-event data"),
  sidebarLayout(
    # Sidebar for choosing the parameters
    sidebarPanel(
      # For visualization
      numericInput("N", "Observations", 2000, min = 1, max = 5000),
      selectInput("dist", "Distribution", list("weibull")),
      parameter_tabs,
      actionButton("viz", "Set parameters"),
      p(br()),
      
      # For bias evaluation
      numericInput("coar", "Coarsening interval width", 1, min = 1, max = 1),
      selectInput(
        "round",
        "Coarsening type",
        list("Rounded up", "Rounded down")
      ),
      selectInput("cens", "Interval-censored?", list("No", "Yes")),
      ic_tabs,
      selectInput("rc", "Right-censored?", list("No", "Yes")),
      rc_tabs,
      numericInput("n.iter", "Number of iterations", 1000, min = 1, max = 5000),
      radioButtons(
        inputId = "days", 
        label = "Evaluate at:", 
        choices = list("30, 60, ..., 360 days" = 1, 
                       "1 year, 2 years, ..., 5 years" = 2)
      ),
      actionButton("click", "Evaluate bias"),
      style = "background-color: white; border: solid; border-radius: 0px; font-family: Monospace;"
    ),
    # Main panel with plots
    mainPanel(
      htmlOutput("instructions"),
      plotOutput("survival"),
      plotOutput("surv.bias"),
      tableOutput("par.bias")
      )
  )
)

server <- function(input, output, session) {
  # Display instructions
  output$instructions <- renderUI({
    HTML("Press \"Visualize\" to display the distribution.<br/>
    Press \"Evaluate Bias\" to plot the bias of survival estimates and distribution parameters.<br/>
    If the number of iterations is large, the plots may take a while to load.")
  })
  
  # Update the parameter tabs user sees based on their choice of distribution
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  })
  
  # Only show the choice for the censoring interval width if interval-censored
  observeEvent(input$cens, {
    updateTabsetPanel(inputId = "ic_width", selected = input$cens)
  })
  
  # Only show the choice for follow-up length if right-censored
  observeEvent(input$rc, {
    updateTabsetPanel(inputId = "rc_day", selected = input$rc)
  })
  
  # Plot the survival curve when the user presses the "Visualize" button
  surv_prob <- eventReactive(input$viz, {
    switch(input$dist,
           "weibull" = 1 - pweibull(1:2000, input$shape, input$scale),
           "exponential" = 1 - pexp(1:2000, input$rate))
  })
  
  output$survival <- renderPlot({
    surv <- surv_prob()
    plot(1:2000, surv, xlim = c(0, 1826), pch = 20, cex = 0.5, xaxt = "n",
         xlab = "", ylab = "Survival probability", main = "Survival curve",
         ylim = c(0, 1))
    axis(1, at = c(0, 365, 1826), labels = c("0", "1 year", "5 years"))
    points(c(365, 1826), surv[c(365, 1826)], col = "firebrick", pch = 9)
    text(c(365, 1826), 
         surv[c(365, 1826)], 
         labels = round(surv[c(365, 1826)], 2), 
         pos = 3)
  })
  
  # Plot the graphs for bias when the user presses the "Evaluate Bias" button
  pars <- eventReactive(input$click, {
    switch(input$dist,
           "weibull" = c("shape" = input$shape, "scale" = input$scale),
           "exponential" = c("rate" = input$rate))
  })
  
  df.pars <- eventReactive(input$click, {
    list(
      "n" = input$N,
      "dist" = input$dist,
      "pars" = pars(),
      "duration" = 0,
      "followup" = if(input$rc == "No") Inf else input$followup,
      "width" = if(input$cens == "Yes") input$width else input$coar,
      "rounding" = input$round
    )
  })
  
  days <- eventReactive(input$click, {
    switch(input$days,
           "1" = c(30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360),
           "2" = c(365, 730, 1095, 1461, 1826))
  })

  estimates <- reactive({
    dist.pars <- names(df.pars()[["pars"]])
    repeat_estimation(isolate(input$n.iter), days(), df.pars(), dist.pars)
  })

  P <- eventReactive(input$click, {
    switch(input$dist,
           "weibull" = 1 - pweibull(days(), input$shape, input$scale),
           "exponential" = 1 - pexp(days(), input$rate))
    })

  output$surv.bias <- renderPlot({
    prob.bias <- average_bias(estimates()[["prob"]], P())
    plot_bias(prob.bias,
              c("darkorange", "red", "brown", "forestgreen", "royalblue", "cyan3"),
              paste0("Bias for est. S(t), \ninterval width = ", df.pars()[["width"]], " days"))
  })

  output$par.bias <- renderTable({
    par.bias <- average_bias(estimates()[["pars"]], df.pars()[["pars"]])
    par.bias
  }, rownames = TRUE, align = "c", digits = 5)
}

shinyApp(ui, server)