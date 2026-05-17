library(shiny)
source("./helpers.R")

parameterTabs <- tabsetPanel(
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

icTabs <- tabsetPanel(
  id = "ic.width",
  type = "hidden",
  tabPanel("No"),
  tabPanel("Yes", 
           numericInput("width", "Censoring interval width", 30, 
                        min = 1, max = 1000))
)

rcTabs <- tabsetPanel(
  id = "rc.day",
  type = "hidden",
  tabPanel("No"),
  tabPanel("Yes",
           numericInput("followup", "Censor after day:", 365, min = 365))
)

style.str <- "
h2 {
    font-family: Avenir; 
    padding: 10px 5px 15px; 
    background-color: hsl(0, 0%, 94%);
}

h4 {
    font-family: Avenir;
}

p {
    font-family: Avenir;
}

.pdd {
    padding: 10px;
}

.section {
    background-color: hsl(0, 0%, 94%);
    padding: 10px;
}
"


ui <- fluidPage(
  tags$style(style.str),
  titlePanel("Coarsening and interval-censoring in time-to-event data"),
  div(
    p("This application illustrates model bias for different parametric and non-parametric methods for analyzing interval-censored and coarsened data.")
  ),
  hr(),
  sidebarLayout(
    # Sidebar for choosing the parameters
    sidebarPanel(
      h4("Simulation parameters"),
      hr(),
      # For visualization
      numericInput("N", "Observations", 2000, min = 1, max = 5000),
      selectInput("dist", "Distribution", list("weibull")),
      parameterTabs,
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
      icTabs,
      selectInput("rc", "Right-censored?", list("No", "Yes")),
      rcTabs,
      numericInput("n.iter", "Number of iterations", 200, min = 1, max = 5000),
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
      div(
        h4("Instructions:"),
        HTML("<ol>
               <li>Press \"Set parameters\" to fix the parameters and display the distribution.</li>
               <li>Press \"Evaluate bias\" to calculate the bias of survival estimates and distribution parameters. If the number of iterations is large (>500), the plots may take a while to load.</li>
            </ol>"),
        style = "border: solid; border-radius: 0px; padding: 10px; font-family: Monospace; "
      ),
      h4("Plot of the survival function for the distribution", class = "section"),
      div(
        textOutput("surv.desc"),
        plotOutput("survival"),
        class = "pdd"
      ),
      h4("Bias evaluation", class = "section"),
      div(
        textOutput("tbl.desc"),
        div(tableOutput("par.bias"), style = "padding: 10px 5px 0px"),
        class = "pdd"
      ),
      div(
        htmlOutput("plt.desc"),
        plotOutput("surv.bias"),
        class = "pdd"
      )
    )
  ),
  div(
    class = "footer",
    hr(),
    HTML("<footer>Created by Ugne Milasiunaite, last updated on 17/05/2026: <a href=\"https://github.com/ugmi/ShinyThesis\">source code</a>.</footer>"),
    style = "padding: 10px; text-align: right; font-family: Avenir;"
  )
)

server <- function(input, output, session) {
  # Update the parameter tabs user sees based on their choice of distribution
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  })
  
  # Only show the choice for the censoring interval width if interval-censored
  observeEvent(input$cens, {
    updateTabsetPanel(inputId = "ic.width", selected = input$cens)
  })
  
  # Only show the choice for follow-up length if right-censored
  observeEvent(input$rc, {
    updateTabsetPanel(inputId = "rc.day", selected = input$rc)
  })
  
  # Plot the survival curve when the user presses the "Visualize" button
  surv.prob <- eventReactive(input$viz, {
    switch(input$dist,
           "weibull" = 1 - pweibull(1:2000, input$shape, input$scale),
           "exponential" = 1 - pexp(1:2000, input$rate))
  })
  
  txt.surv <- eventReactive(input$viz, {
    paste0(
      "The plot below displays the survival function for Weibull distribution ",
      "with shape parameter ", input$shape, 
      " and scale parameter ", input$scale, "."
    )
  })
  
  output$surv.desc <- renderText({ txt.surv() })
  
  output$survival <- renderPlot({
    surv <- surv.prob()
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
  
  txt.tbl <- eventReactive(input$click, {
    paste0(
      "The table below shows the parameter bias over ", 
      input$n.iter, " iterations. ", 
      "For each iteration, we simulated ", input$N, 
      " survival times distributed according to the ",
      "Weibull distribution with shape parameter ", input$shape, 
      " and scale parameter ", input$scale, 
      ", with coarsening fixed at 1 day ",
      "(rounded ", substring(input$round, 8), "). ",
      "The censoring interval width is ", 
      if(input$cens == "Yes") input$width else input$coar, " day."
    )
  })
  
  output$tbl.desc <- renderText({ txt.tbl() })
  
  output$par.bias <- renderTable({
    par.bias <- average_bias(estimates()[["pars"]], df.pars()[["pars"]])
    par.bias
  }, rownames = TRUE, align = "c", digits = 5, hover = TRUE, width = "100%", bordered = TRUE)
  
  txt.plt <- eventReactive(input$click, {
    paste0(
      "The plot below shows the bias averaged over ", input$n.iter, 
      " simulations performed as described above, for six different models:",
      "<ol>
          <li>KM.MID: Kaplan-Meier with midpoint imputation,</li>
          <li>KM.IGN: Kaplan-Meier with upper limit of the censoring interval treated as the observed event time, i.e., the censoring is ignored,</li>
          <li>NPMLE: Turnbull estimate,</li>
          <li>MID: Weibull parametric model with midpoint imputation,</li>
          <li>IGN: Weibull parametric model ignoring the censoring,</li>
          <li>IC: Weibull parametric model accounting for interval-censoring.</li>
      </ol>"
    )
  })
  
  output$plt.desc <- renderUI({ HTML(txt.plt()) })

  output$surv.bias <- renderPlot({
    prob.bias <- average_bias(estimates()[["prob"]], P())
    plot_bias(prob.bias,
              c("darkorange", "red", "brown", "forestgreen", "royalblue", "cyan3"),
              paste0("Bias for est. S(t), \ninterval width = ", df.pars()[["width"]], " days"))
  })
  
}

shinyApp(ui, server)