# Define UI for random distribution application

source("helpers.R")

shinyUI(fluidPage(

  # Application title
  titlePanel("§ 1092. Straddles"),
  wellPanel(tags$div("library(shiny); runGitHub(repo='tlfsp', username='pyz4', subdir='05_straddle')")),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
        numericInput(inputId="discount_rate", label="Discount Rate (annualized)", value=".025"),
        numericInput(inputId="marginal_rate", label="Marginal Tax Rate", value=".35"),
        numericInput(inputId="capital_gains_rate", label="Capital Gains Rate", value=".15"),
        numericInput(inputId="portfolio_size", label="Portfolio Notional ($)", value=1),
        textAreaInput(inputId="portfolio"
            , label="Portfolio Constituents and Weights"
            , value=default_portfolio
            , rows=10),
        numericInput(inputId="hedge_size", label="Hedge Notional ($)", value=1),
        textAreaInput(inputId="hedge"
            , label="Hedge Constituents and Weights"
            , value=default_portfolio
            , rows=10),
        actionButton("backtest", "Backtest")
      # selectInput(inputId = "obs",
      #             label = "Observations:",
      #             choices = c(10, 50, 100, 250, 500, 1000),
      #             selected = 100),
      # br(),
      # sliderInput(inputId = "theta1", label = ("MA coef 1"),
      #             min = -1.05, max = 1.05, value = 0, step = 0.01),
      # br(),
      # sliderInput(inputId = "theta2", label = ("MA coef 2"),
      #             min = -1.05, max = 1.05, value = 0, step = 0.01),
      # br(),
      # sliderInput("lag",
      #             "ACF maximum lag:",
      #             value = 30,
      #             min = 10,
      #             max = 200),
      # br(),
      # actionButton("goButton", "Simulate")
    ),

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
            tabPanel("Traceplot"
                , plotOutput("returns")
                , plotOutput("straddle")
                , plotOutput("tax_savings")
                , plotOutput("realized_gains")
            )            
            , tabPanel("26 CFR 1.246-5(b)"
                , plotOutput("correlations")
                , wellPanel(textOutput("substantial_overlap_text"))
                , dataTableOutput("substantial_overlap"))
            , tabPanel("Assumptions"
                , tags$div("The algorithm operates as follows:"
                    , tags$ol(
                        tags$li("Take equal long and short exposures in the portfolio and hedge, respectively")
                        , tags$li("Hold the long position")
                        , tags$li("At the end of each year, recognize losses in the hedge, if any. If there are no losses, continue holding.")
                        , tags$li("If losses are recognized, immediately reopen the hedge sized to the same as the long position.")
                        , tags$li("At the end, close out all positions and recognize both gains and losses in both legs of the spread accordingly.")
                    ))
                , tags$div("The backtest assumes the following:"
                    , tags$ul(
                        tags$li("Transaction costs and slippage are negligible")
                        , tags$li("Discount rate is constant")
                        , tags$li("Positions can be sized with arbitrary precision")
                        , tags$li("Tax rates remain constant")
                    )))
            # tabPanel("Partial autoc.", plotOutput("pacf"))

      )
    )
  )
))
