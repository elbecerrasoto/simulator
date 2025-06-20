library(shiny)
library(readr)
library(rlang)
source("simulator.R")

# ---- globals

N_SECTORS <- 35 * 2
MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

# current_state <- get_ZALfx_multipliers(
#   MIPS_BR[["morelia"]],
#   N_SECTORS
# )
#
# simulate_demand_shocks(rep(2.0, N_SECTORS),
#   current_state$L,
#   current_state$f,
#   current_state$x,
#   shocks_are_multipliers = TRUE
# )

# ---- app

ui <- fluidPage(
  selectInput("state", "Choose state to simulate:", STATES),
  sliderInput("shock_multiplier", "Increase on final demand by sector.\n(Multiply by this amount.)",
    value = 1.01, min = 0.50, max = 2.00
  ),
  dataTableOutput("multipliers")
)

server <- function(input, output, session) {
  Z_aug <- reactive(MIPS_BR[[input$state]])
  ZALfx_multipliers <- reactive(get_ZALfx_multipliers(Z_aug(), N_SECTORS))

  L <- reactive(ZALfx_multipliers()$L)
  f <- reactive(ZALfx_multipliers()$f)
  x <- reactive(ZALfx_multipliers()$x)
  multipliers <- reactive(ZALfx_multipliers()$multipliers)

  output$multipliers <- renderDataTable(multipliers())

  shock_multipliers <- reactive(rep(input$shock_multiplier, N_SECTORS))
  results <- reactive(
    simultate_demand_shocks(
      shock_multipliers(),
      L(),
      f(),
      x(),
      shocks_are_mulipliers = TRUE
    )
  )
}

shinyApp(ui, server)
