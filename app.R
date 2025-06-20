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
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText(input$state)
}

shinyApp(ui, server)
