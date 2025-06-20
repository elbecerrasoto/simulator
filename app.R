library(shiny)
library(readr)
library(rlang)
source("simulator.R")

# ---- globals

N_SECTORS <- 35 * 2
MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

x <- get_ZALfx_multipliers(MIPS_BR[["sinaloa"]], N_SECTORS)

# ---- app

ui <- fluidPage()

server <- function(input, output, session) {}

shinyApp(ui, server)
