library(shiny)
library(readr)
library(rlang)

# ---- globals

MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

x <- get_Z_A_L_f_x_multipliers(state)


# ---- app

ui <- fluidPage()

server <- function(input, output, session) {}

shinyApp(ui, server)
