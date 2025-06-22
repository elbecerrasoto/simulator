#!/bin/usr/Rscript

library(tidyverse)
source("simulator.R")

N_SECTORS <- 35 * 2
MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

Z_aug <- MIPS_BR[["nuevo_leon"]]


out <- get_ZALfx_multipliers(Z_aug, N_SECTORS)


shocks <- c(rep(1, N_SECTORS))
shocks[[8]] <- 5
L <- out$L
f <- out$f
x <- out$x

names(L)

simulate_demand_shocks(shocks,
  L,
  f,
  x,
  shocks_are_multipliers = TRUE
) |>
  view()
