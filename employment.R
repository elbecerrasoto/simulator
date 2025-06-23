#!/bin/usr/Rscript

# draft simulator

library(tidyverse)
source("simulator.R")


CURRENT <- "chiapas"
N_SECTORS <- 35 * 2
MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

Z_aug <- MIPS_BR[[CURRENT]]
out <- get_ZALfx_multipliers(Z_aug, N_SECTORS)

E_imss <- read_tsv("data/employment_sinaloa_imss.tsv")

population <- read_tsv("data/population_2020.tsv")
pmex <- population$n |> sum()

resto_employment <- E_imss |>
  filter(str_detect(sector, "^resto_del_pais")) |>
  pull(employment)

employment_rate <- 60e6 / pmex
employment_structure <- resto_employment / sum(resto_employment)

pstate <- population |>
  filter(state == CURRENT) |>
  pull(n)

state_E <- pstate * employment_rate * employment_structure
resto_E <- (pmex - pstate) * employment_rate * employment_structure
E <- c(state_E, resto_E)

get_T <- function(E, L, x) {
  e <- if_else(x != 0, E / x, E)
  e_hat <- diag(e)
  Tm <- e_hat %*% tib2mat(L)

  as_tibble(Tm) |> set_names(names(L))
}

Ttib <- get_T(E, out$L, out$x)

shocks <- rep(1.0, length(out$x))
shocks[[22]] <- 2.0

simulate_demand_shocks(shocks, out$L, out$f, out$x, shocks_are_multipliers = TRUE)
