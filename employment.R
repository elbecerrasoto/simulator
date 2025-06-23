#!/bin/usr/Rscript

# draft simulator

library(tidyverse)
source("simulator.R")


CURRENT <- "sinaloa"
N_SECTORS <- 35 * 2
MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

Z_aug <- MIPS_BR[[CURRENT]]
out <- get_ZALfx_multipliers(Z_aug, N_SECTORS)

E_imss <- read_tsv("data/employment_sinaloa_imss.tsv")
# e <- if_else(out$x != 0, E_imss$employment / out$x, E_imss$employment)
# e_hat <- diag(e)
# Tm <- e_hat %*% tib2mat(out$L)

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


resto_E <- (pmex - pstate) * employment_rate * employment_structure
state_E <- pstate * employment_rate * employment_structure
