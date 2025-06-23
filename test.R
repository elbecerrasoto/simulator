#!/bin/usr/Rscript

library(tidyverse)
source("simulator.R")

N_SECTORS <- 35 * 2
MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

Z_aug <- MIPS_BR[["sinaloa"]]
out <- get_ZALfx_multipliers(Z_aug, N_SECTORS)

E_imss <- read_tsv("data/employment_sinaloa.tsv")
e <- if_else(x != 0, E_imss$employment / out$x, E_imss$employment)
e_hat <- diag(e)
Tm <- e_hat %*% tib2mat(out$L)
