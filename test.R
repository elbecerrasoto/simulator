#!/bin/usr/Rscript

library(tidyverse)
source("simulator.R")

N_SECTORS <- 35 * 2
MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

Z_aug <- MIPS_BR[["sinaloa"]]


out <- get_ZALfx_multipliers(Z_aug, N_SECTORS)


sinaloa_imss <- read_csv("machado/sinaloa_imss.csv")
sinaloa_resto_imss <- read_csv("machado/sinaloa_resto_imss.csv")
sectors <- names(out$Z)
sectors
out$Z

library(janitor)
sinalo <- sinaloa_imss |> clean_names()
sinaloa_imss
#
# sinaloa_imss |>
#   pivot_longer |>

employment_top <- sinaloa_imss |>
  pivot_longer(everything()) |>
  mutate(name = str_c("sinaloa_", name) |>
    make_clean_names()) |>
  set_names(c("sector", "employment"))

tail(names(out$Z))

employment_bottom <- sinaloa_resto_imss |>
  pivot_longer(everything()) |>
  mutate(name = str_c("resto_del_pais_", name) |>
    make_clean_names()) |>
  set_names(c("sector", "employment"))


employment_sinaloa <- bind_rows(employment_top, employment_bottom)

employment_sinaloa |> write_tsv("employment_sinaloa.tsv")

employment_bottom

# Play machado data

simulate_employment <-
  function(shocks, L, f_old, x_old,
           shocks_are_multipliers = FALSE,
           shocks_are_total_demand = FALSE) {
    Lm <- L |> tib2mat()

    if (shocks_are_multipliers) {
      f_new <- shocks * f_old
    } else if (shocks_are_total_demand) {
      f_new <- shocks
    } else {
      f_new <- shocks + f_old
    }

    x_new <- Lm %*% f_new |> as.numeric()
    delta <- x_new - x_old
    delta_rel <-
      if_else(x_old != 0, (delta + x_old) / x_old, (delta + x_old) / 1)

    tibble(f_new, x_new, delta_rel, delta, f_old, x_old, sector = names(L))
  }
