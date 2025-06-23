# /usr/bin/Rscript

# fix44
library(tidyverse)
library(readxl)
library(janitor)

clean44 <- function(mip44_path) {
  mip44 <- read_xlsx(mip44_path)
  mip44 <- mip44 |> slice(-(n() - 6):-n())
  mip44 <- mip44[, -ncol(mip44)]

  ck5 <- mip44 |>
    slice(5) |>
    as.character()

  ck5[[1]] <- "sector"
  ck5[[2]] <- "UTPB"

  mip44 <- mip44 |>
    slice(-1:-5)

  mip44_names <- ck5 |> make_clean_names()

  mip44 <- mip44 |> set_names(mip44_names)

  mip44 <- mip44 |>
    select(-total, -total_2)

  mip44 <- mip44 |>
    mutate(across(2:ncol(mip44), as.numeric))

  mip44 <- mip44 |>
    mutate(sector = make_clean_names(sector))

  mip44
}

source("simulator.R")

N_SECTORS_BR <- 35 * 2
MIPS_BR <- read_rds("data/mips_br.Rds")
STATES <- names(MIPS_BR)

Z_aug <- MIPS_BR[["sinaloa"]]
out <- get_ZALfx_multipliers(Z_aug, N_SECTORS_BR)

# cross mip44 and mip_br --------------------------------------------------

mip44 <- clean44("data/mip44_nacional_ixi_subsector_domimp.xlsx")
N_SECTORS <- 78
sector_names <- mip44$sector[1:N_SECTORS]


sector_names

cat(names(out$Z))
