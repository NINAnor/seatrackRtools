species.param <- openxlsx2::read_xlsx(system.file(file.path("xlsx", "irma_species_info.xlsx"), package = "seatrackRtools"))
species.param$species_param_version <- format(Sys.time(), "%Y%m%d")
usethis::use_data(species.param, overwrite = TRUE)
