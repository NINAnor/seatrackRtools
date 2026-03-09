# Should move this to the database ast some point
# Define species groups and their colours
species_groups <- rbind(
    data.frame(type = "divers", species = c(
        "Atlantic puffin", "Br\u00FCnnich's guillemot", "Common guillemot",
        "Little auk", "Razorbill"
    ), colour = "#99B8E1"),
    data.frame(type = "surface_divers", species = c(
        "Arctic tern", "Black-legged kittiwake", "Great skua",
        "Leach's Storm Petrel", "Northern fulmar", "Northern gannet"
    ), colour = "#C9D9EF"),
    data.frame(
        type = "gull", species = c(
            "Glaucous gull", "Herring gull",
            "Lesser black-backed gull"
        ),
        colour = "#FDECCE"
    ),
    data.frame(type = "shag", species = c("European shag"), colour = "#FCC363"),
    data.frame(type = "eider", species = c("Common eider"), colour = "#F8A482"),
    data.frame(type = "total", species = "Total", colour = "white")
)

usethis::use_data(species_groups, overwrite = TRUE)

table_bg <- list(
    deploy_bg = "#eae4df",
    retrieved_bg = "#d6c9c2",
    lme_bg = "#BDD6D5",
    header = "#69A4A4",
    footer = "#D2D3D5"
)

usethis::use_data(table_bg, overwrite = TRUE)
