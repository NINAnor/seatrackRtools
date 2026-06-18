run_web_metadata_app <- function() {
    app_dir <- system.file("shiny/web_metadata_app", package = "seatrackRtools")
    if (app_dir == "") stop("Could not find Shiny app directory.", call. = FALSE)
    shiny::runApp(app_dir)
}

# generate summary files func
get_web_metadata_data <- function() {
    SEATRACK_species <- c(
        "Herring gull", "Lesser black-backed gull", "Glaucous gull",
        "Northern fulmar", "Northern gannet", "Black-legged kittiwake", "Great skua", "Arctic tern", "Leach's storm petrel",
        "Razorbill", "Atlantic puffin", "Common guillemot", "Brünnich's guillemot", "Little auk",
        "European shag", "Common eider"
    )
    latin_species <- c(
        "Larus argentatus", "Larus fuscus", "Larus hyperboreus",
        "Fulmarus_glacialis", "Morus bassanus", "Rissa_tridactyla", "Stercorarius_skua", "Sterna paradisaea", "Hydrobates_leucorhous",
        "Alca_torda", "Fratercula_arctica", "Uria_aalge", "Uria_lomvia", "Alle_alle",
        "Gulosus aristotelis", "Somateria mollissima"
    )
}
# push to host func
