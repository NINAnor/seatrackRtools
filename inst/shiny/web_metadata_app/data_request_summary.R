dat_req_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        shinydashboard::box(
            uiOutput(ns("last_updated")),
            width = 12
        ),
        shinydashboard::box(
            DT::dataTableOutput(ns("Table1")),
            width = 12
        ),
        shinydashboard::box(
            uiOutput(ns("dict")),
            width = 12
        )
    )
}

dat_req_server <- function(id, species_df) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        dat_req <- readr::read_csv("data/SEATRACK data_request_summary.csv", show_col_types = FALSE)
        dat_req$`Study species` <- " "
        species_cols <- which(names(dat_req) %in% species_df$abrv)
        for (i in 1:nrow(dat_req)) {
            species_i <- species_df$abrv[dat_req[i, species_cols] %in% "x"]

            dat_req$`Study species`[i] <- stringr::str_c(sort(species_i), collapse = ", ")
        }
        dat_req <- dat_req[, c("ID", "Principal Investigator", "Project title", "Study species", "Study area", "ECR project", "data request", "aim")]
        dat_req$`data request` <- as.Date(dat_req$`data request`, format = "%d/%m/%Y")
        dat_req <- dat_req[order(dat_req$ID, decreasing = TRUE), ]

        output$dict <- renderUI(p(HTML(stringr::str_c(paste(species_df$abrv, species_df$eng, sep = " - "), collapse = ", "))))

        # Extract the last modification date
        dat_req_last_modification_date <- file.info("data/SEATRACK data_request_summary.csv")$mtime

        output$last_updated <- renderUI(p(
            HTML(
                paste0("An archive of all approved data requests (last updated: ", as.Date(dat_req_last_modification_date), ")")
            )
        ))


        # Create shortened clickable hyperlinks
        dat_req$aim <- sapply(dat_req$aim, function(url) {
            if (grepl("^https://", url)) {
                paste0('<a href="', url, '" target="_blank">Publication</a>') # "Link" text for https
            } else {
                url # Keep the original URL for non-https links
            }
        })

        output$Table1 <- DT::renderDT({
            DT::datatable(
                dat_req,
                escape = FALSE, # Allow HTML to be rendered
                rownames = FALSE,
                selection = "none",
                class = "stripe row-border order-columns",
                options = list(
                    pageLength = 5,
                    autoWidth = TRUE, # Automatically adjust column width
                    scrollX = TRUE,
                    columnDefs = list(
                        list(
                            targets = 0,
                            width = "5%"
                        ),
                        list(
                            targets = 1,
                            width = "10%"
                        ),
                        list(
                            targets = 2,
                            width = "15%"
                        ),
                        list(
                            targets = 3,
                            width = "10%"
                        ),
                        list(
                            targets = c(4:6),
                            width = "5%"
                        ),
                        list(
                            targets = 7,
                            width = "10%"
                        )
                    )
                )
            )
        })
    })
}
