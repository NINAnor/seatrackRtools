manage_metadata_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h5("Manage metadata:"),
        p("Select location:"),
        layout_columns(
            selectInput(
                ns("select_location"),
                NULL,
                NULL,
                selectize = TRUE
            ),
            actionButton(ns("revert_master_btn"), "Reload file"),
            col_widths = c(4, 3),
            fillable = FALSE,
        ),
        br(),
        navset_card_pill(
            id = ns("tabs"),
            nav_panel(
                title = "Master metadata",
                mod_dt_tabs_ui(ns("viewer"))
            ),
            nav_panel(
                title = "Partner metadata",
                manage_partner_metadata_ui(ns("partner"))
            ),
        )
    )
}

manage_metadata_server <- function(id, busy, all_locations, unsaved) {
    moduleServer(id, function(input, output, session) {

        current_location_idx <- reactiveVal(NA)
        current_location_name <- reactiveVal(NA)
        metadata_tables <- reactiveVal(list())
        current_location_choices <- reactiveVal(c())

        observeEvent(busy(), {
            if (busy()) {
                shinyjs::disable("select_location")
                shinyjs::disable("tabs")
                shinyjs::disable("revert_master_btn")
            } else {
                shinyjs::enable("select_location")
                shinyjs::enable("tabs")
                shinyjs::enable("revert_master_btn")
            }
        })

        observeEvent(all_locations(), {
            choices <- seq_along(names(all_locations()))
            names(choices) <- names(all_locations())
            if (length(choices) != length(current_location_choices()) || any(names(current_location_choices()) != names(choices))) {
                updateSelectInput(inputId = "select_location", choices = choices)
                current_location_choices(choices)
            }
        })

        observeEvent(input$select_location, {
            if (input$select_location != "") {
                current_location_idx(as.numeric(input$select_location))
                current_location_name(names(all_locations())[current_location_idx()])
            }
        })

        refresh_tables <- function() {
            metadata_tables(list())
            if (!is.na(current_location_name())) {
                metadata_tables(all_locations()[[current_location_idx()]]$data)
            }
        }

        observeEvent(current_location_name(), {
            refresh_tables()
            # if (!is.na(current_location_name())) {
            #     metadata_tables(all_locations()[[current_location_idx()]]$data)
            # } else {
            #     metadata_tables(list())
            # }
        })


        manage_partner_metadata <- manage_partner_metadata_server(
            "partner", busy,
            all_locations, unsaved, current_location_idx, current_location_name, refresh_tables
        )

        observeEvent(input$revert_master_btn, {
            tryCatch(
                {
                    busy(TRUE)
                    path <- all_locations()[[current_location_idx()]]$path
                    loaded_master_import <- load_master_import(file_path = path)
                    locations <- all_locations()
                    new_locations <- modify_master_import_in_list(locations, loaded_master_import)
                    modified_locations <- c()
                    seen_paths <- c()
                    for (i in seq_along(new_locations)) {
                        x <- new_locations[[i]]
                        if (!x$path %in% seen_paths) {
                            modified_locations <- c(modified_locations, x$modified)
                        }
                    }
                    all_locations(new_locations)
                    unsaved(any(modified_locations))
                    refresh_tables()
                    busy(FALSE)
                },
                ERROR = function(e) {
                    log_error(paste0("Error reloading master import sheet: ", basename(all_locations()[[current_location_idx()]]$path), " - ", e$message))
                    busy(FALSE)
                }
            )
        })

        mod_dt_tabs_server("viewer", metadata_tables)
    })
}
