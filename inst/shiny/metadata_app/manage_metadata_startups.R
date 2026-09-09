manage_startups_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h5("Search Logger ID:"),
        layout_columns(
            textInput(ns("logger_search"), label = NULL, placeholder = ""),
            actionButton(ns("submit_search_btn"), "Search"),
            col_widths = c(4, 1),
            fillable = FALSE,
            fill = TRUE,
            max_height = "2rem",
            row_height = "2rem"
        ),
        shinyjs::hidden(actionButton(ns("confirm_startup"), "Add startup")),
        uiOutput(ns("search_result")),
    )
}

manage_startups_server <- function(id, busy, all_locations, unsaved, current_location_idx, current_location_name, refresh_tables) {
    moduleServer(id, function(input, output, session) {
        # Search for logger ID
        search_results <- reactiveVal(NULL)
        dummy_models <- reactiveVal(NULL)

        observeEvent(
            {
                list(busy(), all_locations())
            },
            {
                if (busy() || length(all_locations()) == 0) {
                    shinyjs::disable("submit_search_btn")
                } else {
                    shinyjs::enable("submit_search_btn")
                }
            }
        )

        observeEvent(
            {
                input$submit_search_btn
            },
            {
                tryCatch(
                    {
                        do_search()
                    },
                    error = function(e) {
                        log_error(paste("ERROR", e), namespace = "error")
                        busy(FALSE)
                    }
                )
            }
        )

        observeEvent(
            {
                input$confirm_startup
            },
            {
                add_startup()
            }
        )

        observeEvent(
            {
                search_results()
            },
            {
                selected_cols <- c("logger_serial_no", "logger_model", "production_year", "starttime_gmt", "intended_location", "intended_species")

                startup <- search_results()
                if (is.null(startup)) {
                    shinyjs::hideElement("confirm_startup")
                    output$search_result <- renderUI({
                        h4("No results found")
                    })
                    return()
                }
                shinyjs::enable("confirm_startup")
                shinyjs::showElement("confirm_startup")
                output$search_result <- renderUI({
                    tagList(
                        h4("Search results:"),
                        DT::renderDT({
                            DT::datatable(
                                startup[, selected_cols],
                                rownames = FALSE,
                                style = "auto",
                                selection = "none",
                                options = list(
                                    dom = "t",
                                    ordering = FALSE
                                )
                            )
                        })
                    )
                })
            }
        )

        add_startup <- function() {
            startup <- search_results()
            if (is.null(startup)) {
                log_warn("No valid startup")
                return()
            }
            locations <- all_locations()
            master_import <- locations[[current_location_idx()]]

            master_import$data$`STARTUP_SHUTDOWN` <- rbind(master_import$data$`STARTUP_SHUTDOWN`, startup)
            new_locations <- modify_master_import_in_list(locations, master_import)
            all_locations(new_locations)
            unsaved(TRUE)
            refresh_tables()
            shinyjs::disable("confirm_startup")
            log_success("Added startup to master metadata")
            # APPEND TO MASTER METADATA
        }

        do_search <- function() {
            if (nchar(input$logger_search) == 0) {
                log_warn("No input")
                return()
            }
            busy(TRUE)
            logger_id <- input$logger_search
            log_info(paste("Searching for logger", logger_id))

            if (is.null(dummy_models())) {
                dummy_models(get_can_dummy_models())
            }

            # get all previoius instances of this logger from startup

            # paste  together id and date

            id_date <- paste(logger_id, Sys.Date())

            all_startups <- search_startup_files(c(logger_id), id_date, all_locations()[[current_location_idx()]]$data$`STARTUP_SHUTDOWN`)

            if (nrow(all_startups) == 0) {
                search_results(NULL)
                busy(FALSE)
                return()
            }
            logger_info <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_info"))
            logger_info <- dplyr::filter(logger_info, logger_serial_no == !!logger_id) %>% dplyr::collect()

            logger_partner_logger_data <- data.frame(date = Sys.Date(), logger_id = logger_id, model = logger_info$logger_model, deployed = FALSE)

            new_startup <- choose_startup_to_add(logger_partner_logger_data, all_startups, all_locations()[[current_location_idx()]], dummy_models())
            if (is.null(new_startup)) {
                busy(FALSE)
                search_results(NULL)
                return()
            }

            search_results(new_startup)
            busy(FALSE)
        }
    })
}
