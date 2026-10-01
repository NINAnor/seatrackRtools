folder_selector_ui <- function(id) {
    ns <- NS(id)
    tagList(
        layout_columns(
            verbatimTextOutput(ns("text")),
            fillable = TRUE,
            row_heights = "40%",
            min_height = "40%", max_height = "40%"
        ),
        layout_columns(
            shinyDirButton(ns("select_folder_btn"), "Select Folder", "Choose folder"),
            actionButton(ns("get_locations"), "Refresh"),
            fill = TRUE,
            fillable = TRUE,
            col_widths = c(6, 6),
            row_heights = "20%", min_height = "20%", max_height = "20%"
        )
    )
}

folder_selector_server <- function(id, busy, all_locations, app_settings) {
    moduleServer(id, function(input, output, session) {
        volumes <- c(Home = fs::path_home(), getVolumes()())
        shinyDirChoose(input, "select_folder_btn",
            roots = volumes, session = session,
            allowDirCreate = FALSE
        )

        folder <- reactiveVal(NULL)

        observeEvent(app_settings(),
            {
            app_settings_list <- app_settings()
            if (!is.null(app_settings_list$sea_track_folder)) {
                folder(app_settings_list$sea_track_folder)
            }
            },
            once = TRUE
        )


        observeEvent(busy(), {
            if (busy()) {
                shinyjs::disable("select_folder_btn")
                shinyjs::disable("get_locations")
            } else {
                shinyjs::enable("select_folder_btn")
                shinyjs::enable("get_locations")
            }
        })

        folder_check <- reactive({
            if (!is.null(folder()) || length(folder()) > 0) {
                shinyjs::enable("get_locations")
                return(folder())
            } else {
                shinyjs::disable("get_locations")
                return("No value set")
            }
        })

        observeEvent(input$select_folder_btn, {
            path <- shinyFiles::parseDirPath(volumes, input$select_folder_btn)
            # log$add(paste("Selected folder:", p))
            if (length(path) > 0) {
                folder(path)
            } else {
                folder(NULL)
            }
        })

        get_locations <- function(clear = FALSE) {
            new_path <- folder()
            set_sea_track_folder(new_path, save_path = FALSE)

            app_settings_list <- app_settings()
            app_settings_list$sea_track_folder <- new_path
            app_settings(app_settings_list)

            # Setting seatrack path resets the folder structure
            # If there is an existing folder structure
            if (!is.null(app_settings_list$master_sheet_paths)) {
                loaded_folder_structure <- app_settings_list$master_sheet_paths
            } else {
                loaded_folder_structure <- list()
            }

            correct_path <- all(sapply(loaded_folder_structure, function(x) {
                grepl(new_path, loaded_folder_structure, fixed = TRUE)
            })) && length(loaded_folder_structure) > 0

            # If we are not forcing a reset, the folders structure is for the correct sea_track path and there is a folder structure
            if (!clear && correct_path) {
                set_master_import_paths(loaded_folder_structure)
                log_info("Using existing folder structure")
            }

            all_locations(list())
            if (!is.null(new_path)) {
                locations_path <- file.path(new_path, "Locations")


                if (dir.exists(locations_path)) {
                    log_info("Loading locations")
                    busy(TRUE)
                    log_names <- getShinyOption("app_log_names")
                    log_path <- getShinyOption("logging_path")
                    future <- future_promise({
                        setup_app_logs(silent = FALSE, log_path = log_path, log_names = log_names)
                        set_sea_track_folder(new_path, save_path = FALSE)

                        if (!clear && correct_path) {
                            set_master_import_paths(loaded_folder_structure)
                        }

                        all_files <- load_all_master_import(
                            combine = FALSE, skip = c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed", "no_location_not_processed", "FROM MARK MALLORY_HISTORIC ARTE"), 
                            distinct = FALSE, 
                            use_stored = !clear
                        )

                        master_sheet_paths <- tryCatch({
                            get_master_import_paths()
                        }, error = function(e) {
                            log_error(e$message)
                        })
                        

                        return(list(all_files = all_files, master_sheet_paths = master_sheet_paths))
                    }) |>
                        then(function(x) {
                            all_locations(x$all_files)
                            app_settings_list <- app_settings()
                            app_settings_list$master_sheet_paths <- x$master_sheet_paths
                            app_settings(app_settings_list)
                        }) |>
                        finally(\() {
                            all_locations(all_locations()[order(names(all_locations()))])
                            log_success("Finished loading locations")
                            busy(FALSE)
                        })
                    load_promise(future)
                } else {
                    log_error("No location folder in seatrack folder")
                }
            }

        }

        load_promise <- reactiveVal(NULL)
        observeEvent(
            folder(),
            {
                get_locations()
            }
        )

        observeEvent(
            input$get_locations,
            {
                get_locations(TRUE)
            }
        )

        output$text <- renderText({
            folder_check()
        })
        return(list(folder = reactive(folder())))
    })
}
