manage_db_upload_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h5("Upload to database:"),
        actionButton(ns("data_upload_all"), "Upload all"),
        p("Select sheet:"),
        layout_columns(
            selectInput(
                ns("select_sheet"),
                NULL,
                NULL,
                selectize = TRUE
            ),
            actionButton(ns("revert_master_btn"), "Reload file"),
            col_widths = c(4, 3),
            fillable = FALSE,
        ),
        br(),
        uiOutput(ns("message")),
        actionButton(ns("test_prep"), "Test data preparation"),
        actionButton(ns("test_upload"), "Dry run data upload"),
        actionButton(ns("data_upload"), "Data upload"),
    )
}

manage_db_upload_server <- function(id, busy, all_locations, unsaved) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        set_sheet <- function() {
            current_path <- current_sheet()$path
            current_sheet_name <- basename(current_path)

            output$message <- renderUI({
                p(paste(current_sheet_name, "selected."))
            })
            shinyjs::enable("test_prep")
            shinyjs::enable("test_upload")
            shinyjs::enable("data_upload")
        }

        current_sheet_idx <- reactiveVal(NA)
        current_sheet <- reactiveVal(NA)
        current_choices <- reactiveVal(c())

        observeEvent(busy(), {
            if (busy()) {
                shinyjs::disable("select_sheet")
                shinyjs::disable("test_prep")
                shinyjs::disable("test_upload")
                shinyjs::disable("data_upload")
                shinyjs::disable("data_upload_all")
                shinyjs::disable("revert_master_btn")
            } else {
                shinyjs::enable("select_sheet")
                shinyjs::enable("test_prep")
                shinyjs::enable("test_upload")
                shinyjs::enable("data_upload")
                shinyjs::enable("data_upload_all")
                shinyjs::enable("revert_master_btn")
            }
        })

        observeEvent(all_locations(), {
            locations <- all_locations()
            if (length(locations) == 0) {
                return()
            }
            busy(TRUE)
            unique_sheet_idx <- c()
            seen_paths <- c()
            for (i in seq_along(locations)) {
                x <- locations[[i]]
                if (!x$path %in% seen_paths) {
                    unique_sheet_idx <- c(unique_sheet_idx, i)
                    seen_paths <- c(seen_paths, x$path)
                }
            }

            choices <- unique_sheet_idx
            names(choices) <- sapply(seen_paths, basename)
            choices <- choices[order(names(choices))]
            if (length(choices) != length(current_choices()) || any(paste(names(choices), choices) != paste(names(current_choices()), current_choices()))) {
                current_choices(choices)
                updateSelectInput(inputId = "select_sheet", choices = choices)
            }

            busy(FALSE)
        })

        observeEvent(input$select_sheet, {
            if (input$select_sheet != "") {
                sheet_idx <- as.numeric(input$select_sheet)
                print(sheet_idx)
                current_sheet_idx(sheet_idx)
                current_sheet(all_locations()[[current_sheet_idx()]])
                set_sheet()
            } else {
                output$message <- renderUI({
                    p(paste("No sheet selected."))
                })
                shinyjs::disable("test_prep")
                shinyjs::disable("test_upload")
                shinyjs::disable("data_upload")
                shinyjs::disable("data_upload_all")
            }
        })



        observeEvent(input$test_prep, {
            busy(TRUE)
            prepped_data <- tryCatch(
                prepare_master_sheet_for_db(current_sheet()),
                error = function(e) {
                    log_error(paste("ERROR", e), namespace = "error")
                    return(NULL)
                }
            )

            print(prepped_data)
            busy(FALSE)
        })

        observeEvent(input$test_upload, {
            busy(TRUE)

            tryCatch(
                push_master_import_file_to_db(master_sheets = current_sheet(), dry_run = TRUE),
                error = function(e) {
                    log_error(paste("ERROR", e), namespace = "error")
                    return(NULL)
                }
            )

            busy(FALSE)
        })

        observe({
            showModal(
                modalDialog(
                    "This will push data to the database! Are you sure?",
                    br(),
                    layout_columns(
                        actionButton(paste("main", ns("confirm_upload"), sep = "-"), "Do it."),
                        actionButton(paste("main", ns("cancel_upload"), sep = "-"), "Don't do it")
                    ),
                    title = "Confirm upload?",
                    easyClose = TRUE,
                    footer = NULL
                )
            )
        }) |>
            bindEvent(input$data_upload)

            observe({
                showModal(
                    modalDialog(
                        "This will try to push ALL data to the database! This might be quite hard to follow. Are you sure?",
                        br(),
                        layout_columns(
                            actionButton(paste("main", ns("confirm_upload_all"), sep = "-"), "Do it."),
                            actionButton(paste("main", ns("cancel_upload"), sep = "-"), "Don't do it")
                        ),
                        title = "Confirm upload ALL ?",
                        easyClose = TRUE,
                        footer = NULL
                    )
                )
            }) |>
                bindEvent(input$data_upload_all)

            # Upload data
            observe({
                busy(TRUE)

                tryCatch(
                    push_master_import_file_to_db(master_sheets = current_sheet(), dry_run = FALSE),
                    error = function(e) {
                        log_error(paste("ERROR", e), namespace = "error")
                        return(NULL)
                    }
                )

                busy(FALSE)

                removeModal()
            }) |>
                bindEvent(input$confirm_upload)

            # Upload all data
            observe({
                busy(TRUE)
                for (i in current_choices()) {
                    tryCatch(
                        push_master_import_file_to_db(master_sheets = all_locations()[i], dry_run = FALSE),
                        error = function(e) {
                            log_error(paste("ERROR", e), namespace = "error")
                            return(NULL)
                        }
                    )
                }
                busy(FALSE)

                removeModal()
            }) |>
                bindEvent(input$confirm_upload_all)

        observe({
            removeModal()
        }) |>
            bindEvent(input$cancel_upload)

        observeEvent(input$revert_master_btn, {
            busy(TRUE)
            tryCatch(
                {
                    path <- current_sheet()$path
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
                    current_sheet(all_locations()[[current_sheet_idx()]])
                    set_sheet()
                },
                error = function(e) {
                    log_error(paste("ERROR", e), namespace = "error")
                }
            )
            busy(FALSE)
        })
    })
}
