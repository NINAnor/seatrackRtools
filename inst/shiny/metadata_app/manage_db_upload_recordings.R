db_upload_recording_ui <- function(id) {
    ns <- NS(id)
    tagList(
        actionButton(ns("data_upload_all"), "Upload all"),
    )
}

db_recordings_server <- function(id, busy, all_locations, unsaved) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {

        observeEvent(busy(), {
            if (busy()) {
                shinyjs::disable("data_upload_all")
            } else {
                shinyjs::enable("data_upload_all")
            }
        })



        observe({
            showModal(
                modalDialog(
                    "This will push recording data to the database! Are you sure?",
                    br(),
                    layout_columns(
                        actionButton(paste("main","manage_db_upload", ns("confirm_upload_all"), sep = "-"), "Do it."),
                        actionButton(paste("main","manage_db_upload", ns("cancel_upload"), sep = "-"), "Don't do it")
                    ),
                    title = "Confirm upload ALL ?",
                    easyClose = TRUE,
                    footer = NULL
                )
            )
        }) |>
            bindEvent(input$data_upload_all)


        # Upload all data
        observe({
            busy(TRUE)
            for (i in current_choices()) {
                tryCatch(
                    {
                        push_db_activity
                    },
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
    })
}
