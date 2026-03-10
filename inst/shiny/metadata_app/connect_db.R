connect_db_ui <- function(id) {
  ns <- NS(id)

  actionButton(ns("login"), "Login to database")
}

connect_db_server <- function(id, busy, test = FALSE, app_settings) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    observeEvent(busy(), {
      if (busy()) {
        shinyjs::disable("login")
      } else {
        shinyjs::enable("login")
      }
    })

    # app_settings_list <- app_settings()
    # input$username <- ifelse(!is.null(app_settings_list$db_username), app_settings_list$db_username, "")

    observe({
      app_settings_list <- app_settings()
      loaded_username <- ifelse(!is.null(app_settings_list$db_username), app_settings_list$db_username, "")
      showModal(
        modalDialog(
          textInput(paste("main", ns("username"), sep = "-"), "Username:", value = loaded_username),
          passwordInput(paste("main", ns("password"), sep = "-"), "Password:"),
          br(),
          layout_columns(
            actionButton(paste("main", ns("connect"), sep = "-"), "Connect"),
            actionButton(paste("main", ns("cancel"), sep = "-"), "Cancel"),
          ),
          title = "Database Credentials",
          easyClose = FALSE,
          footer = NULL
        )
      )
    }) |>
      bindEvent(input$login)

    observe({
      removeModal()
    }) |>
      bindEvent(input$cancel)


    observe({
      tryCatch(
        {
          app_settings_list <- app_settings()
          app_settings_list$db_username <- input$username
          app_settings(app_settings_list)
          if (!test) {
            seatrackR::connectSeatrack(input$username, input$password)
          } else {
            seatrackR::connectSeatrack(input$username, input$password, host = "localhost", "seatrack-test")
          }

          log_success("Succesfully connected")
        },
        error = function(e) {
          log_error(paste("ERROR", e), namespace = "error")
        }
      )

      removeModal()
    }) |>
      bindEvent(input$connect)
  })
}
