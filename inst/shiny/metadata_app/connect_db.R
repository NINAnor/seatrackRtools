connect_db_ui <- function(id) {
  ns <- NS(id)

  actionButton(ns("login"), "Login to database")
}

connect_db_server <- function(id, busy, test = FALSE) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    observeEvent(busy(), {
      if (busy()) {
        shinyjs::disable("login")
      } else {
        shinyjs::enable("login")
      }
    })


    observe({
      showModal(
        modalDialog(
          textInput(paste("main", ns("username"), sep = "-"), "Username:"),
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
