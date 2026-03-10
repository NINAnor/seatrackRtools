manage_db_upload_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h5("Upload to database:"),
        navset_card_pill(
            id = ns("tabs"),
            nav_panel(
                title = "Metadata",
                db_upload_metadata_ui(ns("db_metadta"))
            ),
            nav_panel(
                title = "Recordings",
                db_upload_recording_ui(ns("db_recordings"))
            ),
        )
    )
}

manage_db_upload_server <- function(id, busy, all_locations, unsaved) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        db_metadata_server <- db_upload_metadata_server("db_metadata", busy, all_locations, unsaved)
        db_recordings_server <- db_recordings_server("db_recordings", busy, all_locations, unsaved)
    })
}
