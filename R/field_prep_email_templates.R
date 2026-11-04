#' Generate field plan email templates
#'
#' This function generates email templates for field plan communication based on the provided field plan data. It creates personalized emails for each responsible person, detailing the planned allocations of loggers and equipment for different species and colonies. The emails also request confirmation of receipt and any necessary data or metadata from the previous year.
#' @param field_plan_path Path to the field plan Excel file
#' @param field_year Year to generate email templates for, default is next year
#' @param template_dir Directory to save the generated email templates. Default is "Admin/Fieldplanning/Fieldplanning<field_year>/templates" in the sea track folder.
#' @export
#' @concept field_planning
generate_fieldplan_templates <- function(
    field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
    field_year = c(as.numeric(format(Sys.Date(), "%Y")) + 1),
    template_dir = file.path(the$sea_track_folder, "Admin", "Fieldplanning", paste0("Fieldplanning", field_year), "templates")) {
    # load fieldwork plan

    field_plan_sheet <- load_field_plan_sheet(field_plan_path, field_year, check_colony = FALSE)

    field_plan_sheet$`gls assigned` <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", field_plan_sheet$`gls assigned`)))
    field_plan_sheet$`gps assigned` <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", field_plan_sheet$`gps assigned`)))
    field_plan_sheet$`gps-gsm assigned` <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", field_plan_sheet$`gps-gsm assigned`)))

    # reduce to only rows with planned allocations
    field_plan_sheet <- field_plan_sheet[(!is.na(field_plan_sheet$`gls assigned`) & field_plan_sheet$`gls assigned` > 0) |
        (!is.na(field_plan_sheet$`gps assigned`) & field_plan_sheet$`gps assigned` > 0) |
        (!is.na(field_plan_sheet$`gps-gsm assigned`) & field_plan_sheet$`gps-gsm assigned` > 0), ]

    all_responsible <- unique(field_plan_sheet$responsible)
    all_responsible <- all_responsible[order(all_responsible)]
    for (responsible in all_responsible) {
        responsible_rows <- field_plan_sheet[field_plan_sheet$responsible == responsible, ]
        first_names <- sapply(strsplit(responsible, "_")[[1]], function(x) {
            strsplit(x, " ")[[1]][1]
        })
        if (length(first_names) > 1) {
            first_names_string <- paste(paste(first_names[1:(length(first_names) - 1)], collapse = ", "), first_names[length(first_names)], sep = " and ")
        } else {
            first_names_string <- first_names[1]
        }

        metadata_idx <- which(colnames(responsible_rows) == paste(field_year - 1, "metadata received"))
        data_idx <- which(colnames(responsible_rows) == paste(field_year - 1, "loggerdata received"))

        need_metadata <- any(is.na(responsible_rows[, metadata_idx]))
        need_data <- any(is.na(responsible_rows[, data_idx]))

        # Greetings fellow humans
        email_string <- glue::glue("
        <p><span>Dear {first_names_string},</span></p>
        <p><span>We are planning the {field_year} field_season and I have some questions for you about loggers and equipment:</span></p>
        <span>We understand that your plans can change, or this may be too early for you to confirm all details but would appreciate if you could confirm you have received this email by <u>December.</u></span>
        <span>&nbsp;</span>
        <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
        Deployment plan:
        </span></p>
        ")

        for (species in unique(responsible_rows$species)) {
            species_rows <- responsible_rows[responsible_rows$species == species, ]
            # Species header, open list
            species_string <- glue::glue("<p><span style='font-size:11.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'><i>{species}</i></span></p>

            <ul style='margin: 0; padding-left: 20px; sans-serif'>

            ")

            all_colony <- unique(species_rows$colony)
            multi_colony <- length(all_colony) > 1
            species_string_list <- ""
            for (colony in all_colony) {
                colony_rows <- species_rows[species_rows$colony == colony, ]
                all_age <- unique(colony_rows$age)
                multi_age <- length(all_age) > 1
                for (age in all_age) {
                    age_rows <- colony_rows[colony_rows$age == age, ]
                    gls_n <- age_rows$`gls assigned`
                    gps_n <- age_rows$`gps assigned`
                    gps_gsm_n <- age_rows$`gps assigned`
                    multi_device <- sum(gls_n > 0, gps_n > 0, gps_gsm_n > 0) > 1
                    age_string <- ifelse(age == "A", " adults", " juveniles")
                    # Construct a string that will be used if multi colony or multi age
                    # These are list items
                    if (multi_age || multi_colony) {
                        species_string_list <- glue::glue("
                            {species_string_list}

                                <li>
                                    {gls_n} GLS loggers for{ifelse(multi_colony, paste0(' ', colony), '')}{ifelse(multi_age, age_string, '')}.
                                </li>
                            ")
                    }
                }
            }
            # If we have not been adding list items, this can be a sentence?
            if (!(multi_age || multi_colony)) {
                species_string <- glue::glue("{species_string}
                <li style='margin-bottom: 8px;'>
                We propose to send you {gls_n} GLS loggers for this species. Is that ok?
                </li>
                ")
            } else {
                species_string <- glue::glue("{species_string}

                    <li style='margin-bottom: 8px;'>
                        We propose to send you:
                        <ul>
                        {species_string_list}
                        </ul>
                    </li>
                    <li style='margin-bottom: 8px;'>
                        Is that ok?
                    </li>")
            }

            mounting_hole <- !is.na(age_rows$`mounting holes diameter (mm)`) && age_rows$`mounting holes diameter (mm)` != "-"
            mounting_hole_string <- ifelse(mounting_hole, glue::glue(", with predrilled mounting holes of {age_rows$`mounting holes diameter (mm)`} mm set {age_rows$`mounting holes apart (mm)`} mm apart"), "")

            species_string <- glue::glue("
                        {species_string}
                        <ul style='margin: 4px 0 0 0; padding-left: 20px;'>
                            <li style='margin-bottom: 4px;'>If yes, would you need any equipment or rings for this species?</li>
                            <li style='margin-bottom: 4px;'>Any specific wishes?
                                <ul style='margin: 4px 0 0 0; padding-left: 20px;'>
                                    <li style='margin-bottom: 4px;'>We can provide you with for example: <b style='color:{age_rows$`colour outer`}'>{age_rows$`colour outer`}</b> - {age_rows$overlap} overlap, {age_rows$`height (mm)`} mm height, {age_rows$`inner diameter(s) (mm)`} mm inner diameter{ifelse(age_rows$`cable tie slot` == 'yes', ', inner slot for cable ties', '')}{mounting_hole_string}.</li>
                                    <li>We will send you cable ties, self-amalgamating tape and glue. Let us know if not needed.</li>
                                </ul>
                            </li>
                        </ul>
                    </ul>
                    ")
            email_string <- glue::glue("
            {email_string}
            {species_string}
            <span>&nbsp;</span>
            ")
        }

        delivery_date <- max(as.Date(responsible_rows$`deadline (delivery)`))
        email_string <- glue::glue("
            {email_string}
            <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
            Delivery date:
            </span></p>
            <ul style='margin: 0; padding-left: 20px; sans-serif'>
                If possible, we would like to send the equipment for all species in one package. Is the latest due date of {delivery_date} ok?
            </ul>
            <span>&nbsp;</span>
        ")


        if (need_data || need_metadata) {
            request_strings <- c(
                ifelse(need_data, paste("your downloaded", field_year - 1, "logger data"), NA),
                metadata_string <- ifelse(need_metadata, paste("your", field_year - 1, "metadata"), NA)
            )
            request_string <- paste(request_strings[!is.na(request_strings)], collapse = " and ")



            email_string <- glue::glue("
                {email_string}
                <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
                Data and metadata:
                </span></p>
                <ul style='margin: 0; padding-left: 20px; sans-serif'>
                    Could you send us {request_string}? That would be great - thanks!
                </ul>
                <span>&nbsp;</span>
            ")
        }

        email_string <- glue::glue("
            {email_string}
            <p><span>
            Thank you and best regards
            </span></p>
            <p>Julian</p>

        ")
        # save template
        if (!dir.exists(template_dir)) {
            dir.create(template_dir)
        }
        cat(as.character(email_string), file = file.path(template_dir, paste0(responsible, "_", field_year, ".html")))
    }
}
