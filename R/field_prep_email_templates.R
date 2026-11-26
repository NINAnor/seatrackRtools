#' Generate field plan email templates
#'
#' This function generates email templates for field plan communication based on the provided field plan data. It creates personalized emails for each responsible person, detailing the planned allocations of loggers and equipment for different species and colonies. The emails also request confirmation of receipt and any necessary data or metadata from the previous year.
#' @param field_plan_path Path to the field plan Excel file
#' @param field_year Year to generate email templates for, default is next year
#' @param template_dir Directory to save the generated email templates. Default is "Admin/Fieldplanning/Fieldplanning<field_year>/templates" in the sea track folder.
#' @param save_format Format to save the email templates, either "msg" for Outlook messages or "html" for HTML files. Default is "msg".
#' @param technician_names Names of the technicians to whom the emails will be sent. Default is "Julian Evans".
#' @param gps_gsm_responsible Name of the person responsible for GPS-GSM devices. Default is "Nanette Verboven".
#' @param arte_responsible Name of the person responsible for ARTE rings. Default is "Børge Moe".
#' @export
#' @concept field_planning
generate_fieldplan_templates <- function(
    field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
    field_year = c(as.numeric(format(Sys.Date(), "%Y")) + 1),
    template_dir = file.path(the$sea_track_folder, "Admin", "Fieldplanning", paste0("Fieldplanning", field_year), "templates"),
    save_format = c("msg", "html"),
    technician_names = c("Julian Evans"),
    bcc = c("Caitlin Frankish"),
    gps_gsm_responsible = "Nanette Verboven",
    arte_responsible = "Børge Moe",
    lsp_responsible = "Børge Moe") {
    save_format <- match.arg(save_format)
    options(knitr.kable.NA = "")

    people_table <- dplyr::tbl(con, dbplyr::in_schema("metadata", "people"))
    technician_emails <- dplyr::filter(people_table, full_name %in% technician_names) %>% dplyr::pull("email")
    bcc_emails <- dplyr::filter(people_table, full_name %in% bcc) %>% dplyr::pull("email")
    arte_email <- dplyr::filter(people_table, full_name == arte_responsible) %>% dplyr::pull("email")
    lsp_email <- dplyr::filter(people_table, full_name == lsp_responsible) %>% dplyr::pull("email")
    gps_gsm_email <- dplyr::filter(people_table, full_name == gps_gsm_responsible) %>% dplyr::pull("email")

    all_species <- seatrackR::getSpecies()

    if (save_format == "msg") {
        require(RDCOMClient)
        outApp <- RDCOMClient::COMCreate("Outlook.Application")
    }

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
    all_names <- unlist(strsplit(all_responsible, "_"))
    all_names_duplicated <- all_names[duplicated(all_names)]

    for (responsible in all_responsible) {
        responsible_rows <- field_plan_sheet[field_plan_sheet$responsible == responsible, ]
        responsible_rows <- responsible_rows[order(responsible_rows$species, responsible_rows$age, responsible_rows$colony),]
        current_names <- strsplit(responsible, "_")[[1]]
        if(any(current_names %in% all_names_duplicated)){
            species_abbreviations <- all_species$species_name_eng_abrv[match(responsible_rows$species, all_species$species_name_eng)]
            species_abbreviations_age <- paste(species_abbreviations, responsible_rows$age)
            species_age_string <- paste(sort(unique(species_abbreviations_age)), collapse = " ")
            all_colony_string <- paste(sort(unique(responsible_rows$colony)), collapse = " ")
            subject_string <- paste("-", all_colony_string, species_age_string)
        }else{
            subject_string <- ""
        }
        first_names <- sapply(current_names, function(x) {
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

        metadata_comment <- unique(responsible_rows$`metadata comment`)
        metadata_comment <- metadata_comment[!is.na(metadata_comment)]
        has_metadata_comment <- length(metadata_comment) > 0
        if (!has_metadata_comment) {
            metadata_comment <- ""
        }

        arte_deployments <- "Arctic tern" %in% responsible_rows$species
        lsp_deployments <- "Leach's storm petrel" %in% responsible_rows$species

        address <- unique(responsible_rows$address)
        address <- address[!is.na(address)]

        to_email <- unique(responsible_rows$email)
        to_email <- unique(unlist(strsplit(to_email, ";")))
        if (length(address) > 0) {
            address_strings <- sapply(
                address,
                function(current_address) {
                    if (length(address) > 1) {
                        address_rows <- responsible_rows[responsible_rows$address == current_address, ]
                        combined_identifier <- paste0("<b>", address_rows$colony, " ", address_rows$species, " ", ifelse(address_rows$age == "A", "adults", "juveniles"), "</b>")
                        pre_address_string <- paste0(paste(combined_identifier, collapse = ", "), ": ")
                    } else {
                        pre_address_string <- ""
                    }

                    glue::glue("<li>{pre_address_string}<i>{current_address}</i></li>")
                }
            )

            address_query_string <- glue::glue(
                "<li>Can you confirm these shipping details?:
                    <ul style='margin: 4px 0 0 0; padding-left: 20px;'>
                        {paste(address_strings, collapse = '\n')}
                    </ul>
                </li>"
            )
        } else {
            address_query_string <- "<li>Can you confirm your shipping details?</li>"
        }

        # Greetings fellow humans
        email_string <- glue::glue("
        <p><span>Dear {first_names_string},</span></p>
        <p><span>We are planning the {field_year} field season and I have some questions for you about loggers and equipment:</span></p>
        <span>We understand that your plans can change, or this may be too early for you to confirm all details but would appreciate if you could at least confirm you have received this email by <u>December 15th.</u></span>
        <span>&nbsp;</span>
        <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
        Deployment plan:
        </span></p>
        <p> We propose the following: </p>
        ")

        # seperate by device type, then by age category?

        gls_n <- responsible_rows$`gls assigned`
        gps_n <- responsible_rows$`gps assigned`
        gps_gsm_n <- responsible_rows$`gps-gsm assigned`
        multi_device <- sum(gls_n > 0, gps_n > 0, gps_gsm_n > 0, na.rm = TRUE) > 1

        # Only include device headers if multi device

        generate_email_table <- function(device_type, email_string, responsible_rows) {
            colname <- paste(device_type, "assigned")
            device_rows <- responsible_rows[!is.na(responsible_rows[[colname]]) & responsible_rows[[colname]] > 0, ]
            if (nrow(device_rows) == 0) {
                return(email_string)
            }

            device_table <- dplyr::mutate(device_rows,
                Species = species,
                Colony = colony,
                Age = ifelse(age == "A", "Adult", "Juvenile"),
                loggers = !!rlang::sym(colname),
                `Rings needed?` = ifelse(`number of rings` > 0, "Yes", "No"),
                Colour = `colour outer`,
                Overlap = overlap,
                `Height (mm)` = round(as.numeric(`height (mm)`), 2),
                `Inner diameter(s) (mm)` = ifelse(grepl("x", `inner diameter(s) (mm)`), `inner diameter(s) (mm)`, round(as.numeric(`inner diameter(s) (mm)`), 2)),
                `Cable tie slot` = ifelse(!is.na(`cable tie slot`) & tolower(`cable tie slot`) == "yes", "Yes", "No"),
                `Mounting holes diameter (mm)` = round(as.numeric(`mounting holes diameter (mm)`), 2),
                `Mounting holes dist. apart (mm)` = round(as.numeric(`mounting holes apart (mm)`), ),
                .keep = "none",
            )


            if (device_type == "gps-gsm") {
                logger_type_string <- "GPS transmitters"
            } else {
                logger_type_string <- paste(toupper(device_type), "loggers")
            }

            device_table <- dplyr::rename_with(device_table, function(x) {
                return(logger_type_string)
            }, starts_with("loggers"))

            if (device_type %in% c("gls", "gps")) {
                device_table$`Cable ties?` <- "Yes"
                device_table$`Glue` <- "No"
                device_table$`Amalgamating tape?` <- "No"
            }

            if (device_type == "gps-gsm") {
                device_table$Age <- NULL
                device_table$`Cable tie slot` <- NULL
                device_table$`Mounting holes diameter (mm)` <- NULL
                device_table$`Mounting holes dist. apart (mm)` <- NULL
                device_table$Harness <- "Yes"
            }

            if (device_type == "gps") {
                device_table$Age <- NULL
                device_table$`Height (mm)` <- 18
            }

            table_string <- knitr::kable(device_table,
                format = "html", escape = FALSE,
                table.attr = 'cellpadding="0" cellspacing="0" border="1" style="width: 100%; font-size: 11px;"'
            )

            # Inject some more inline styles
            table_string <- gsub(
                "<th([^>]*)>",
                '<th\\1 style="padding: 10px 2px; background-color: #f4f4f4; text-align: left;">',
                table_string
            )

            table_string <- gsub(
                "<td([^>]*)>",
                '<td\\1 style="padding: 10px 2px;">',
                table_string
            )

            email_string <- glue::glue("
            {email_string}
            <p><span style='font-size:12.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'><i>{logger_type_string}</i></span></p>
            {table_string}
            <span>&nbsp;</span>")
            return(email_string)
        }

        has_gls <- sum(gls_n, na.rm = TRUE) > 0
        has_gps_gsm <- sum(gps_gsm_n, na.rm = TRUE) > 0
        has_gps <- sum(gps_n, na.rm = TRUE) > 0

        if (has_gls) {
            email_string <- generate_email_table("gls", email_string, responsible_rows)
        }

        if (has_gps) {
            email_string <- generate_email_table("gps", email_string, responsible_rows)
        }

        if (has_gps_gsm) {
            email_string <- generate_email_table("gps-gsm", email_string, responsible_rows)
        }

        lsp_ring_string <- if_else(lsp_deployments, glue::glue("<li>Leach's storm petrel rings will be 3d printed, and the loggers pre-mounted. Please direct queries about LSP rings to {arte_responsible} {arte_email}.</li>"), "")
        arte_ring_string <- if_else(arte_deployments, glue::glue("<li>Arctic tern loggers will be pre-mounted on rings. Let us know if you do not want this.</li>"), "")

        gps_gsm_device_string <- if_else(has_gps_gsm, glue::glue("
            <li>Please direct queries about GPS transmitter devices to {gps_gsm_responsible} {gps_gsm_email}.</li> "), "")
        gps_device_string <- if_else(has_gps, "<li>We propose to deploy leg mounted GPS in a limited capacity. While all signs point to an improvement in data quality, please be fully aware of the potential risks of when accepting these devices.</li>", "")

        comments <- unique(responsible_rows$comment)
        comments <- comments[!is.na(comments)]
        if (length(comments) > 0) {
            comments_html <- sapply(comments, function(comment) {
                glue::glue("<li>{comment}</li>")
            })
            comments_string <- paste(comments_html, collapse = "\n")
        } else {
            comments_string <- ""
        }

        email_string <- glue::glue("
            {email_string}
            <span>&nbsp;</span>
            <p><span style='font-size:14.0pt;font-family:`Calibri`,color:#0F4761'>
            Questions and further information:
            </span></p>
            <ul style='margin: 0; padding-left: 20px; sans-serif'>
                {comments_string}
                <li>Do these device numbers seem realistic to you, or would you like to change them (and if so, please tell us why)?
                <ul style='margin: 4px 0 0 0; padding-left: 20px;'>
                {gps_gsm_device_string}
                {gps_device_string}
                </ul>
                </li>
                <li>Do you need rings?
                    <ul style='margin: 4px 0 0 0; padding-left: 20px;'>
                        <li>If yes, are the specified ring dimensions listed above suitable or do they need adjusting?</li>
                        <li>If you would like to have special rings (e.g. coded), we would like you to organize these yourself. We can reimburse you for the costs of the rings (up to the price of plain rings).</li>
                        {arte_ring_string}
                        {lsp_ring_string}
                    </ul>
                </li>
                <li>We will send you cable ties for all ring deployments </li>
                <li><b>Several partners have informed us that they have more than enough glue and tape. As such, we will not send you self-amalgamating tape and glue unless you tell us that you need them.</b></li>
                <li>Do you need interface cables for connecting to loggers? </li>
            </ul>
            <p><span> If necessary please include a modified table with your adjustments/answers highlighted in your reply. </span></p>
        ")

        all_dates <- responsible_rows$`deadline (delivery)`[responsible_rows$species != "Leach's Storm Petrel"]
        if (length(all_dates) > 0) {
            delivery_date <- min(as.Date(all_dates), na.rm = TRUE)
            delivery_string <- glue::glue("<li>If possible, we would like to send the equipment for all species {ifelse(lsp_deployments|arte_deployments, '(with the exceptions below)', '')} in one package. Is the latest due date of <b>{delivery_date}</b> {ifelse(lsp_deployments|arte_deployments, 'for all other species', '')} OK?</li>")
        } else {
            delivery_string <- ""
        }

        if (lsp_deployments) {
            lsp_delivery_string <- glue::glue("<li>Leach's storm petrel loggers have very small batteries. It is best to deploy them as soon as possible after we have started the loggers. <b>What is the latest date that would be possible for delivery of these loggers?</b></li>")
        } else {
            lsp_delivery_string <- ""
        }

        if (arte_deployments) {
            arte_delivery_string <- glue::glue("<li>Leach's storm petrel loggers have very small batteries. It is best to deploy them as soon as possible after we have started the loggers. <b>What is the latest date that would be possible for delivery of these loggers?</b></li>")
        } else {
            arte_delivery_string <- ""
        }

        email_string <- glue::glue("
            {email_string}
            <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
            Delivery date:
            </span></p>
            <ul style='margin: 0; padding-left: 20px; sans-serif'>
                {
                    delivery_string
                }
                {
                    lsp_delivery_string
                }
                {
                    arte_delivery_string
                }
                {
                    address_query_string
                }
            </ul>
        ")


        if (need_data || need_metadata || has_metadata_comment) {
            request_strings <- c(
                ifelse(need_data, paste("your downloaded", field_year - 1, "logger data"), NA),
                ifelse(need_metadata, paste("your", field_year - 1, "metadata"), NA)
            )
            if (any(!is.na(request_strings))) {
                request_string <- paste(request_strings[!is.na(request_strings)], collapse = " and ")
                full_request_string <- glue::glue("<li> Could you send us {request_string}? That would be great - thanks! </li>")
            } else {
                full_request_string <- ""
            }
            if (has_metadata_comment) {
                metadata_comment <- glue::glue("<li>{metadata_comment}</li>")
            }

            email_string <- glue::glue("
                {email_string}
                <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
                Data and metadata:
                </span></p>
                <ul style='margin: 0; padding-left: 20px; sans-serif'>
                    {full_request_string} {metadata_comment}
                </ul>
            ")
        }

        contact <- responsible_rows[["who's contacting"]][1]
        contact_first_name <- strsplit(contact, " ", fixed = TRUE)[[1]][1]

        email_string <- glue::glue("
            {email_string}
            <p><span>
            Thank you and best regards
            </span></p>
            <p>{contact_first_name}</p>

        ")
        arte_string <- ifelse(arte_deployments, arte_responsible, "")
        lsp_string <- ifelse(arte_deployments, lsp_responsible, "")
        gps_gsm_string <- ifelse(has_gps_gsm, gps_gsm_responsible, "")
        cc_string <- ifelse(arte_deployments | has_gps_gsm, glue::glue(" cc {paste(unique(c(arte_string, lsp_string, gps_gsm_string)), collapse = ' ')}"), "")



        full_template_dir <- file.path(template_dir, save_format, contact)

        # save template
        if (!dir.exists(full_template_dir)) {
            dir.create(full_template_dir, recursive = TRUE)
        }
        template_name <- paste0(responsible, cc_string, "_", field_year)
        if (save_format == "html") {
            cat(as.character(email_string), file = file.path(full_template_dir, paste0(template_name, ".html")))
        } else if (save_format == "msg") {
            # Initialize Outlook and create a mail item (0 = olMailItem)

            all_emails <- technician_emails
            if (arte_deployments) {
                all_emails <- c(all_emails, arte_email)
            }
            if (lsp_deployments) {
                all_emails <- c(all_emails, lsp_email)
            }
            if (has_gps_gsm) {
                all_emails <- c(all_emails, gps_gsm_email)
            }
            all_emails <- unique(all_emails[!is.na(all_emails)])
            all_emails <- all_emails[!all_emails %in% to_email]
            outMail <- outApp$CreateItem(0)

            # Fill details
            outMail[["To"]] <- paste(to_email, collapse = ";")
            outMail[["Subject"]] <- paste0("SEATRACK field planning ", field_year, subject_string)
            outMail[["CC"]] <- paste(all_emails, collapse = ";")
            outMail[["BCC"]] <- paste(bcc_emails, collapse = ";")
            outMail[["HTMLBody"]] <- as.character(email_string)

            msg_path <- normalizePath(file.path(full_template_dir, paste0(template_name, ".msg")), winslash = "\\", mustWork = FALSE)
            outMail$SaveAs(msg_path, 3)

            # 5. Clean up the COM object without sending
            outMail$Close(1) # 1 = olDiscard (closes without prompting to save to Drafts)
        }
    }
}

# #' Generate field plan email templates (old style)
# #'
# #' This function generates email templates for field plan communication based on the provided field plan data. It creates personalized emails for each responsible person, detailing the planned allocations of loggers and equipment for different species and colonies. The emails also request confirmation of receipt and any necessary data or metadata from the previous year.
# #' @param field_plan_path Path to the field plan Excel file
# #' @param field_year Year to generate email templates for, default is next year
# #' @param template_dir Directory to save the generated email templates. Default is "Admin/Fieldplanning/Fieldplanning<field_year>/templates" in the sea track folder.
# generate_fieldplan_templates_old <- function(
#     field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
#     field_year = c(as.numeric(format(Sys.Date(), "%Y")) + 1),
#     template_dir = file.path(the$sea_track_folder, "Admin", "Fieldplanning", paste0("Fieldplanning", field_year), "templates")) {
#     # load fieldwork plan

#     field_plan_sheet <- load_field_plan_sheet(field_plan_path, field_year, check_colony = FALSE)

#     field_plan_sheet$`gls assigned` <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", field_plan_sheet$`gls assigned`)))
#     field_plan_sheet$`gps assigned` <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", field_plan_sheet$`gps assigned`)))
#     field_plan_sheet$`gps-gsm assigned` <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", field_plan_sheet$`gps-gsm assigned`)))

#     # reduce to only rows with planned allocations
#     field_plan_sheet <- field_plan_sheet[(!is.na(field_plan_sheet$`gls assigned`) & field_plan_sheet$`gls assigned` > 0) |
#         (!is.na(field_plan_sheet$`gps assigned`) & field_plan_sheet$`gps assigned` > 0) |
#         (!is.na(field_plan_sheet$`gps-gsm assigned`) & field_plan_sheet$`gps-gsm assigned` > 0), ]

#     all_responsible <- unique(field_plan_sheet$responsible)
#     all_responsible <- all_responsible[order(all_responsible)]
#     for (responsible in all_responsible) {
#         responsible_rows <- field_plan_sheet[field_plan_sheet$responsible == responsible, ]
#         first_names <- sapply(strsplit(responsible, "_")[[1]], function(x) {
#             strsplit(x, " ")[[1]][1]
#         })
#         if (length(first_names) > 1) {
#             first_names_string <- paste(paste(first_names[1:(length(first_names) - 1)], collapse = ", "), first_names[length(first_names)], sep = " and ")
#         } else {
#             first_names_string <- first_names[1]
#         }

#         metadata_idx <- which(colnames(responsible_rows) == paste(field_year - 1, "metadata received"))
#         data_idx <- which(colnames(responsible_rows) == paste(field_year - 1, "loggerdata received"))

#         need_metadata <- any(is.na(responsible_rows[, metadata_idx]))
#         need_data <- any(is.na(responsible_rows[, data_idx]))

#         # Greetings fellow humans
#         email_string <- glue::glue("
#         <p><span>Dear {first_names_string},</span></p>
#         <p><span>We are planning the {field_year} field_season and I have some questions for you about loggers and equipment:</span></p>
#         <span>We understand that your plans can change, or this may be too early for you to confirm all details but would appreciate if you could confirm you have received this email by <u>December.</u></span>
#         <span>&nbsp;</span>
#         <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
#         Deployment plan:
#         </span></p>
#         ")

#         for (species in unique(responsible_rows$species)) {
#             species_rows <- responsible_rows[responsible_rows$species == species, ]
#             # Species header, open list
#             species_string <- glue::glue("<p><span style='font-size:11.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'><i>{species}</i></span></p>

#             <ul style='margin: 0; padding-left: 20px; sans-serif'>

#             ")

#             all_colony <- unique(species_rows$colony)
#             multi_colony <- length(all_colony) > 1
#             species_string_list <- ""
#             for (colony in all_colony) {
#                 colony_rows <- species_rows[species_rows$colony == colony, ]
#                 all_age <- unique(colony_rows$age)
#                 multi_age <- length(all_age) > 1
#                 for (age in all_age) {
#                     age_rows <- colony_rows[colony_rows$age == age, ]
#                     gls_n <- age_rows$`gls assigned`
#                     gps_n <- age_rows$`gps assigned`
#                     gps_gsm_n <- age_rows$`gps assigned`
#                     multi_device <- sum(gls_n > 0, gps_n > 0, gps_gsm_n > 0) > 1
#                     age_string <- ifelse(age == "A", " adults", " juveniles")
#                     # Construct a string that will be used if multi colony or multi age
#                     # These are list items
#                     if (multi_age || multi_colony) {
#                         species_string_list <- glue::glue("
#                             {species_string_list}

#                                 <li>
#                                     {gls_n} GLS loggers for{ifelse(multi_colony, paste0(' ', colony), '')}{ifelse(multi_age, age_string, '')}.
#                                 </li>
#                             ")
#                     }
#                 }
#             }
#             # If we have not been adding list items, this can be a sentence?
#             if (!(multi_age || multi_colony)) {
#                 species_string <- glue::glue("{species_string}
#                 <li style='margin-bottom: 8px;'>
#                 We propose to send you {gls_n} GLS loggers for this species. Is that ok?
#                 </li>
#                 ")
#             } else {
#                 species_string <- glue::glue("{species_string}

#                     <li style='margin-bottom: 8px;'>
#                         We propose to send you:
#                         <ul>
#                         {species_string_list}
#                         </ul>
#                     </li>
#                     <li style='margin-bottom: 8px;'>
#                         Is that ok?
#                     </li>")
#             }

#             mounting_hole <- !is.na(age_rows$`mounting holes diameter (mm)`) && age_rows$`mounting holes diameter (mm)` != "-"
#             mounting_hole_string <- ifelse(mounting_hole, glue::glue(", with predrilled mounting holes of {age_rows$`mounting holes diameter (mm)`} mm set {age_rows$`mounting holes apart (mm)`} mm apart"), "")

#             species_string <- glue::glue("
#                         {species_string}
#                         <ul style='margin: 4px 0 0 0; padding-left: 20px;'>
#                             <li style='margin-bottom: 4px;'>If yes, would you need any equipment or rings for this species?</li>
#                             <li style='margin-bottom: 4px;'>Any specific wishes?
#                                 <ul style='margin: 4px 0 0 0; padding-left: 20px;'>
#                                     <li style='margin-bottom: 4px;'>We can provide you with for example: <b style='color:{age_rows$`colour outer`}'>{age_rows$`colour outer`}</b> - {age_rows$overlap} overlap, {age_rows$`height (mm)`} mm height, {age_rows$`inner diameter(s) (mm)`} mm inner diameter{ifelse(!is.na(age_rows$`cable tie slot`) & age_rows$`cable tie slot` == 'yes', ', inner slot for cable ties', '')}{mounting_hole_string}.</li>
#                                     <li>We will send you cable ties, self-amalgamating tape and glue. Let us know if not needed.</li>
#                                 </ul>
#                             </li>
#                         </ul>
#                     </ul>
#                     ")
#             email_string <- glue::glue("
#             {email_string}
#             {species_string}
#             <span>&nbsp;</span>
#             ")
#         }

#         delivery_date <- max(as.Date(responsible_rows$`deadline (delivery)`))
#         email_string <- glue::glue("
#             {email_string}
#             <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
#             Delivery date:
#             </span></p>
#             <ul style='margin: 0; padding-left: 20px; sans-serif'>
#                 If possible, we would like to send the equipment for all species in one package. Is the latest due date of {delivery_date} ok?
#             </ul>
#             <span>&nbsp;</span>
#         ")


#         if (need_data || need_metadata) {
#             request_strings <- c(
#                 ifelse(need_data, paste("your downloaded", field_year - 1, "logger data"), NA),
#                 metadata_string <- ifelse(need_metadata, paste("your", field_year - 1, "metadata"), NA)
#             )
#             request_string <- paste(request_strings[!is.na(request_strings)], collapse = " and ")



#             email_string <- glue::glue("
#                 {email_string}
#                 <p><span style='font-size:14.0pt;font-family:`Calibri`,sans-serif;color:#0F4761'>
#                 Data and metadata:
#                 </span></p>
#                 <ul style='margin: 0; padding-left: 20px; sans-serif'>
#                     Could you send us {request_string}? That would be great - thanks!
#                 </ul>
#                 <span>&nbsp;</span>
#             ")
#         }

#         email_string <- glue::glue("
#             {email_string}
#             <p><span>
#             Thank you and best regards
#             </span></p>
#             <p>Julian</p>

#         ")
#         # save template
#         if (!dir.exists(template_dir)) {
#             dir.create(template_dir)
#         }
#         cat(as.character(email_string), file = file.path(template_dir, paste0(responsible, "_", field_year, ".html")))
#     }
# }
