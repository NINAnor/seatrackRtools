#' Parse data request PDFs and compile into a summary CSV
#' This function reads all PDF files in a specified folder, extracts relevant information from each PDF, and compiles it into a summary. The summary can then be exported as a CSV file.
#' @param request_folder The folder containing the PDF data request files. Default is set to a specific path within the SEATRACK folder.
#' @param first_id An optional parameter to specify the first ID to include in the summary. If NA, all entries will be included.
#' @param export A boolean indicating whether to export the summary as a CSV file. Default is TRUE.
#' @return A data frame containing the parsed information from the PDF files.
#' @export
#' @concept data_requests
parse_data_request_folder <- function(request_folder = file.path(the$sea_track_folder, "Admin\\08_Data requests and AoU\\Data applications_received\\"), first_id = NA, export = TRUE) {
    all_pdf <- list.files(request_folder, pattern = "*.pdf", full.names = TRUE)
    ids <- sapply(strsplit(basename(all_pdf), "_"), function(x) x[1])
    all_rows <- lapply(all_pdf, parse_data_request_pdf)
    request_df <- do.call(rbind, all_rows)
    if (!is.na(first_id)) {
        request_df <- request_df[(which(request_df$ID == first_id)):nrow(request_df), ]
    }
    if (export) {
        write.csv(request_df, file.path(the$sea_track_folder, "Admin\\08_Data requests and AoU\\", glue::glue("SEATRACK_data_request_summary_{request_df$ID[1]}_{request_df$ID[length(request_df$ID)]}.csv")), row.names = FALSE)
    }
    return(request_df)
}


#' Parse a single PDF data request file and extract relevant information
#' This function reads a PDF file, extracts specific information such as the applicant's name, project title, study area, species involved, and other details, and compiles this information into a structured format.
#' @param pdf_path The file path to the PDF data request file.
#' @return A data frame containing the extracted information from the PDF file.
#' @export
#' @concept data_requests
parse_data_request_pdf <- function(pdf_path) {
    if (!file.exists(pdf_path)) {
        stop("The specified PDF file does not exist.")
    }

    # Read the PDF file
    pdf_text <- tryCatch(
        {
            pdftools::pdf_text(pdf_path)
        },
        error = function(e) {
            stop("Error reading the PDF file: ", e$message)
        }
    )

    # Combine the text from all pages into a single string
    combined_text <- paste(pdf_text, collapse = "\n")

    id <- strsplit(basename(pdf_path), "_")[[1]][1]
    # Extract text between "Applicant name/s:" and "Contact Email"
    if (grepl("Applicant name/s:", combined_text, fixed = TRUE) && grepl("Contact Email", combined_text, fixed = TRUE)) {
        authors <- gsub("(?s).*Applicant name/s:\\s*(.*?)\\s*Contact Email.*", "\\1", combined_text, perl = TRUE)
        authors <- trimws(authors)
        authors <- remove_tab(authors)
        authors <- trimws(authors)
    } else {
        authors <- NA_character_
    }

    if (grepl("Project title:", combined_text, fixed = TRUE) && grepl("Funder/s", combined_text, fixed = TRUE)) {
        title <- gsub("(?s).*Project title:\\s*(.*?)\\s*Funder/s.*", "\\1", combined_text, perl = TRUE)
        title <- trimws(title)
        title <- remove_tab(title)
        title <- trimws(title)
    } else {
        title <- NA_character_
    }

    if (grepl("Study area:", combined_text, fixed = TRUE) && grepl("Is this an early career", combined_text, fixed = TRUE)) {
        study_area <- gsub("(?s).*Study area:\\s*(.*?)\\s*Is this an early career.*", "\\1", combined_text, perl = TRUE)
        study_area <- trimws(study_area)
        study_area <- remove_tab(study_area)
        study_area <- gsub("[0-9]+", "", study_area)
        study_area <- trimws(study_area)
    } else {
        study_area <- NA_character_
    }

    if (grepl("Specie\n\n", combined_text, fixed = TRUE) && grepl("\n\n ", combined_text, fixed = TRUE)) {
        species_matches <- gregexpr("Specie\n\n(.*?)\n\n ", combined_text, perl = TRUE)
        species_sections <- regmatches(combined_text, species_matches)[[1]]
        species_sections <- gsub("Specie\n\n|\n\n", "", species_sections)
        species_sections <- trimws(species_sections)
    } else {
        species_sections <- character(0)
    }

    if (grepl("Is this an early career researcher (ECR) project?", combined_text, fixed = TRUE) && grepl("Co‐authorship/acknowledgements:", combined_text, fixed = TRUE)) {
        pat <- "(?s).*Is this an early career researcher \\(ECR\\) project\\?\\s*(.*?)\\s*Co.?authorship/acknowledgements:.*"
        ecr <- gsub(pat, "\\1", combined_text, perl = TRUE)
    } else {
        ecr <- NA_character_
    }

    species_cols <- species_string_vector(species_sections)

    if (grepl("Request Date", combined_text, fixed = TRUE) && grepl("Applicant name/", combined_text, fixed = TRUE)) {
        request_date <- gsub("(?s).*Request Date\\s*(.*?)\\s*Applicant name/.*", "\\1", combined_text, perl = TRUE)
    } else {
        request_date <- NA_character_
    }
    request_date <- as.Date(request_date, format = "%d/%m/%Y")


    if (grepl("Study outcome, including planned peer-reviewed publication or report (please include tentative title or general topic):", combined_text, fixed = TRUE) &&
        grepl("Study area", combined_text, fixed = TRUE)) {
        aims <- sub(
            "(?s).*?Study outcome, including planned peer-reviewed publication or report \\(please include tentative title or general topic\\):\\s*(.*?)\\s*Study\\s+area\\b.*",
            "\\1",
            combined_text,
            perl = TRUE
        )
        aims <- gsub("\n", ", ", aims)
    } else {
        aims <- NA_character_
    }





    final_row <- dplyr::tibble(ID = id, `Principal Investigator` = authors, `Project title` = title, dplyr::tibble(!!!setNames(as.vector(species_cols), colnames(species_cols))), `Study area` = study_area, `ECR project` = ecr, `data request` = request_date, aim = aims, row.names = NULL)

    return(final_row)
}

remove_tab <- function(x) {
    x <- gsub("\n    ", " ", x, fixed = TRUE)
    x <- gsub("\n", "", x, fixed = TRUE)
    x
}

species_string_vector <- function(species_vector) {
    shortnames <- c("NOFU", "BLKW", "GRSK", "NOGA", "LSP", "ARTE", "COGU", "BRGU", "LIAU", "ATPU", "RABI", "HEGU", "LBBG", "GLGU", "EUSH", "COEI")
    common_names <- c("Northern fulmar", "Black-legged kittiwake", "Great skua", "Northern gannet", "Leach's storm petrel", "Arctic tern", "Common guillemot", "Brünnich's guillemot", "Little auk", "Atlantic puffin", "Razorbill", "Herring gull", "Lesser black-backed gull", "Glaucous gull", "European shag", "Common eider")
    present_bool <- common_names %in% species_vector
    names(present_bool) <- shortnames
    string_vector <- sapply(present_bool, function(x) {
        ifelse(x, "x", "")
    })
    string_df <- t(as.data.frame(string_vector))
    return(string_df)
}
