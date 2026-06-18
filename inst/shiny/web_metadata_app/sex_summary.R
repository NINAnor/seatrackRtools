sex_summary_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        shinydashboard::box(
            p("Proportion of tracked individuals with sexing information for each species in SEATRACK."),
            width = 12
        ),
        shinydashboard::box(
            uiOutput(ns("last_updated")),
            width = 12
        ),
        shinydashboard::box(
            plotOutput(ns("Plot_sexing_summary"), height = 300),
            width = 12
        ),
        shinydashboard::box(
            p("Below, you can select a species to investigate what sexing data is available for each species and location."),
            # p("This metadata can be downloaded."),
            width = 12
        ),
        shinydashboard::box(
            selectInput(inputId = ns("species_sex"), label = "Choose a species:", choices = c(), selected = ""),
            selectInput(inputId = ns("colony_sex"), label = "Choose a colony:", choices = c("all colonies"), selected = "all colonies"),
            selectInput(inputId = ns("choice"), label = "Plot by:", choices = c("sexing method", "colony"), selected = "sexing method"),
            width = 4
        ),
        shinydashboard::box(plotOutput(ns("Plot_sexing_species"), height = 600),
            width = 8
        )
    )
}

sex_summary_server <- function(id, ind, pos_data_last_modification, log, species_df, colony_cols) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        output$last_updated <- renderUI(p(HTML(paste("Last updated:", as.Date(pos_data_last_modification)))))

        updateSelectInput(session, "species_sex", choices = species_df$eng, selected = species_df$eng[1])

        observeEvent(input$species_sex, {
            updateSelectInput(session, "colony_sex",
                choices = c("all colonies", unique(ind$colony[ind$species == input$species_sex])),
                selected = isolate(input$colony)
            )
        })

        output$Plot_sexing_species <- renderPlot({
            x <- ind[!duplicated(ind$ring_number), c(
                "species", "colony", "status_sex", "status_sexing_method",
                "ring_number", "latest_info_date"
            )]
            x <- x[x$species == input$species_sex, ]
            if (input$colony_sex != "all colonies") x <- x[x$colony == input$colony_sex, ]

            x$sexing_data <- "yes"
            x$sexing_data[x$status_sex == "unknown"] <- "no"
            x$sexing_data <- factor(x$sexing_data, levels = c(
                "yes",
                "no"
            ))

            x$status_sexing_method[x$status_sexing_method == "none_yet"] <- NA
            x$status_sexing_method <- factor(x$status_sexing_method,
                levels = c(
                    "dna",
                    "behaviour",
                    "morphology",
                    NA
                )
            )
            # x$'sexing method' <- x$status_sexing_method
            x$colony <- factor(x$colony, levels = unique(x$colony))

            if (input$choice == "sexing method") var_chosen <- "status_sexing_method"
            if (input$choice == "colony") var_chosen <- "colony"
            x$var <- data.frame(x)[, var_chosen]


            if (input$choice == "sexing method") col_chosen <- RColorBrewer::brewer.pal(4, "Set2")
            if (input$choice == "colony") col_chosen <- colony_cols

            ggplot(x, aes(
                x = sexing_data,
                by = var,
                fill = var
            )) +
                geom_bar(stat = "count", width = 0.7) +
                scale_x_discrete(drop = FALSE) +
                scale_fill_manual(values = col_chosen, drop = FALSE) +
                labs(
                    x = "sexing data available?",
                    y = "# individuals",
                    fill = input$choice
                ) +
                theme_grey() +
                theme(
                    legend.position = "bottom",
                    text = element_text(size = 15),
                    axis.line = element_blank(),
                    # axis.text.x=element_text(size=11,margin=margin(b=10),colour="black",angle = 90, vjust = 0.5),
                    # axis.text.y=element_text(size=13,margin=margin(l=10),colour="black"),
                    axis.ticks = element_blank(),
                    panel.grid.major = element_line(colour = grey(0.95)),
                    panel.background = element_blank(),
                    legend.title = element_text(size = 12)
                )
        })

        output$Plot_sexing_summary <- renderPlot({
            x <- ind[!duplicated(ind$ring_number), c("species", "status_sex")]

            x$sexing_data <- "yes"
            x$sexing_data[x$status_sex == "unknown"] <- "no"
            x$sexing_data <- factor(x$sexing_data, levels = c("yes", "no"))

            data <- x %>%
                group_by(species, sexing_data) %>%
                summarize(n = n()) %>%
                mutate(freq = n / sum(n))
            x1 <- data[data$sexing_data == "yes", ]
            data$species <- factor(data$species, levels = x1$species[order(x1$freq, decreasing = F)])
            data <- data[!is.na(data$species), ]

            col_chosen <- RColorBrewer::brewer.pal(3, "Set2")[c(3, 2)]

            ggplot(data, aes(
                x = species,
                by = freq,
                fill = sexing_data
            )) +
                geom_bar(aes(y = freq), stat = "identity", width = 0.7) +
                scale_x_discrete(drop = FALSE) +
                scale_fill_manual(values = col_chosen) +
                labs(
                    x = " ",
                    y = "proportion of individuals with known sex",
                    fill = "sexing data available?"
                ) +
                theme_minimal() +
                coord_flip() +
                theme(
                    legend.position = "bottom",
                    legend.title = element_blank(), text = element_text(size = 15)
                )
        })
    })
}
