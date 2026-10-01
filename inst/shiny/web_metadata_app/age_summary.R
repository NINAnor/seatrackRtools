age_summary_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        shinydashboard::box(
            p("Proportion of tracked individuals with known age for each species in SEATRACK."),
            width = 12
        ),
        shinydashboard::box(
            uiOutput(ns("last_updated")),
            width = 12
        ),
        shinydashboard::box(
            plotOutput(ns("Plot_ageing_summary"), height = 300),
            width = 12
        ),
        shinydashboard::box(
            p("Below, you can select a species to investigate what age information is available for each location."),
            width = 12
        ),
        shinydashboard::box(
            selectInput(inputId = ns("species_age"), label = "Choose a species:", choices = c(), selected = ""),
            selectInput(inputId = ns("colony_age"), label = "Choose a colony:", choices = c("all colonies"), selected = "all colonies"),
            width = 4
        ),
        shinydashboard::box(
            plotOutput(ns("Plot_ageing_species"), height = 600),
            width = 8
        )
    )
}

age_summary_server <- function(id, ind, pos_data_last_modification, log, species_df, colony_cols) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        output$last_updated <- renderUI(p(HTML(paste("Last updated:", as.Date(pos_data_last_modification)))))

        updateSelectInput(session, "species_age", choices = species_df$eng, selected = species_df$eng[1])

        observeEvent(input$species_age, {
            updateSelectInput(session, "colony_age",
                choices = c("all colonies", unique(ind$colony[ind$species == input$species_age])),
                selected = isolate(input$colony)
            )
        })

        output$Plot_ageing_summary <- renderPlot({
            x <- log
            x <- x[order(x$known_age, decreasing = T), ]
            x <- x[!duplicated(x$ring_number), c("species", "status_sex", "known_age")]

            x$known_age <- factor(x$known_age, levels = c("yes", "no"))

            data <- x %>%
                group_by(species, known_age) %>%
                summarize(n = n()) %>%
                mutate(freq = n / sum(n))
            x1 <- data[data$known_age == "yes", ]
            data$species <- factor(data$species, levels = x1$species[order(x1$freq, decreasing = FALSE)])
            data <- data[!is.na(data$species), ]

            col_chosen <- RColorBrewer::brewer.pal(3, "Set2")[c(3, 2)]

            ggplot(data, aes(
                x = species,
                by = freq,
                fill = known_age
            )) +
                geom_bar(aes(y = freq), stat = "identity", width = 0.7) +
                scale_x_discrete(drop = FALSE) +
                scale_fill_manual(values = col_chosen) +
                labs(
                    x = " ",
                    y = "proportion of individuals with known age",
                    fill = "known age available?"
                ) +
                theme_minimal() +
                coord_flip() +
                theme(
                    legend.position = "bottom",
                    legend.title = element_blank(), text = element_text(size = 15)
                )
        })

        output$Plot_ageing_species <- renderPlot({
            x <- ind[!duplicated(ind$ring_number), c(
                "species", "colony", "status_age", "known_age",
                "ring_number", "latest_info_date"
            )]
            x <- x[x$species == input$species_age, ]
            if (input$colony_age != "all colonies") x <- x[x$colony == input$colony_age, ]

            x$known_age <- factor(x$known_age, levels = c("yes", "no"))
            x$colony <- factor(x$colony, levels = unique(x$colony))

            var_chosen <- "colony"
            x$var <- data.frame(x)[, var_chosen]
            col_chosen <- colony_cols

            ggplot(x, aes(
                x = known_age,
                by = var,
                fill = var
            )) +
                geom_bar(stat = "count", width = 0.7) +
                scale_x_discrete(drop = FALSE) +
                scale_fill_manual(values = col_chosen, drop = FALSE) +
                labs(
                    x = "known age available?",
                    y = "# individuals",
                    fill = "colony"
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
    })
}
