# ============================================================
# Shiny app template: scatterplot + map viewer
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(scales)
# library(png)
# library(grid)
# library(here)

# ---- Placeholder datasets ----
#Saturation curve data:

# Find the path to the app directory inside the installed package
app_path <- system.file("shiny", "App_Colony_Repr", package = "seatrackRtools")

# Load data relative to that path
tot_combined_areas <- readRDS(file.path(app_path, "app_data/tot_combined_areas2.rds"))
spec_col_uncertainty <- readRDS(file.path(app_path, "app_data/spec_col_uncertainty.rds"))
# ... and so on for the others


# tot_combined_areas <- readRDS("app_data/tot_combined_areas2.rds")
# spec_col_uncertainty <- readRDS('app_data/spec_col_uncertainty.rds')

# # Spatial data:
tot_contours <- readRDS(file.path(app_path, "app_data/tot_contours_un2.rds"))
world <- ne_countries(scale = "medium", returnclass = "sf")
world_un <- st_union(world)
rm(world)
Colony_locs <- readRDS(file.path(app_path, "app_data/Colony_locs.rds"))

# Kernel fill colors:
Fills <- viridis(length(seq(from = 25, to = 95, by = 5)))
names(Fills) <- seq(from = 25, to = 95, by = 5)

# Watermark_background:

# img <- readPNG("www/logo.png")
# # adjust transparency (0 = fully transparent, 1 = fully opaque)
# alpha_level <- 0.1   # <-- change this (0–1)
#
# # apply alpha to the image
# # if the image has 4 channels (R,G,B,A), multiply the alpha channel
# if (dim(img)[3] == 4) {
#   img[,,4] <- img[,,4] * alpha_level
# } else {
#   # If the image has no alpha channel, add one
#   alpha_channel <- array(alpha_level, dim = dim(img)[1:2])
#   img <- abind::abind(img, alpha_channel, along = 3)
# }
#
# # Function to tile the logo N × M times
# tile_logo <- function(img, nx = 5, ny = 5) {
#   h <- dim(img)[1]
#   w <- dim(img)[2]
#
#   # Repeat rows and columns
#   tiled <- img[rep(1:h, ny), rep(1:w, nx), ]
#
#   tiled
# }
#
# logos_nx <- 7 #Set the number of watermark columns you want

# To change slider colors:
setSliderColor <- function(color, sliderId) {

  # some tests to control inputs
  stopifnot(!is.null(color))
  stopifnot(is.character(color))
  stopifnot(is.numeric(sliderId))
  stopifnot(!is.null(sliderId))

  # the css class for ionrangeslider starts from 0
  # therefore need to remove 1 from sliderId
  sliderId <- sliderId - 1

  # create custom css background for each slider
  # selected by the user
  sliderCol <- lapply(sliderId, FUN = function(i) {
    paste0(
      ".js-irs-", i, " .irs-single,",
      " .js-irs-", i, " .irs-from,",
      " .js-irs-", i, " .irs-to,",
      " .js-irs-", i, " .irs-bar-edge,",
      " .js-irs-", i,
      " .irs-bar{  border-color: transparent;background: ", color[i+1],
      "; border-top: 1px solid ", color[i+1],
      "; border-bottom: 1px solid ", color[i+1],
      ";}"
    )
  })

  # insert this custom css code in the head
  # of the shiy app
  custom_head <- tags$head(tags$style(HTML(as.character(sliderCol))))
  return(custom_head)
}



# Creating a function to display the SEATRACK logo while loading:
# Takes a location 'href', an image location 'src', a loading gif 'loadingsrc'
# height, width and alt text, and produces a loading logo that activates while
# Shiny is busy
loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)")
    ),
    tags$a(href=href,
           target = "_blank", # opens the link in a new browser tab
           div(class = "busy",
               img(src=loadingsrc,height = height, width = width, alt = alt)),
           div(class = 'notbusy',
               img(src = src, height = height, width = width, alt = alt))
    )
  )
}

# ============================================================
# UI
# ============================================================
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  title = "Colony Representativeness Explorer",

  dashboardHeader(title = tags$a(href='http://seatrack.net',
                                 target = "_blank", # opens the link in a new browser tab
                                 tags$img(src='logo_landscape_inverted.png',height = "70px")),
                  titleWidth = 350),

  dashboardSidebar(disable = T),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Increase overall header height */
        .main-header .navbar {
          height: 80px;
          min-height: 80px;
        }

        /* Adjust the logo area to match */
        .main-header .logo {
          height: 80px;
          line-height: 80px;
        }

        /* Optional: vertically center the title text */
        .main-header .navbar .sidebar-toggle {
          height: 80px;
          padding-top: 25px;
          padding-bottom: 25px;
        }
      "))
    ),

    fluidRow(
      #First the column with text
      column(
        width = 3,
        style = "padding-right: 5px; padding-left: 3px;",  # reduce padding
        box(
          width = 12,
          HTML("<h3>Welcome to the <b>Colony Representativeness Explorer!</b></h3>"),
          HTML("<h4>This app allows you to visualize colony representativeness captured in the SEATRACK database,
            and make comparisons among colonies and species.</h4>"),
          HTML("<h4>It also provides a mapping tool to display the kernels used in the analysis.</h4>"),
          h3("How to use this app:"),
          tags$ul(
            HTML("<li><p style='font-size: 17px;'>Select a species and colony from the dropdown menus</p></li>"),
            HTML("<li><p style='font-size: 17px;'>Find out how much information new individuals will add to your colony representativeness (green slider) or estimate how many individuals you'd have to tag and recapture to obtain a desired representativeness (blue slider)</p></li>"),
            HTML("<li><p style='font-size: 17px;'>Adjust the KDE contour slider to control the kernel density level</p></li>"),
            HTML("<li><p style='font-size: 17px;'>View both the estimated saturation curve and matching kernels</p></li>")

          )
        ),
        box(
          width = 12,
          HTML("<h3>Methods summary</b></h3>"),
          HTML("<h4>To perform this analysis, we first assessed which individuals had more than 10 location estimates in December, assuming that this month represents the winter distribution.
          Then, we selected colonies with more than 4 individuals that match this requirement for further analysis. Individual-level utilization densities were then constructed for the selected individuals (for any desired KDE%; see panel b).
          We could then use these kernels to estimate the spatial variation within the whole colony (assuming that the saturation process follows a Michaelis-Menten function; red curve in panel a).
          We could then also estimate how much of the expected within-colony variation we currently capture (black percentage in panel a).</h4>"),
          HTML("<h4>Additionally, we could use the fitted Michaelis-Menten curve to determine how much new information (in %) we'd gain from including tracking data of additional individuals (green circle in panel a). We could also estimate how many individuals need to be tracked to obtain a certain desired representativeness (blue circle in panel a)</h4>"),
        ),

        box(
          width = 12,
          HTML("<p style='font-size: 14px;'>By Lars Ursem <br> lars.ursem@npolar.no / lars.ursem@live.nl</p>")
        )
      ),
      # Second the column with the sliders etc.
      column(
        width = 2,
        style = "padding-right: 5px; padding-left: 3px;",  # reduce padding
        box(
          width = 12,
          title = "Select species-colony of interest",

          selectInput("species", "Select species:",
                      choices = unique(tot_combined_areas$species)),

          selectInput("colony", "Select colony:",
                      choices = unique(tot_combined_areas$colony)),

          selectInput("month", "Select winter month:",
                      choices = month.name[unique(tot_combined_areas$month)]
                        ),
        ),

        box(
          width = 12,
          title = "Extra options",

          setSliderColor(c("#14870E","#1D48DE","#757575"), c(1,2,3)),

          sliderInput("ADD", "How many individuals do you want to add?",
                      min = 0, max = 200, value = 0, step = 5),

          sliderInput("Desired_perc", "How much representativeness (%) would you like to achieve?",
                      min = 0, max = 90, value = 0, step = 5)

        ),
        box(
          width = 12,

          sliderInput("contour", "KDE% used in analysis (see panel b)",
                      min = 25, max = 95, value = 75, step = 5),

          br()
        ),
      ),

      column(
        width = 7,
        style = "padding-right: 5px; padding-left: 3px;",  # reduce padding

        uiOutput("summary_box"),

        box(
          width = 12,
          plotOutput("scatterplot", height = "500px", width = "900px"),
          plotOutput("mapplot", height = "500px", width = "900px"),
        )

      ),
    )
  )
)


# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {

  # ---- Filter data dynamically ----
  observeEvent(input$species, {
    colonies_available <- unique(tot_combined_areas$colony[tot_combined_areas$species == input$species])
    updateSelectInput(session, "colony", choices = colonies_available)
  })

  # Saturation curve
  filtered_scatter <- reactive({
    tot_combined_areas %>%
      filter(KDE_contour == input$contour,
             species == input$species,
             colony == input$colony)
  })

  month_numeric <- reactive({match(input$month, month.name) %>%
      as.numeric()
    })
  uncertainty <- reactive({
    spec_col_uncertainty %>%
      dplyr::filter(KDE_contour == input$contour,
             species == input$species,
             colony == input$colony,
             month == month_numeric()) %>%
      pull(uncertainty)
  })

  n_ind <- reactive({
    max(filtered_scatter()$n_inds)
  })

  h_par <- reactive({
    filtered_scatter() %>%
      distinct(h_par) %>%
      pull()
  })

  A_mean <- reactive({
    filtered_scatter() %>%
      distinct(A_mean) %>%
      pull()
  })

  B_mean <- reactive({
    filtered_scatter() %>%
      distinct(B_mean) %>%
      pull()
  })

  v_half <- reactive({
    A_mean * B_mean / (B_mean + B_mean)  # = Vmax/2
  })

  MM_mean <- function(x) {A_mean()*x/(B_mean()+x)}

  col_dat_sum <- reactive({
    filtered_scatter() %>%
      group_by(n_inds) %>%
      dplyr::summarise(mean = mean(area),
                       sd = sd(area)) %>%
      mutate(up_bound = mean+sd,
             low_bound = mean-sd)
  })

  y_95 = reactive({A_mean() *0.95})
  x_95 = reactive({y_95()*B_mean()/(A_mean()-y_95())})

  y_50 = reactive({A_mean() *0.50})
  x_50 = reactive({y_50()*B_mean()/(A_mean()-y_50())})

  higher_ind_or_x_95 <- reactive({max(n_ind(), x_95())})

  max_point <- reactive({
    col_dat_sum() %>%
      arrange(desc(n_inds)) %>%
      slice_head(n=1) %>%
      pull(mean)
  })

  max_percent <- reactive({round(100/A_mean()*max_point(),0)})

  y_new <- reactive({MM_mean((n_ind()+input$ADD))})
  new_percent <- reactive({round(100/A_mean()*(y_new()),0)-max_percent()})

  y_desired <- reactive({A_mean() *input$Desired_perc/100})
  x_desired <- reactive({y_desired()*B_mean()/(A_mean()-y_desired())})


  perc_lines_col <- "grey70"

  # ---- Scatterplot ----
  output$scatterplot <- renderPlot({
    ggplot(col_dat_sum(), aes(x = n_inds, y = mean)) +
      geom_ribbon(aes(ymin = low_bound, ymax = up_bound), fill = "grey80")+
      stat_function(fun = MM_mean, col = "tomato", linetype = "dashed", lwd = 2)+
      geom_point(size = 3, shape = 21, fill = "grey20", col = "black", alpha =0.8)+
      geom_hline(yintercept = A_mean(), linetype = "dotted")+
      geom_text(x = x_95()*0.15, y = A_mean()*1.025, label = "Estimated within-colony variation", col = "grey40", size = 5)+
      geom_segment(y = y_95(), x = 1, xend = x_95(), col = perc_lines_col, linetype = "dashed", lwd = 1)+
      geom_segment(y = -1, x = x_95(), yend = y_95(), col = perc_lines_col, linetype = "dashed", lwd = 1)+
      geom_point(x = x_95(), y = y_95(), size = 3, col = perc_lines_col)+
      geom_text(x = 0, y = y_95()-A_mean()/100*3, label = "95%", col = perc_lines_col, size = 5)+
      geom_segment(y = y_50(), x = 1, xend = x_50(), col = perc_lines_col, linetype = "dashed", lwd = 1)+
      geom_segment(y = -1, x = x_50(), yend = y_50(), col = perc_lines_col, linetype = "dashed", lwd = 1)+
      geom_point(x = x_50(), y = y_50(), size = 4, shape = 21, fill = perc_lines_col, col = "black")+
      geom_text(x = 0, y = y_50()+A_mean()/100*3, label = "50%", col = perc_lines_col, nudge_y = 1500, size = 5)+
      geom_text(x = B_mean()+0.02*x_95(), y = A_mean()*0.035, label = ceiling(B_mean()), col = perc_lines_col, size = 5)+
      geom_point(x = n_ind()+input$ADD, y = y_new(), shape = 21, fill = "transparent", col = ifelse(input$ADD == 0,"transparent","#14870E"),stroke = 2, size = 5)+
      geom_segment(y = y_new(), x = 1, xend = n_ind()+input$ADD, col = ifelse(input$ADD == 0,"transparent","#14870E"), linetype = "dashed", lwd = 1)+
      geom_text(x = 0, y = y_new()+A_mean()/100*3, label = paste0(max_percent()+new_percent(),"%"), col = ifelse(input$ADD == 0,"transparent","#14870E"), nudge_y = 1500, size = 5)+
      geom_text(x = n_ind()+input$ADD+0.065*x_95(), y = y_new(), label = paste0("+",new_percent(),"%"), col = ifelse(input$ADD == 0,"transparent","#14870E"), size = 5)+
      geom_text(x = n_ind()+0.05*x_95(), y = max_point(), label = paste0(max_percent(),"%"), inherit.aes = F, size = 5) +
      geom_point(x = x_desired(), y = y_desired(), shape = 21, fill = "transparent", col = ifelse(input$Desired_perc == 0,"transparent","#1D48DE"),stroke = 2, size = 5)+
      geom_segment(yend = y_desired(), y = -1, x = x_desired(), col = ifelse(input$Desired_perc == 0,"transparent","#1D48DE"), linetype = "dashed", lwd = 1)+
      geom_label(x = x_desired()+0.075*x_95(), y = A_mean()*0.035, label = paste0(ceiling(x_desired()), " individuals"), col = ifelse(input$Desired_perc == 0,"transparent","#1D48DE"), fill = ifelse(input$Desired_perc == 0,"transparent","white"), nudge_y = 1500, size = 5)+
      geom_text(x = x_desired()+0.065*x_95(), y = y_desired(), label = paste0(input$Desired_perc,"%"), col = ifelse(input$Desired_perc == 0,"transparent","#1D48DE"), size = 5)+
      labs(title = "a)",
           y = "Colony representativeness (%)",
           x = "Number of tracked individuals")+
      scale_y_continuous(breaks = seq(from = 0, to = A_mean(), length.out = 5),
                         labels = seq(from = 0, to = 100, by = 25)
      )+
      scale_x_continuous(
        limits = c(0, round(higher_ind_or_x_95()+5,-1))
      )+
      theme_minimal()+
      theme(plot.title = element_text(size = 20, margin = margin(b=20)),
            plot.subtitle = element_text(size = 20),
            axis.title.x = element_text(size = 22,margin = margin(t=8, b = 5)),
            axis.title.y.left = element_text(size = 22,margin = margin(r=8)),
            axis.title.y.right = element_text(size = 22,margin = margin(l=8)),
            axis.text = element_text(size = 18),
            legend.text = element_text(size=30),
            legend.title = element_blank(),
            strip.text = element_text(size = 16),
            legend.position = "none",
            plot.title.position = "plot",
            plot.background = element_rect(fill = "white", color = "white"),
            legend.key.width = unit(0.5, units="cm"))+
      coord_cartesian(clip = "off")
  })



  # Results summary text
  output$summary_box <- renderUI({
    box(
      title = HTML("<u>Summary of Results</u>"),
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      HTML(paste0(
        "<p style='font-size: 17px;'><b>Species:</b>  ", input$species, "</p>",
        "<p style='font-size: 17px;'><b>Colony:</b>  ", input$colony, "</p>",
        "<p style='font-size: 17px;'><b>Winter month:</b>  ", input$month, "</p>",
        "<p style='font-size: 17px;'><b>N individuals tracked:</b>  ", n_ind(), "</p>",
        "<p style='font-size: 20px;'><b>Colony representativeness:</b>  <u>", round(max_percent(), 1), "%</u></p>",
        "<p style='font-size: 17px;'><b>Uncertainty:</b>  <u>", uncertainty(), "</u></p>",
        "<p style='font-size: 17px;'><b>Gain in representativeness (+", input$ADD, " individuals; green slider):</b>  ", ifelse(input$ADD == 0, 0,new_percent()), "%</p>",
        "<p style='font-size: 17px;'><b>N tracked individuals needed to obtain desired representativeness (blue slider):</b>  ", ifelse(input$Desired_perc == 0, 0,ceiling(x_desired())), "</p>",
        "<p style='font-size: 17px;'><b>KDE% used (grey slider):</b>  ", input$contour, "%</p>"

      ))
    )
  })



  # Plotting the contours

  filtered_map <- reactive({
    tot_contours %>%
      as_tibble() %>%
      # filter(perc == 75,
      #        species == "Atlantic puffin",
      #        colony == "Anda") %>%
      filter(KDE_contour == input$contour,
             species == input$species,
             colony == input$colony) %>%
      st_as_sf()
  })

  colony_dat <- reactive({
    tot_contours %>%
      as_tibble() %>%
      # filter(perc == 75,
      #        species == "Atlantic puffin",
      #        colony == "Anda") %>%
      filter(KDE_contour == 95,
             species == input$species,
             colony == input$colony) %>%
      st_as_sf() %>%
      st_cast("MULTIPOINT") %>%
      st_cast("POINT") %>%
      mutate(longitude = st_coordinates(.)[,1],
             latitude  = st_coordinates(.)[,2]) %>%
      as_tibble()
  })

  # Determine projection for the colony in general:
  colony_proj.laea    <- reactive({
    paste0("+proj=laea +x_0=0 +y_0=0 +lon_0=",
           mean(colony_dat()$longitude),
           " +lat_0=",
           ifelse(mean(colony_dat()$latitude)>50, 50, mean(colony_dat()$latitude)),
           " +units=km"
    )
  })


  Colony_vertices_proj <- reactive({
    filtered_map() %>%
      st_transform(crs = colony_proj.laea()) %>%
      arrange(desc(KDE_contour))
  })

  colony_loc_proj <- reactive({
    Colony_locs %>%
      filter(colony == input$colony) %>%
      st_as_sf(coords = c("lon_colony","lat_colony"), crs = st_crs(world_un)) %>%
      st_transform(crs = colony_proj.laea())
  })


  bbox <- reactive({
    st_bbox(Colony_vertices_proj())
  })

  xmin <- reactive({
    bbox()["xmin"]- (bbox()["xmax"] - bbox()["xmin"])*0.05
  })

  xmax <-  reactive({
    bbox()["xmax"]+ (bbox()["xmax"] - bbox()["xmin"])*0.05
  })

  ymin <- reactive({
    bbox()["ymin"]- (bbox()["ymax"] - bbox()["ymin"])*0.05
  })

  ymax <- reactive({
    bbox()["ymax"]+ (bbox()["ymax"] - bbox()["ymin"])*0.05
  })

  world_proj <- reactive({
    world_un %>%
      st_transform(crs = colony_proj.laea())
  })

  # logos_ny <- reactive({
  #   ceiling((ymax()-ymin()) / ((xmax()-xmin()) / logos_nx))
  # })
  #
  # tiled_img <- reactive({
  #   tile_logo(img, nx = logos_nx, ny = logos_ny())
  # })
  #
  # g <- reactive({
  #   rasterGrob(tiled_img(), interpolate = TRUE)
  # })

  # ---- Map ----
  output$mapplot <- renderPlot({
    ggplot()+
      # annotation_custom(
      #   g(),
      #   (xmin() - ((xmax()-xmin()) / 25)),
      #   (xmax() + ((xmax()-xmin()) / 25)),
      #   ymin(),
      #   ymax()
      # ) +
      geom_sf(data = world_proj(),lwd = 0.2, fill = "grey90", col = "grey70")+
      geom_sf(data = Colony_vertices_proj(), aes(fill = KDE_contour), col = ifelse(input$contour <= 50, "white","black"),stroke = 2, alpha = 0.65)+
      # geom_sf(data = col_dat2_proj, size = 0.1, alpha = 0.2)+
      geom_sf(data = colony_loc_proj(), shape = 21, fill = "red", size = 4)+
      labs(title = "b)",
           fill = "KDE% used"
      )+
      theme(
        panel.background = element_rect(fill = "grey95",),
        plot.title.position = "plot",
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16, margin = margin(b = 20)),
        legend.key.height = unit(2, "cm")
      )+
      coord_sf(
        xlim = c(xmin(), xmax()), ylim = c(ymin(),ymax()),
      )+
      scale_fill_viridis_c(breaks = seq(from = 25, to = 95, by = 10),
                           name = "KDE% used",
                           limits = c(25, 95) # full range of the gradient
      )
  })
}

# ============================================================
# Run the app
# ============================================================
shinyApp(ui = ui, server = server)
