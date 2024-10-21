# This Shiny Dashboard is designed to track the progress of Davis Lab x Dior Herbariomix project
# It is connected to a private GoogleSheets doc, where the data is stored

# Load Libraries
library(dplyr)
library(DT)
library(ggplot2)
library(googlesheets4)
library(lubridate)
library(plotly)
library(scales)
library(stringr)
library(shiny)
library(shinydashboard)
library(tidyr)
library(ggpubr)

## prepare the OAuth token and set up the target sheet:
##  - do this EXACTLY ONCE
#shiny_token <- gs4_auth(email = "abakirbas@gmail.com",
#                        scopes= "spreadsheets") # authenticate w/ your desired Google identity here
#saveRDS(shiny_token, "gs_app_token.rds")

# herbariomix_app$sheet_key # 1f5RikOUjaperzxZodhcom7Wuqrddopy0ZwbBPhWFAAY

## Load & Clean Data
#gs4_auth(token = "gs_app_token.rds")
gs4_auth(email = "abakirbas@gmail.com",
         scopes= "spreadsheets", cache = ".secrets") # authenticate w/ your desired Google identity here

# designate project-specific cache
options(gargle_oauth_cache = ".secrets", gargle_oauth_email = TRUE)

# check the value of the option, if you like
gargle::gargle_oauth_cache()

# see your token file in the cache, if you like
list.files(".secrets/")

herbariomix_app <- read_sheet(
  "1f5RikOUjaperzxZodhcom7Wuqrddopy0ZwbBPhWFAAY", sheet = "Sheet2")

digitization_records <- read_sheet(
  "1f5RikOUjaperzxZodhcom7Wuqrddopy0ZwbBPhWFAAY", sheet = "Sheet5")

digitization_genera_progress <- read_sheet(
  "1f5RikOUjaperzxZodhcom7Wuqrddopy0ZwbBPhWFAAY", sheet = "Sheet7") %>% filter(!is.na(Genus))

# Create a function to load data already submitted
#loadData <- function(sheet_id) {
# Read the data from google sheets
#    read_sheet(sheet_id)
#}

## Begin App Build
# Sidebar definition
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("Workflow stats", tabName = "stats"),
    menuItem("Progress", tabName = "progress"),
    menuItem("Digi Progress", tabName = "digi_progress")
  )
)

# Body definition
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            h2("About this dashboard..."),
            h4("The intent of this dashboard is to track and visualize the progress of Dior Herbariomix project.")
    ),
    tabItem(tabName = "stats",
            h2("Progress by each step of the workflow"),
            DT::dataTableOutput("data_table"),
            h2("Digitization progress"),                 # New header for the second table
            DT::dataTableOutput("digitization_table") # Output for the second table
    ),
    tabItem(tabName = "progress",
            h2("Workflow progress"),
            uiOutput("plots_ui_progress")
    ),
    tabItem(tabName = "digi_progress",
            h2("Digitization progress by family"),
            uiOutput("digitization_plots_ui")
    )
  )
)

# Combine header, sidebar, and body into the UI
ui <- dashboardPage(
  dashboardHeader(title = "Dior Herbariomix Dashboard", titleWidth = 350),
  sidebar,
  body
)

server <- function(input, output) {
  
  # Load Data upon URL Load
  dat <- herbariomix_app %>%
    select(Step, 'species completed', 'species remaining', 'specimens completed', 'specimens remaining')%>% 
    mutate(
      `Total number of species` = `species completed` + `species remaining`,
      `Total number of specimens` = `specimens completed` + `specimens remaining`,
      `Completed species (%)` = round((`species completed` / `Total number of species`) * 100, 1),
      `Remaining species (%)` = round((100 - `Completed species (%)`), 1),
      `Completed specimens (%)` = round((`specimens completed` / `Total number of specimens`) * 100, 1),
      `Remaining specimens (%)` = round((100 - `Completed specimens (%)`), 1)
    ) %>% select(-`Total number of species`, -`Total number of specimens`)
  
  # rename the columns 'species completed' to 'Number of species completed' and 'species remaining' to 'Number of species remaining'
  # colnames(dat) <- c("Step", "Number of species completed", "Number of species remaining", "Number of specimens completed", 
  #                     "Number of specimens remaining", "Completed species (%)", "Remaining species (%)", "Completed specimens (%)", "Remaining specimens (%)")
  
  # Render the main data table
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      dat,
      options = list(dom = 'ft', scrollX = TRUE)
    )
  })
  
  # Render the digitization table
  output$digitization_table <- DT::renderDataTable({
    DT::datatable(
      digitization_records,
      options = list(dom = 't', scrollX = TRUE)
    )
  })
  
  ## Plots
  # Function to create pie charts with percentages
  create_pie_chart <- function(step, completed, remaining, completed_percent, remaining_percent, title){
    
    pie_data <- data.frame(
      Category = c("Completed", "Remaining"),
      Count = c(completed, remaining),
      Percentage = c(completed_percent, remaining_percent)
    ) %>%
      mutate(Label = paste0(Category, ": ", round(Percentage, 1), "%")) %>% 
      mutate(WrappedLabel = str_wrap(Label, width = 10))  # Wrap text at 10 character
    
    ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(
        plot.margin = margin(0, 0, 0, 0, "cm"),
        legend.position = "none"
      ) +
      geom_text(aes(x = 1.1, label = WrappedLabel), size = 6, position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = c("Completed" = "#ef8a62", "Remaining" = "#67a9cf"))
  }
  
  # Create lists to store the pie charts
  species_pie_charts <- list()
  specimens_pie_charts <- list()
  
  # Create pie charts for each step
  steps <- dat$Step
  for (i in 1:length(steps)) {
    step <- steps[i]
    
    # Species Pie Charts
    species_completed <- dat$`species completed`[i]
    species_remaining <- dat$`species remaining`[i]
    species_completed_percent <- dat$`Completed species (%)`[i]
    species_remaining_percent <- dat$`Remaining species (%)`[i]
    
    species_pie_chart <- create_pie_chart(step, species_completed, species_remaining, species_completed_percent, species_remaining_percent, paste(step, "- Species"))
    species_pie_charts[[i]] <- species_pie_chart
    
    # Specimens Pie Charts
    specimens_completed <- dat$`specimens completed`[i]
    specimens_remaining <- dat$`specimens remaining`[i]
    specimens_completed_percent <- dat$`Completed specimens (%)`[i]
    specimens_remaining_percent <- dat$`Remaining specimens (%)`[i]
    
    specimens_pie_chart <- create_pie_chart(step, specimens_completed, specimens_remaining, specimens_completed_percent, specimens_remaining_percent, paste(step, "- Specimens"))
    specimens_pie_charts[[i]] <- specimens_pie_chart
  }
  
  colnames(dat) <- c("Step", "Number of species completed", "Number of species remaining", "Number of specimens completed", 
                     "Number of specimens remaining", "Completed species (%)", "Remaining species (%)", "Completed specimens (%)", "Remaining specimens (%)")

  
  # Render the plots dynamically in the UI
  output$plots_ui_progress <- renderUI({
    plot_output_list <- lapply(1:length(species_pie_charts), function(i) {
      fluidRow(
        box(
          title = paste(steps[i], "- Species Progress"), status = "primary", solidHeader = TRUE, width = 6,
          plotOutput(outputId = paste0("species_plot_", i), height = 400)
        ),
        box(
          title = paste(steps[i], "- Specimens Progress"), status = "primary", solidHeader = TRUE, width = 6,
          plotOutput(outputId = paste0("specimens_plot_", i), height = 400)
        )
      )
    })
    do.call(tagList, plot_output_list)
  })
  
  # Render each species and specimens plot in the list
  for (i in 1:length(species_pie_charts)) {
    local({
      my_i <- i
      
      # Render species plot
      output[[paste0("species_plot_", my_i)]] <- renderPlot({
        species_pie_charts[[my_i]]
      })
      
      # Render specimens plot
      output[[paste0("specimens_plot_", my_i)]] <- renderPlot({
        specimens_pie_charts[[my_i]]
      })
    })
  }
  
  # Create list to store plots for digitization progress
  digitization_charts <- list()
  
  # Calculate the maximum value of "Records transcribed" across all data
  max_y <- max(digitization_records$`Records transcribed`, na.rm = TRUE)
  
  # Get the list of unique families
  families <- unique(digitization_records$Family)
  
  # Loop over each family
  for (i in seq_along(families)) {
    family <- families[i]
    
    # Subset data for the current family
    data_family <- digitization_records[digitization_records$Family == family, ]
    
    # Calculate the number of bars (genera) for this family
    num_genus <- nrow(data_family)
    
    # Adjust the aspect ratio based on the number of genera
    aspect_ratio <- max(1, 3 / num_genus)
    
    # Create the plot using ggpubr's ggbarplot
    plot <- ggbarplot(
      data_family,
      x = "Genus",
      y = "Records transcribed",
      fill = "Genus",
      color = "Genus",
      label = TRUE,               # Add value labels on top of bars
      lab.vjust = -0.5,           # Adjust vertical position of labels
      width = 0.5,                # Adjust bar width
      ylim = c(0, max_y * 1.1),   # Set consistent y-axis limits
      x.text.angle = 45,          # Rotate x-axis text
      ggtheme = theme_classic()   # Set the theme (you can choose another theme if desired)
    ) +
      font("y.text", size = 14) +  # Adjust font size of y axis tick labels
      font("x.text", size = 12) + # Adjust font size of x axis tick labels
      font("xy.title", size = 12) + # Adjust font size of x and y axis names
      theme(
        aspect.ratio = aspect_ratio,
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "none"  # Remove the legend
      ) +
      labs(
        x = "Genus",
        y = "Number of Records"
      )
    
    # Store plots in the list I created earlier
    digitization_charts[[i]] <- plot
  }
  
  # Dynamically generate UI elements for the digitization plots
  output$digitization_plots_ui <- renderUI({
    plot_output_list <- list()
    num_plots <- length(digitization_charts)
    row_elements <- list()
    accumulated_width <- 0
    
    for (i in seq(1, num_plots)) {
      plotname <- paste0("digitization_plot_", i)
      num_genus <- nrow(digitization_charts[[i]]$data)
      
      # Determine box width based on number of genera
      box_width <- if (num_genus <= 2) {
        2
      } else if (num_genus <= 8) {
        4
      } else {
        6
      }
      
      # Create the box
      plot_box <- box(
        title = families[i], status = "primary", solidHeader = TRUE, width = box_width,
        plotOutput(outputId = plotname, height = 400)
      )
      
      # Add the box to the current row
      row_elements[[length(row_elements) + 1]] <- plot_box
      accumulated_width <- accumulated_width + box_width
      
      # Check if the row is full or if it's the last plot
      if (accumulated_width >= 12 || i == num_plots) {
        # Add the row to the plot_output_list
        plot_output_list[[length(plot_output_list) + 1]] <- fluidRow(row_elements)
        # Reset for the next row
        row_elements <- list()
        accumulated_width <- 0
      }
    }
    do.call(tagList, plot_output_list)
  })
  
  # Render each plot in the digitization_charts list
  for (i in 1:length(digitization_charts)) {
    local({
      my_i <- i
      plotname <- paste0("digitization_plot_", my_i)
      output[[plotname]] <- renderPlot({
        digitization_charts[[my_i]]
      })
    })
  }
}

shinyApp(ui, server)

