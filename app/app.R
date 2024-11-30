library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(patchwork)
library(ggiraph)
library(shinyWidgets)
library(shinythemes)
library(waiter)

# data <- read_csv("testing.csv")

# Loop through team files to get data file paths
my_files <- list.files(path=("data/"), pattern="*.csv", full.names = TRUE, recursive = TRUE)

players <- read_csv("player_lookup.csv")

# Read in files with the tidyverse
data <- read_csv(my_files, guess_max = 500) %>%
  mutate(result = case_when(type == "B" ~ "Ball",
                            type == "S" ~ "Strike",
                            type == "X" ~ "In Play"),
         game_date = ymd(game_date),
         month = month(game_date, label = TRUE, abbr = FALSE),
         on_base = 3 - rowSums(is.na(select(., c(on_3b, on_2b, on_1b)))),
         p_game_state = case_when(fld_score < bat_score ~ "Losing",
                                  bat_score < fld_score ~ "Winning",
                                  TRUE ~ "Tied")) %>% 
  left_join(players, by = join_by(pitcher == key_mlbam))

data$description <- str_replace_all(data$description, "_", " ") %>% str_to_title()
data$bb_type <- str_replace_all(data$bb_type, "_", " ") %>% str_to_title()

player_names <- data %>% 
  select(player_name) %>% 
  arrange(player_name) %>% 
  unique() %>% 
  pull()
all_innings <- data %>% 
  select(inning) %>% 
  arrange(inning) %>% 
  unique() %>% 
  pull()
all_plays <- data %>% 
  select(description) %>% 
  arrange(description) %>% 
  unique() %>% 
  pull()

ui <- fluidPage(theme = shinytheme("darkly"),
                useWaiter(),

  # Application title
  titlePanel("Looking at MLB Pitchers: 2024 Regular Season"),
  

  sidebarLayout(
    sidebarPanel(
      selectizeInput("player_name", choices = NULL, label = "Pitcher's Name"),
      uiOutput("month"),
      pickerInput("on_base", "Number of Players On Base",  choices = c(0, 1, 2, 3),
                  selected = c(0, 1, 2, 3), multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("batter_hand", "Batter Dominant Hand",  choices = c("Left", "Right"),
                  selected = c("Left", "Right"), multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("balls", "Balls in Count",  choices = c(0, 1, 2, 3),
                  selected = c(0, 1, 2, 3), multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("strikes", "Strikes in Count",  choices = c(0, 1, 2),
                  selected = c(0, 1, 2), multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("outs", "Outs",  choices = c(0, 1, 2),
                  selected = c(0, 1, 2), multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      uiOutput("pitch_name"), 
      pickerInput("pitch_number", "Pitch Number in At Bat", 
                  choices = c(1:max(data$pitch_number, na.rm = TRUE)),
                  selected = c(1:max(data$pitch_number, na.rm = TRUE)), multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("result", "Pitch Result",  choices = all_plays,
                  selected = all_plays, multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("top_bot", "Inning State",  choices = c("Top", "Bottom"),
                  selected = c("Top", "Bottom"), multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("inning", "Inning Number",  choices = all_innings,
                  selected = all_innings, multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      pickerInput("game_state", "Pitcher Game State",  choices = c("Losing", "Tied", "Winning"),
                  selected = c("Losing", "Tied", "Winning"), multiple = TRUE,
                  options = pickerOptions(actionsBox = TRUE)),
      hr(),
      pickerInput("fill_var", "Color Plots by...",
                  choices = c("Nothing", "Batter Dominant Hand", "Pitch Result", "Runners on Base",
                              "Pitcher Game State", "Pitch Name"),
                  selected = "Nothing"),
      pickerInput("bottom_plot", "Third Plot Type",
                  choices = c("Pitch Type Frequencies", "Pitch Type Clusters"),
                  selected = "Pitch Type Frequencies"),
      
      actionButton("generate", "Update Plots"),
      actionButton("reset", "Reset to Defaults"),
      width = 3
    ),
    

    mainPanel(
      div(htmlOutput("p_summary"), 
           style = "font-size:18px;text-align:center"),
      br(),
      girafeOutput("patchwork_plots")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  updateSelectizeInput(session, 'player_name', choices = player_names, server = TRUE)
  
  output$pitch_name <- renderUI({
    pickerInput('pitch_name', "Pitch Name",
                choices = data %>% filter(player_name == input$player_name) %>% 
                  select(pitch_name) %>% unique() %>% arrange() %>% pull(),
                selected = data %>% filter(player_name == input$player_name) %>% 
                  select(pitch_name) %>% unique() %>% arrange() %>% pull(),
                multiple = TRUE,
                options = pickerOptions(actionsBox = TRUE))
  })
  
  output$month <- renderUI({
    
    pickerInput("month", "Month",
                choices = data %>% filter(player_name == input$player_name) %>% 
                  select(month) %>% unique() %>% arrange() %>% pull(),
                selected = data %>% filter(player_name == input$player_name) %>% 
                  select(month) %>% unique() %>% arrange() %>% pull(),
                multiple = TRUE,
                options = pickerOptions(actionsBox = TRUE))
    
  })
  
  
  
  observeEvent(input$reset, {
    updateSelectizeInput(inputId = "player_name", choices = NULL, selected = player_names[1])
    updatePickerInput(inputId = "month", select = data %>% filter(player_name == input$player_name) %>% 
                        select(month) %>% unique() %>% arrange() %>% pull())
    updatePickerInput(inputId = "on_base", select = c(0, 1, 2, 3))
    updatePickerInput(inputId = "batter_hand", select = c("Left", "Right"))
    updatePickerInput(inputId = "balls", select = c(0, 1, 2, 3))
    updatePickerInput(inputId = "strikes", select = c(0, 1, 2))
    updatePickerInput(inputId = "outs", select = c(0, 1, 2))
    updatePickerInput(inputId = "pitch_name", select = data %>% filter(player_name == input$player_name) %>% 
                        select(pitch_name) %>% unique() %>% arrange() %>% pull())
    updatePickerInput(inputId = "pitch_number", select = c(1:max(data$pitch_number, na.rm = TRUE)))
    updatePickerInput(inputId = "result", select = all_plays)
    updatePickerInput(inputId = "top_bot", select = c("Top", "Bottom"))
    updatePickerInput(inputId = "inning", select = all_innings)
    updatePickerInput(inputId = "game_state", select = c("Losing", "Tied", "Winning"))
    updatePickerInput(inputId = "fill_var", select = "Nothing")
    updatePickerInput(inputId = "bottom_plot", select = "Pitch Type Frequencies")
  })
  
  rv <- reactiveValues()
  
  observeEvent(input$generate, {
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      paste0("Generating plots...")))
    waiter$show()
    
    # Set reactive values ####
    rv$player_name <- input$player_name
    rv$month <- input$month
    rv$on_base <- input$on_base
    rv$batter_hand <- input$batter_hand %>% case_match("Left" ~ "L",
                                                       "Right" ~ "R")
    rv$balls <- input$balls
    rv$strikes <- input$strikes
    rv$outs <- input$outs
    rv$pitch_name <- input$pitch_name
    rv$pitch_number <- input$pitch_number
    rv$result <- input$result
    rv$top_bot <- input$top_bot %>% case_match("Top" ~ "Top",
                                               "Bottom" ~ "Bot")
    rv$inning <- input$inning
    rv$game_state <- input$game_state
    rv$fill_var <- input$fill_var %>% case_match("Nothing" ~ "none",
                                                 "Batter Dominant Hand" ~ "stand",
                                                 "Pitch Result" ~ "type",
                                                 "Runners on Base" ~ "on_base",
                                                 "Pitcher Game State" ~ "p_game_state",
                                                 "Pitch Name" ~ "pitch_name")
    rv$fill_text <- input$fill_var
    rv$bottom_plot <- input$bottom_plot
    
    

    # Edit Original Data ####
    rv$player_data <- data %>% 
      filter(player_name == rv$player_name)
    
    rv$new_data <- rv$player_data %>%
      filter(on_base %in% rv$on_base) %>%
      filter(month %in% rv$month) %>% 
      filter(stand %in% rv$batter_hand) %>%
      filter(balls %in% rv$balls) %>%
      filter(strikes %in% rv$strikes) %>%
      filter(outs_when_up %in% rv$outs) %>%
      filter(pitch_name %in% rv$pitch_name) %>%
      filter(pitch_number %in% rv$pitch_number) %>%
      filter(description %in% rv$result) %>%
      filter(inning_topbot %in% rv$top_bot) %>%
      filter(inning %in% rv$inning) %>%
      filter(p_game_state %in% rv$game_state) %>% 
      mutate(tooltip = paste0("Speed: ", release_speed, " mph\n",
                              "Pitch Type: ", pitch_name, "\n",
                              "Pitch Result: ", description, "\n",
                              "Inning: ", paste(inning_topbot, inning), "\n",
                              "Month: ", month))
    
    output$p_summary <- renderText({
      
      p_name <- paste(rv$new_data$name_first[1], rv$new_data$name_last[1])
      
      HTML(paste0("Overall, in the Major League Baseball 2024 Regular Season:<br>", 
                  "<span style=\"color: #7570B3\"><b>709,510</b></span> pitches were thrown<br>",
                  "<span style=\"color: #7570B3\"><b>854</b></span> pitchers were used<br>",
                  "The most frequent pitch thrown was a <span style=\"color: #7570B3\"><b>4-Seam Fastball (225,387)</b></span><br>",
                  "Pitchers threw <span style=\"color: #7570B3\"><b>252,882</b></span> balls and <span style=\"color: #7570B3\"><b>332,427</b></span> strikes (<span style=\"color: #7570B3\"><b>124,201</b></span> went into play)<br><br>",

                  "You are viewing pitching data for ",
                  "<span style=\"color: #A6761D\"><b>", p_name, "</b></span>, ",
                  "who threw ",
                  "<span style=\"color: #A6761D\"><b>", nrow(rv$player_data), "</b></span>",
                  " of the 709,510 pitches in 2024.<br>",
                  "There are currently <span style=\"color: #A6761D\"><b>", nrow(rv$new_data), 
                  "</b></span> pitches displayed in the plots, according to the variables selected."))
    })

    rv$avg_sz_bot <- mean(rv$new_data$sz_bot, na.rm = TRUE)
    rv$avg_sz_top <- mean(rv$new_data$sz_top,  na.rm = TRUE)
    
    # "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" "#666666"
    # Dark2 Hex Codes
    output$patchwork_plots <- renderGirafe({
      
      if (rv$fill_var == "none") {line_color <- "#E7298A"} else {line_color <- "#000000"}
      
      release_point <- rv$new_data %>%
        ggplot(aes(x = release_pos_x, y = release_pos_z,
                   color = if(rv$fill_var == "none") {NULL} else {factor(.data[[rv$fill_var]])},
                   data_id = rownames(rv$new_data),
                   tooltip = tooltip)) +
        geom_point_interactive() +
        # coord_cartesian(xlim = c(-5, 5), ylim = c(0, 9)) +
        labs(title = "The Point in Space\nWhere a Pitcher Releases the Ball",
             subtitle = "From the Catcher's Perspective",
             x  = "Feet from Center Mound\n(Negative = Left)",
             y = "Feet from Ground",
             color = rv$fill_text) +
        scale_color_brewer(palette = "Dark2") +
        theme(panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "#CCCCCC"),
              axis.ticks = element_blank())

      plate_point <- rv$new_data %>%
        ggplot(aes(x = plate_x, y = plate_z,
                   color = if(rv$fill_var == "none") {NULL} else {factor(.data[[rv$fill_var]])},
                   data_id = rownames(rv$new_data),
                   tooltip = tooltip)) +
        geom_point_interactive() +
        geom_segment(x = -0.708, y = rv$avg_sz_bot, xend = 0.708, color = line_color) +
        geom_segment(x = -0.708, y = rv$avg_sz_top, xend = 0.708, color = line_color) +
        geom_segment(x = -0.708, y = rv$avg_sz_bot, yend = rv$avg_sz_top, color = line_color) +
        geom_segment(x = 0.708, y = rv$avg_sz_bot, yend = rv$avg_sz_top, color = line_color) +
        labs(title = "The Point in Space\nWhere a Pitch Crosses the Plate",
             subtitle = "From the Catcher's Perspective\nAverage Strike Zone Depicted",
             x = "Feet from Center Plate\n(0 = Center)",
             y = "Feet from Ground",
             color = rv$fill_text) +
        scale_color_brewer(palette = "Dark2") +
        theme(panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "#CCCCCC"),
              axis.ticks = element_blank())

      speed_spin <- rv$new_data %>%
        ggplot(aes(x = release_spin_rate, y = release_speed, 
                   color = if(rv$fill_var == "none") {NULL} else {factor(.data[[rv$fill_var]])},
                   data_id = rownames(rv$new_data))) +
        geom_point_interactive() +
        scale_color_brewer(palette = "Dark2") +
        labs(title = "Pitch Release Speed vs. Spin Rate",
             subtitle = "Based on Variable Selection",
             x = "Spin Rate at Release",
             y = "Pitch Speed at Release",
             color = rv$fill_text) +
        theme(panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "#CCCCCC"),
              axis.ticks = element_blank())
      
      pitch_types <- rv$new_data %>% 
        ggplot(aes(x = pitch_name,
               fill = if(rv$fill_var == "none") {NULL} else {factor(.data[[rv$fill_var]])},
               data_id = rownames(rv$new_data)),
               alpha = 1) +
        geom_bar_interactive() +
        labs(title = "Distribution of Pitch Types",
             subtitle = "Based on Variable Selection",
             x = "Pitch Name",
             y = "Count",
             fill = rv$fill_text) +
        scale_fill_brewer(palette = "Dark2") +
        theme(panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "#CCCCCC"),
              axis.ticks = element_blank())
      
      if (rv$bottom_plot == "Pitch Type Frequencies") {
        third_plot <- pitch_types
      } else if (rv$bottom_plot == "Pitch Type Clusters") {
        third_plot <- speed_spin
      }
      
      # djpr_girafe has error when customizing with @options
      girafe(ggobj = (release_point + plate_point) / third_plot + plot_layout(guides = "collect"), 
             width = 8, height = 8, 
             options = list(
               opts_tooltip(css = "background-color:#000000;color: #ffffff;padding: 5px; opacity: 1"),
               opts_hover(css = "fill:white;stroke:black;stroke-width:0.5px"),
               opts_selection(css = "fill:white;stroke:black;stroke-width:0.5px", type = "single")))
    })
    waiter$hide()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
