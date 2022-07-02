library(tidyverse)
library(nflfastR)
library(ggimage)
library(ggthemes)
library(gt)
library(shiny)
library(gsisdecoder)
library(gtExtras)
library(vip)

roster <- nflfastR::fast_scraper_roster(2021)

nfl_data <- load_pbp(2021)

nfl_data <- nfl_data %>% filter(rush == 1)

nfl2 <- nflfastR::decode_player_ids(nfl_data)

nfl1 <- left_join(nfl2, roster, by = c('id' = 'gsis_id'))

run_plays <- nfl1 %>%
    filter(rush == 1) %>%
    filter(!is.na(rushing_yards), !is.na(down), !is.na(score_differential),
           !is.na(ydstogo), !is.na(half_seconds_remaining))

run_play_model <- run_plays %>%
    select(rushing_yards, down, ydstogo, half_seconds_remaining) %>%
    mutate(down = as.factor(down))

str(run_play_model)
colSums(is.na(run_play_model))

# Make the linear model
rush_yards_lm <- lm(rushing_yards ~ down + ydstogo + half_seconds_remaining, 
                    data = run_play_model)

summary(rush_yards_lm)

vip(rush_yards_lm, num_features = 4) + theme_fivethirtyeight()

# Get predictions 

rush_yards_pred <- data.frame(predict.lm(rush_yards_lm, newdata = run_play_model)) %>%
    rename(exp_rush_yards = predict.lm.rush_yards_lm..newdata...run_play_model.)


rush_yards_projs <- cbind(run_plays, rush_yards_pred)

# Leaders in air yards over expected in 2021
ryoe <- rush_yards_projs %>%
    mutate(ryoe = rushing_yards - exp_rush_yards) %>%
    group_by(rusher, posteam) %>%
    summarize(rush_attempt = n(),
              avg_ryoe = mean(ryoe),
              total_ryoe = sum(ryoe),
              head = first(headshot_url))

ui <- shinyUI(fluidPage(navbarPage("NFL RYOE | 2021",
             tabPanel("Home",
                      mainPanel(
                        h2("Expected Rushing Yards | NFL"),
                        h5("RYOE is calculated using a linear regression model."),
                        p("Made by Robbie Dunne, H/T @LeeSharpeNFL and the NFLVerse team."),
                        h4("This app is powered by @nflfastR data", img(src='logo.png')))),
    tabPanel("Team Rankings",
            sidebarPanel(selectInput("team","Choose a team", choices = ryoe$posteam)),
            mainPanel(gt_output(outputId = "table")
            )
        ),
    tabPanel("Player Rankings",
             sidebarPanel(selectInput("team","Choose a team", choices = ryoe$posteam)),
             mainPanel(gt_output(outputId = "table")
             )
    )
    )
))


server <- function(input, output) {
    
    selectedData <- reactive({
        ryoe %>%
            filter(posteam == input$team) %>%
            arrange(-total_ryoe) %>%
            gt() %>%
            gt_theme_espn() %>%
            cols_align(align = "center",
                       columns = c(2:4)) %>%
            fmt_number(column = c(avg_ryoe, total_ryoe), decimals = 2) %>%
            tab_options(
                data_row.padding = px(1)
            ) %>%
            tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_column_labels(TRUE)
            ) %>%
            tab_header(
                title = md("Rushing Yards Over Expected"),
            ) %>%
            opt_all_caps() %>%
            tab_options(
                table.background.color = "white",
                column_labels.background.color = "#e9c46a",
                #row.striping.background_color = "#e0e0e0"
            ) %>%
            gt::cols_move_to_start(c(head)) %>%
            opt_table_font(
                font = list(
                    google_font("Montserrat"),
                    default_fonts()
                )
            ) %>%
            gt::cols_label(
                head = "",
                posteam = 'Team',
                rush_attempt = 'Total rushing attempts',
                avg_ryoe = 'Average RYOE',
                total_ryoe = 'Total RYOE'
            ) %>%
            text_transform(
                locations = cells_body(columns = c(head)),
                fn = function(x){
                    gt::web_image(x)
                }
            ) %>%
            data_color(
                columns = c(avg_ryoe),
                colors = scales::col_numeric(
                    palette = c("#3fc1c9", "white"),
                    domain = NULL
                )
            ) %>%
            tab_source_note(
                source_note = gt::html(
                    htmltools::tags$a(
                        href = "https://twitter.com/robbiejdunne", 
                        target = "_blank", 
                        "@robbiejdunne"
                    ) %>% 
                        as.character()
                )
            )
    })
    
    output$table <-
        render_gt(expr = selectedData(), height = "100%", width = "100%")
}

shinyApp(ui, server)
