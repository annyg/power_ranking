library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)

# Define function to update Elo ratings with goal difference consideration
update_elo <- function(rating1, rating2, score1 = NULL, score2 = NULL, k_factor = 20) {
  expected_score_a <- 1 / (1 + 10^((rating2 - rating1) / 400))
  expected_score_b <- 1 - expected_score_a

  if(is.null(score1) || is.null(score2)) {
    return(list(expected_elo_score_a = expected_score_a, expected_elo_score_b = expected_score_b))
  }

  outcome_score1 <- ifelse(score1 > score2, 1, ifelse(score1 == score2, 0.5, 0))
  goal_diff <- abs(score1 - score2)
  G <- ifelse(goal_diff == 0, 1,
              ifelse(goal_diff == 1, 1,
                     ifelse(goal_diff == 2, 1.5,
                            (11 + goal_diff) / 8)))

  rating1_new <- rating1 + k_factor * G * (outcome_score1 - expected_score_a)
  rating2_new <- rating2 + k_factor * G * ((1 - outcome_score1) - (1 - expected_score_a))

  list(rating1_new = rating1_new, rating2_new = rating2_new, expected_elo_score_a = expected_score_a, expected_elo_score_b = expected_score_b)
}

ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage("Elo Rating Dashboard",
             tabPanel("Upload & Rankings",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("fileData", "Upload Excel File",
                                    accept = c(".xlsx", ".xls"))
                        ),
                        mainPanel(
                          tableOutput("rankings") # Display team rankings as a HTML table
                        )
                      )
             ),

             tabPanel("Plots",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("plotType", "Select Plot Type",
                                      choices = c("Points Over Season", "ELO Ranking", "Points vs ELO", "Heat Plot"))
                        ),
                        mainPanel(
                          plotOutput("teamPlot")
                        )
                      )
             )
  ))

server <- function(input, output) {
  data_reactive <- reactive({
    req(input$fileData)
    file_path <- input$fileData$datapath

    matches_serie <- read_excel(file_path)

    # Data preparation as per initial script
    matches <- matches_serie %>%
      separate(Resultat, into = c("Hjemme", "Borte"), sep = " - ", convert = TRUE, remove = FALSE) %>%
      mutate(
        Hjemme = as.numeric(Hjemme),
        Borte = as.numeric(Borte),
        week = isoweek(Dato)
      ) %>%
      select(Dato, Runde, week, Hjemmelag, Bortelag, Hjemme, Borte, Turnering) %>%
      rename(team_a = Hjemmelag, team_b = Bortelag, score_a = Hjemme, score_b = Borte) %>%
      arrange(Dato) %>%
      mutate(match = row_number())

    # Deriving Points, Goal Differential, and Reshaping to Long Format
    team_stats_by_round <- matches %>%
      mutate(
        points_a = case_when(
          score_a > score_b ~ 3,
          score_a == score_b ~ 1,
          TRUE ~ 0
        ),
        points_b = case_when(
          score_b > score_a ~ 3,
          score_a == score_b ~ 1,
          TRUE ~ 0
        ),
        gd_a = score_a - score_b,
        gd_b = score_b - score_a,
        score_against_a = score_b * -1,
        score_against_b = score_b * -1,
        opponent_a = team_b,
        opponent_b = team_a
      ) %>%
      pivot_longer(
        cols = c(team_a, team_b, opponent_a, opponent_b, score_a, score_b, score_against_a, score_against_b, gd_a, gd_b, points_a, points_b),
        names_to = c(".value", "team_type"),
        names_pattern = "(.*)_(a|b)"
      ) %>%
      arrange(match) %>%
      group_by(team, Turnering) %>%
      complete(
        week = full_seq(13:max(week), 1),
        fill = list(team_type = "bye")
      ) %>%
      mutate(
        games_played = cumsum(if_else(team_type %in% c("a", "b"), 1, 0)),
        total_points = cumsum(ifelse(is.na(points), 0, points)),
        total_gd = cumsum(ifelse(is.na(gd), 0, gd)),
        total_scored = cumsum(ifelse(is.na(score), 0, score)),
        total_scored_against = cumsum(ifelse(is.na(score_against), 0, score_against)),
        total_points_home = cumsum(if_else(team_type == "a", points, 0)),
        total_points_away = cumsum(if_else(team_type == "b", points, 0)),
        total_gd_home = cumsum(if_else(team_type == "a", gd, 0)),
        total_gd_away = cumsum(if_else(team_type == "b", gd, 0))
      ) %>%
      ungroup() %>%
      mutate(
        strength_score_static = round(total_points + (total_gd / games_played), 2),
        strength_score_home = round(total_points_home + (total_gd_home / games_played), 2),
        strength_score_away = round(total_points_away + (total_gd_away / games_played), 2),
        strength_score_home_away = round((strength_score_home + strength_score_away) / 2, 2),
        strength_score = strength_score_static
      )

    matches_with_strength <- matches %>%
      left_join(
        team_stats_by_round %>% select(team, strength_score, match),
        by = c("team_a" = "team", "match" = "match")
      ) %>%
      rename(strength_a = strength_score) %>%
      left_join(
        team_stats_by_round %>% select(team, strength_score, match),
        by = c("team_b" = "team", "match" = "match")
      ) %>%
      rename(strength_b = strength_score) %>%
      mutate(
        opponent_strength_a = strength_b,
        opponent_strength_b = strength_a
      )

    # Adding ELO Rankings to Matches
    initial_elo <- matches_with_strength %>%
      select(team_a) %>%
      distinct() %>%
      rename(team = team_a) %>%
      mutate(elo_rating = 1500)

    elo_ratings <- initial_elo

    for (i in seq_len(nrow(matches_with_strength))) {
      current_match <- matches_with_strength[i, ]
      rating_a <- elo_ratings$elo_rating[match(current_match$team_a, elo_ratings$team)]
      rating_b <- elo_ratings$elo_rating[match(current_match$team_b, elo_ratings$team)]

      if (is.na(current_match$score_a) || is.na(current_match$score_b)) {
        predicted_outcomes <- update_elo(rating_a, rating_b)
      } else {
        updated_ratings <- update_elo(rating_a, rating_b, current_match$score_a, current_match$score_b, k_factor = 40)

        elo_ratings <- elo_ratings %>%
          mutate(
            elo_rating = case_when(
              team == current_match$team_a ~ updated_ratings$rating1_new,
              team == current_match$team_b ~ updated_ratings$rating2_new,
              TRUE ~ elo_rating
            )
          )

        matches_with_strength[i, "elo_new_a"] <- updated_ratings$rating1_new
        matches_with_strength[i, "elo_new_b"] <- updated_ratings$rating2_new
      }

      matches_with_strength[i, "elo_outcome_a"] <- ifelse(is.na(current_match$score_a),
                                                          predicted_outcomes$expected_elo_score_a,
                                                          updated_ratings$expected_elo_score_a)
      matches_with_strength[i, "elo_outcome_b"] <- ifelse(is.na(current_match$score_b),
                                                          predicted_outcomes$expected_elo_score_b,
                                                          updated_ratings$expected_elo_score_b)
    }

    team_stats_by_round_w_strength <- team_stats_by_round %>%
      left_join(
        matches_with_strength %>% select(Runde, match, strength_a, strength_b, elo_new_a, elo_new_b, elo_outcome_a, elo_outcome_b),
        by = c("Runde", "match")
      ) %>%
      mutate(
        team_strength = case_when(team_type == "a" ~ strength_a, TRUE ~ strength_b),
        opponent_strength = case_when(team_type == "a" ~ strength_b, TRUE ~ strength_a),
        team_elo = case_when(team_type == "a" ~ elo_new_a, TRUE ~ elo_new_b),
        opponent_elo = case_when(team_type == "a" ~ elo_new_b, TRUE ~ elo_new_a),
        team_prediction_elo = case_when(team_type == "a" ~ elo_outcome_a, TRUE ~ elo_outcome_b),
        Dato = case_when(week == 13 ~ as.Date("2025-04-01"), TRUE ~ Dato),
        team_elo = case_when(week == 13 ~ 1500, TRUE ~ team_elo),
        net_strength_difference = team_strength - opponent_strength,
        expected_Performance = team_strength / (team_strength + opponent_strength),
        over_performance = net_strength_difference / opponent_strength,
        points_status = case_when(!is.na(score) | week == 13 ~ "Played", TRUE ~ "Projected"),
        points = case_when(
          !is.na(score) ~ points,
          TRUE ~ case_when(
            team_prediction_elo >= 0.55 ~ 3,
            team_prediction_elo < 0.55 & team_prediction_elo >= 0.45 ~ 1,
            TRUE ~ 0
          )
        )
      ) %>%
      group_by(team) %>%
      mutate(
        games_played = cumsum(if_else(team_type %in% c("a", "b"), 1, 0)),
        total_points = cumsum(ifelse(is.na(points), 0, points)),
        total_gd = cumsum(ifelse(is.na(gd), 0, gd)),
        total_scored = cumsum(ifelse(is.na(score), 0, score)),
        total_scored_against = cumsum(ifelse(is.na(score_against), 0, score_against)),
        total_points_home = cumsum(if_else(team_type == "a", points, 0)),
        total_points_away = cumsum(if_else(team_type == "b", points, 0)),
        total_gd_home = cumsum(if_else(team_type == "a", gd, 0)),
        total_gd_away = cumsum(if_else(team_type == "b", gd, 0)),
        game_outcome = case_when(points == 3 ~ "Win", points == 1 ~ "Draw", points == 0 ~ "Loss", TRUE ~ "Bye")
      ) %>%
      ungroup()

    team_rankings <- team_stats_by_round_w_strength %>%
      group_by(team) %>%
      summarize(
        total_points = sum(points, na.rm = TRUE),
        last_elo = last(team_elo),
        max_elo = max(team_elo, na.rm = TRUE),
        games_played = n(),
        avg_points_per_game = total_points / games_played,
        total_gd = sum(gd, na.rm = TRUE),
        avg_opponent_strength = mean(opponent_strength, na.rm = TRUE),
        avg_strength_diff = mean(net_strength_difference, na.rm = TRUE)
      ) %>%
      mutate(
        ranking_score_1 = total_points * 2 + total_gd,
        ranking_score_2 = total_points * 2 + total_gd + avg_opponent_strength
      ) %>%
      arrange(desc(total_points))

    # Data prep ####
    end_points_projected <- team_stats_by_round_w_strength %>%
      ungroup() %>%
      group_by(team) %>%
      slice_max(order_by = match, n = 1) %>%
      ungroup() %>%
      arrange(-total_points, -strength_score_static, -total_gd, team)

    end_points_played <- team_stats_by_round_w_strength %>%
      ungroup() %>%
      dplyr:: filter(points_status == "Played") %>%
      group_by(team) %>%
      slice_max(order_by = match, n = 1) %>%
      ungroup() %>%
      arrange(-total_points, -strength_score_static, -total_gd, team)

    # Determine limits dynamically
    date_minimum <- min(team_stats_by_round_w_strength$Dato) - weeks(1)
    date_maximum <- max(team_stats_by_round_w_strength$Dato) + weeks(1)

    # Calculate slopes and intercepts for each category
    lines_data <- end_points_played %>%
      group_by(Turnering) %>%
      summarise(points_min = min(total_points),
                points_max = max(total_points),
                elo_min = min(team_elo),
                elo_max = max(team_elo)) %>%
      mutate(slope = (elo_max - elo_min) / (points_max - points_min),
             intercept = elo_min - slope * points_min)

    team_stats_by_round_exp <- team_stats_by_round_w_strength %>%
      # select(team, games_played, total_points, gd, total_gd, strength_score_static, net_strength_difference) %>%
      arrange(team, -total_points, -total_gd, -games_played) %>%
      select(team) %>%
      distinct()

    #  Return the list of all the created objects
    list(
      matches_with_strength = matches_with_strength,
      team_stats_by_round_w_strength = team_stats_by_round_w_strength,
      team_rankings = team_rankings,
      end_points_projected = end_points_projected,
      end_points_played = end_points_played,
      date_minimum = date_minimum,
      date_maximum = date_maximum,
      lines_data = lines_data,
      team_stats_by_round_exp = team_stats_by_round_exp
    )

    })

  output$teamPlot <- renderPlot({
    plot_data <- data_reactive()

    matches_with_strength <- plot_data$matches_with_strength
    team_stats_by_round_w_strength <- plot_data$team_stats_by_round_w_strength
    team_rankings <- plot_data$team_rankings

    # Plot generation based on user selection
    if (input$plotType == "Points Over Season") {
      fig_points <- ggplot(team_stats_by_round_w_strength, aes(x = Dato, y = total_points, color = team)) +
        geom_hline(yintercept = 0, linetype = "longdash") +
        geom_line(aes(linetype = points_status), linewidth = 1, alpha = 0.8, show.legend = TRUE) +
        geom_point(aes(shape = game_outcome), size = 3, alpha = 0.8) +
        geom_text(data = team_stats_by_round_w_strength %>% group_by(team) %>% slice_tail(n = 1),
                  aes(label = team), hjust = -0.1, size = 4, angle = -10) +
        scale_y_continuous(breaks = seq(0, 100, by = 1), limits = c(0,NA), minor_breaks = NULL) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%d", limits = c(min(team_stats_by_round_w_strength$Dato), max(team_stats_by_round_w_strength$Dato))) +
        labs(title = "Total Team Points Over Season", subtitle = "With projected outcomes based on latest ELO ranking", x = "Date", y = "Total Points") +
        hrbrthemes::theme_ipsum_rc() +
        ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
        theme(legend.position = "bottom") +
        facet_wrap(~Turnering, ncol = 2)
      return(fig_points)
    }

    # Additional plot logic...

    if (input$plotType == "ELO Ranking") {
      fig_elo <- ggplot(team_stats_by_round_w_strength, aes(x = Dato, y = team_elo, color = team)) +
        geom_line(linewidth = 0.7, alpha = 0.8) +
        geom_line(aes(linetype = points_status), linewidth = 1, alpha = 0.8, show.legend = FALSE) +
        geom_point(aes(shape = game_outcome), size = 3, alpha = 0.8) +
        geom_text(data = team_stats_by_round_w_strength %>% group_by(team) %>% slice_tail(n = 1),
                  aes(label = team), hjust = -0.2, size = 4, angle = -10) +
        scale_y_continuous(breaks = seq(1000, 2000, by = 50), minor_breaks = NULL) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%d", limits = c(min(team_stats_by_round_w_strength$Dato), max(team_stats_by_round_w_strength$Dato))) +
        labs(title = "ELO Ranking", x = "Date", y = "ELO Rank") +
        hrbrthemes::theme_ipsum_rc() +
        ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
        theme(legend.position = "bottom") +
        facet_wrap(~Turnering, ncol = 2)
      return(fig_elo)
    }

    if (input$plotType == "Points vs ELO") {
      end_points <- team_stats_by_round_w_strength %>%
        group_by(team) %>%
        slice_max(order_by = match, n = 1) %>%
        ungroup() %>%
        arrange(-total_points, -strength_score, -total_gd, team)

      lines_data <- end_points %>%
        group_by(Turnering) %>%
        summarise(points_min = min(total_points),
                  points_max = max(total_points),
                  elo_min = min(team_elo),
                  elo_max = max(team_elo)) %>%
        mutate(slope = (elo_max - elo_min) / (points_max - points_min),
               intercept = elo_min - slope * points_min)

      fig_comp <- ggplot(end_points, aes(x = total_points, y = team_elo)) +
        geom_point(aes(color = team), size = 4, alpha = 0.8) +
        geom_text(aes(label = team), vjust = -1, hjust = 0.5) +
        geom_abline(data = lines_data, aes(slope = slope, intercept = intercept), linetype = "dashed", color = "gray") +
        scale_x_continuous(breaks = seq(0, 50, by = 1), minor_breaks = NULL) +
        scale_y_continuous(breaks = seq(1000, 2000, by = 50), minor_breaks = NULL) +
        labs(title = "Comparison of Team Rankings by Points and ELO", x = "Points Rank", y = "ELO Rank") +
        hrbrthemes::theme_ipsum_rc() +
        ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
        theme(legend.position = "none") +
        facet_wrap(~Turnering, ncol = 2, scales = "free")
      return(fig_comp)
    }

    if (input$plotType == "Heat Plot") {
      heatmap_data <- team_stats_by_round_w_strength %>%
        mutate(
          normalized_prediction = case_when(
            points == 3 ~ team_prediction_elo * 1.5,
            points == 1 ~ (0.5 + 2 * team_prediction_elo) / 3,
            points == 0 ~ (team_prediction_elo + 0.5) * 1.5 / 3
          ),
          prediction_distance = abs(normalized_prediction * 3 - points),
          prediction_gradient = 1 - prediction_distance / 3
        ) %>%
        filter(points_status == "Played") %>%
        drop_na(opponent) %>%
        mutate(team = factor(team, levels = end_points$team)) %>%
        mutate(opponent = factor(opponent, levels = end_points$team))

      heatmap_plot <- ggplot(heatmap_data, aes(x = opponent, y = team)) +
        geom_tile(aes(fill = prediction_gradient), color = "darkgrey", width = 0.9, height = 0.9, size = 0.3) +
        geom_text(aes(label = game_outcome), color = "black", size = 4) +
        scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                             midpoint = 0.5, limits = c(0, 1),
                             breaks = c(0, 0.5, 1),
                             labels = c("Unpredicted Result", "Neutral", "Predicted Result"),
                             name = "Prediction Status") +
        labs(title = "Match Outcomes and Elo Predictions", x = "Opponent", y = "Team") +
        hrbrthemes::theme_ipsum_rc() +
        facet_wrap(~Turnering, ncol = 2, scales = "free") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(heatmap_plot)
    }
  })

  output$rankings <- renderTable({
    plot_data <- data_reactive()
    team_rankings <- plot_data$team_rankings
    print(team_rankings)
  })
}

shinyApp(ui = ui, server = server)
