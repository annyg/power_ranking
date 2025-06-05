library(tidyverse)
library(lubridate)
library(ggplot2)

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
date_minimum <- min(team_stats_by_round_w_strength$Dato, na.rm = TRUE) - weeks(1)
date_maximum <- max(team_stats_by_round_w_strength$Dato, na.rm = TRUE) + weeks(1)

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

# Point rankings and projections ####
fig_points <- ggplot(team_stats_by_round_w_strength, aes(x = Dato, y = total_points, color = team)) +
  geom_hline(yintercept = 0, linetype = "longdash") +                                 # Add horizontal lines
  geom_line(linetype = "dashed", linewidth = 1, alpha = 0.8) + # Base line without varying linetypes
  geom_line(aes(linetype = points_status),linewidth = 1, alpha = 0.8, show.legend = TRUE) +
  geom_point(aes(shape = game_outcome), size = 3, alpha = 0.8) +
  geom_text(data = end_points_projected,                      # Add labels to endpoints
            aes(label = team),
            hjust = -0.1,                           # Horizontal adjustment (push text slightly to the right)
            size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(0, 100, by = 1), limits = c(0,NA), minor_breaks = NULL) +   # Set y-axis breaks
  # scale_x_continuous(limits = c(0,6.5), breaks = seq(0, 7, by = 1), minor_breaks = NULL) +   # Set x-axis breaks
  scale_x_datetime(date_breaks = "1 month",  # Breaks at each week
                   date_labels = "%m-%d",  # Format for date labels
                   limits = c(date_minimum, date_maximum)) +
  labs(title = "Total Team Points Over Season", subtitle = "With projected outcomes based on latest ELO ranking", x = "Date", y = "Total Points") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  theme(legend.position = "bottom") +                   # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2)
fig_points

# Elo ranking plot ####
fig_elo <- ggplot(team_stats_by_round_w_strength, aes(x = Dato, y = team_elo, color = team)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_line(aes(linetype = points_status),linewidth = 1, alpha = 0.8, show.legend = FALSE) +
  geom_point(aes(shape = game_outcome), size = 3, alpha = 0.8) +
  geom_text(data = end_points_played,                      # Add labels to endpoints
            aes(label = team),
            hjust = -0.2,                           # Horizontal adjustment (push text slightly to the right)
            size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(1000, 2000, by = 50), minor_breaks = NULL) +   # Set y-axis breaks
  scale_x_datetime(date_breaks = "1 month",  # Breaks at each week
                   date_labels = "%m-%d",  # Format for date labels
                   limits = c(date_minimum, date_maximum)) +
  labs(title = "ELO Ranking", x = "Date", y = "ELO Rank") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  theme(legend.position = "bottom") +                  # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2)
fig_elo


# Compare points and Elo rankings ####
fig_comp <- ggplot(end_points_played, aes(x = total_points, y = team_elo)) +
  geom_point(aes(color = team), size = 4, alpha = 0.8) +
  geom_text(aes(label = team), vjust = -1, hjust = 0.5) +  # Add team names near points
  geom_abline(data = lines_data, aes(slope = slope, intercept = intercept), linetype = "dashed", color = "gray") +  # Line of equality
  scale_x_continuous(breaks = seq(0, 50, by = 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(1000, 2000, by = 50), minor_breaks = NULL) +
  labs(title = "Comparison of Team Rankings by Points and ELO",
       x = "Points Rank",
       y = "ELO Rank") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  theme(legend.position = "none") +                  # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2, scales = "free")
fig_comp


# Heat plot ####
# Visualization using a heatmap
heatmap_plot <- team_stats_by_round_w_strength %>%
  # V4
  # mutate(
  #   normalized_prediction = case_when(
  #     points == 3 ~ (team_prediction_elo^2) * 3,  # Applying square transformation for emphasis
  #     points == 1 ~ (0.5 + team_prediction_elo) * 1.5,
  #     points == 0 ~ (team_prediction_elo^2) * 3
  #   ),
  #   prediction_distance = abs(normalized_prediction - points),
  #   prediction_gradient = 1 - (prediction_distance / 9)^0.5  # Square root for more sensitivity
  # ) %>%
  mutate(
    # normalized_prediction = case_when(
    #   # V3
    #   points == 3 ~ team_prediction_elo * 3,      # Emphasize win scaling
    #   points == 1 ~ (abs(team_prediction_elo + 0.5) + 0.5) * 1,  # Tie balance and skew towards actual abs(team_prediction_elo - 0.5)
    #   points == 0 ~ team_prediction_elo * 3 #1.5 / 3  # Loss focus divergence
    #   # V2
    #   # points == 3 ~ team_prediction_elo * 1.5,      # Win emphasizing
    #   # points == 1 ~ (0.5 + 2 * team_prediction_elo) / 3,  # Skew towards tie clarity
    #   # points == 0 ~ (team_prediction_elo + 0.5) * 1.5 / 3 # Loss focus
    #   # V1
    #   # points == 3 ~ team_prediction_elo,
    #   # points == 1 ~ (0.5 + team_prediction_elo) / 2,  # Closer to tie
    #   # points == 0 ~ (team_prediction_elo + 0.5) / 2   # Closer to loss
    # ),
    # # V3
    # prediction_distance = abs(normalized_prediction - points),
    normalized_prediction = case_when(
      points == 3 ~ team_prediction_elo * 3,  # Emphasize win scaling
      points == 1 ~ 1.5,  # Set desired value for Elo exactly at 0.5 to 1.5
      points == 0 ~ team_prediction_elo * 3  # Emphasize loss
    ),
    prediction_distance = case_when(
      points == 1 ~ abs(team_prediction_elo - 0.5) * 3,  # Direct distance for tie to achieve proportional scaling
      TRUE ~ abs(normalized_prediction - points)
    ),
    prediction_distance = case_when(
      points == 1 ~ abs(team_prediction_elo - 0.5),  # Capture tie deviation
      TRUE ~ abs(normalized_prediction - points)
    ),
    prediction_gradient = case_when(
      points == 1 & team_prediction_elo == 0.5 ~ 1,
      points == 1 ~ 0.5 - (prediction_distance * 3 / 3),  # Start at 0.5 and decrease with deviation
      TRUE ~ 1 - prediction_distance / 3  # Adjust for other outcomes
    ),
    # prediction_gradient = 1 - prediction_distance / 3  # Correct for range alignment
    # V2
    # prediction_distance = abs(normalized_prediction * 3 - points),  # Scale factor
    # prediction_gradient = 1 - prediction_distance / 3  # Adjust for bounded format
    # prediction_gradient = abs(normalized_prediction - points)
  ) %>%
  mutate(
    prediction_gradient = case_when(points_status == "Played" ~ prediction_gradient,  # Only apply to played games
                                  TRUE ~ NA_real_)  # Set NA for unplayed games
  ) %>%
  # dplyr::filter(points_status == "Played") %>%
  drop_na(opponent) %>%
  mutate(team = factor(team, levels = end_points_played$team)) %>%
  mutate(opponent = factor(opponent, levels = end_points_played$team)) %>%
  # ggplot(aes(x = Runde, y = team)) +
  ggplot(aes(x = opponent, y = team)) +
  geom_tile(aes(fill = prediction_gradient), color = "darkgrey", width = 0.9, height = 0.9, size = 0.3) +
  geom_text(aes(label = game_outcome), color = "black", size = 4, nudge_y = 0.15) +  # Overlay text
  geom_text(aes(label = round(team_prediction_elo, digits = 2)), color = "black", size = 4, nudge_y = -0.15) +  # Overlay text
  scale_fill_gradient2(low = "red",
                       # mid = "yellow",
                       high = "green", na.value = "lightgrey",
                       midpoint = 0.5, limits = c(0, 1),
                       breaks = c(0, 0.5, 1),
                       labels = c("Unpredicted Result", "Neutral", "Predicted Result"),
                       name = "Prediction Status") +
  labs(title = "Match Outcomes and Elo Predictions",
       x = "Opponent",
       y = "Team") +
  hrbrthemes::theme_ipsum_rc() +
  # ggsci::scale_color_d3(palette = "category20", guide = 'none')  +
  # theme(legend.position = "none") +                  # Optionally remove legend
  facet_wrap(~Turnering, ncol = 2, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
heatmap_plot

fig_weighted_points <- team_stats_by_round_w_strength %>%
  group_by(team) %>%
  slice_max(order_by = match, n = 1) #%>%

fig_gd_total <- ggplot(team_stats_by_round, aes(x = games_played, y = total_gd, color = team)) +
  geom_hline(yintercept = 0, linetype = "longdash") +                                 # Add horizontal lines
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_text(data = end_points,                      # Add labels to endpoints
            aes(label = team),
            hjust = -0.2,                           # Horizontal adjustment (push text slightly to the right)
            size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(-100, 100, by = 2), minor_breaks = NULL) +   # Set y-axis breaks
  scale_x_continuous(limits = c(0,6.5), breaks = seq(0, 7, by = 1), minor_breaks = NULL) +   # Set x-axis breaks
  labs(title = "Total Goal Difference Over Games Played", x = "Games played", y = "Goal Difference") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3()  +
  theme(legend.position = "none")                   # Optionally remove legend
fig_gd_total

fig_gd <- ggplot(team_stats_by_round, aes(x = games_played, y = gd, color = team)) +
  geom_hline(yintercept = 0, linetype = "longdash") +                                 # Add horizontal lines
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_text(data = end_points,                      # Add labels to endpoints
            aes(label = team),
            hjust = -0.2,                           # Horizontal adjustment (push text slightly to the right)
            size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(-100, 100, by = 1), minor_breaks = NULL) +   # Set y-axis breaks
  scale_x_continuous(limits = c(0,6.5), breaks = seq(0, 7, by = 1), minor_breaks = NULL) +   # Set x-axis breaks
  labs(title = "Goal Difference for Each Game Played", x = "Games played", y = "Goal Difference") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3()  +
  theme(legend.position = "none")                   # Optionally remove legend
fig_gd

fig_str_static <- ggplot(team_stats_by_round, aes(x = games_played, y = strength_score_static, color = team)) +
  geom_hline(yintercept = 0, linetype = "longdash") +                                 # Add horizontal lines
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_text(data = end_points,                      # Add labels to endpoints
            aes(label = team),
            hjust = -0.2,                           # Horizontal adjustment (push text slightly to the right)
            size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(-100, 100, by = 1), minor_breaks = NULL) +   # Set y-axis breaks
  scale_x_continuous(limits = c(0,6.5), breaks = seq(0, 7, by = 1), minor_breaks = NULL) +   # Set x-axis breaks
  labs(title = "Team Static Strength Score Per Week", x = "Games played", y = "Static Strength Score") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_color_d3()  +
  theme(legend.position = "none")                   # Optionally remove legend
fig_str_static

fig_opp_strength <- team_stats_by_round_w_strength %>%
  mutate(team = factor(team, levels = as.list(end_points$team))) %>%
  # arrange(team, -total_points, -strength_score_static, -total_gd) %>%
  ggplot(aes(x = team, y = net_strength_difference, fill = team)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_boxplot() +
  geom_point(size = 2) +# Add horizontal lines
  # geom_line(linewidth = 0.7, alpha = 0.8) +
  # geom_text(data = end_points,                      # Add labels to endpoints
  #           aes(label = team),
  #           hjust = -0.2,                           # Horizontal adjustment (push text slightly to the right)
  #           size = 4, angle = -10) +                             # Font size
  scale_y_continuous(breaks = seq(-100, 100, by = 5), minor_breaks = NULL) +   # Set y-axis breaks
  # scale_x_dic(limits = c(0,6.5), breaks = seq(0, 7, by = 1), minor_breaks = NULL) +   # Set x-axis breaks
  labs(title = "Opponent Strength Difference", x = "Team", y = "Strength Difference") +
  hrbrthemes::theme_ipsum_rc() +
  ggsci::scale_fill_d3()  +
  theme(
    legend.position = "none",# Optionally remove legend
    axis.text.x = element_text(angle = 25, hjust = 1)# Rotate x-axis text)
    )
fig_opp_strength

library(patchwork)
fig_elo + fig_points + fig_comp +  # wrap_table(end_points, panel = "full", space = "fixed") + # gt::gt(end_points) +
  plot_layout(ncol = 1, nrow = 3) +
  plot_annotation(title = "Team Strength Metrics Over Games Played",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

g(team_stats_by_round_exp, panel = "full", space = "fixed")

pak::pak("hrbrthemes")

write_csv(team_stats_by_round_exp, "team_stats_by_round_exp.csv")
