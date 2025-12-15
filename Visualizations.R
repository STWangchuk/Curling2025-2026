# Effectively an extension of the Analysis script with the visualizations

model_data <- readRDS("Data.rds")


accuracy_plot <- ggplot(accuracy_by_shot, aes(x = ShotID, y = Accuracy)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  geom_text(aes(label = sprintf("%.2f", Accuracy)), vjust = -1, color = "black") +
  geom_hline(yintercept = conf_mat$overall['Accuracy'], linetype = "dashed", color = "red") +
  annotate("text", x = 9, y = conf_mat$overall['Accuracy'] - 0.01,
           label = paste("Overall Accuracy:", sprintf("%.4f", conf_mat$overall['Accuracy'])),
           color = "red", size = 4) +
  scale_x_continuous(breaks = unique(accuracy_by_shot$ShotID)) +
  scale_y_continuous(labels = scales::percent, limits = c(0.7, 1)) +
  labs(title = "XGBoost Model Accuracy Across the End (by Shot ID)",
       x = "Shot ID (1 = First Stone, 10 = Last Stone)",
       y = "Win Prediction Accuracy") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



steal_heatmap <- ggplot(matrix_data, aes(x = Shot9, y = Final, fill = Count)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", Count, Percentage)), size = 5, fontface = "bold", color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Team 1 Shot 9 Status vs Final Outcome",
       x = "Status After Shot 9", y = "Final End Result") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))



hammer_analysis <- model_data %>%
  filter(!is.na(Hammer)) %>%
  group_by(Hammer) %>%
  summarise(
    Win_Percentage = (sum(Win == 1, na.rm = TRUE) / n()) * 100,
    .groups = 'drop'
  ) %>%
  mutate(Hammer_Status = ifelse(Hammer == 1, "Team 1 Has Hammer", "Team 2 Has Hammer"))

hammer_plot <- ggplot(hammer_analysis, aes(x = Hammer_Status, y = Win_Percentage, fill = Hammer_Status)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", Win_Percentage)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  labs(title = "Team 1 Win Percentage by Hammer Status",
       x = "Hammer Status",
       y = "Win Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")



powerplay_analysis <- model_data %>%
  mutate(
    PowerPlay_Status = case_when(
      Team1_Using_PowerPlay == TRUE ~ "Team 1 PowerPlay",
      Team2_Using_PowerPlay == TRUE ~ "Team 2 PowerPlay",
      TRUE ~ "No PowerPlay"
    )
  ) %>%
  group_by(PowerPlay_Status) %>%
  summarise(
    Win_Percentage = (sum(Win == 1, na.rm = TRUE) / n()) * 100,
    .groups = 'drop'
  )

powerplay_plot <- ggplot(powerplay_analysis, aes(x = PowerPlay_Status, y = Win_Percentage, fill = PowerPlay_Status)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", Win_Percentage)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  labs(title = "Team 1 Win Percentage by PowerPlay Status",
       x = "PowerPlay Status",
       y = "Win Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")



top_10_importance <- importance_matrix %>%
  head(10) %>%
  mutate(Feature = factor(Feature, levels = rev(Feature)))

importance_plot <- ggplot(top_10_importance, aes(x = Gain, y = Feature, fill = Gain)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f%%", Gain * 100)), 
            hjust = -0.1, size = 4, fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 10 Most Important Features for Win Prediction",
       x = "Gain (% Contribution to Model)",
       y = "Feature") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )



