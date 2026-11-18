# Calculate scored-to-missed fuel ratios
team_ratios <- match_data %>%
  filter(!is.na(`Team #`)) %>%
  group_by(`Team #`) %>%
  summarise(
    average_scored = mean(`Scored Fuel Tele`, na.rm = TRUE),
    average_missed = mean(`Missed Fuel Tele`, na.rm = TRUE),
    matches_played = sum(!is.na(`Scored Fuel Tele`)),
    .groups = "drop"
  ) %>%
  mutate(
    scored_missed_ratio = ifelse(
      average_missed > 0,
      average_scored / average_missed,
      NA_real_
    )
  )

# Calculate median average score
median_score <- median(team_ratios$average_scored, na.rm = TRUE)

# Divide teams into scoring groups
team_ratios <- team_ratios %>%
  mutate(
    scoring_group = ifelse(
      average_scored >= median_score,
      "Higher Average Scoring",
      "Lower Average Scoring"
    )
  ) %>%
  arrange(desc(average_scored))

# Print team results
print(team_ratios)

# Compare the two groups
group_comparison <- team_ratios %>%
  group_by(scoring_group) %>%
  summarise(
    average_ratio = mean(scored_missed_ratio, na.rm = TRUE),
    .groups = "drop"
  )

print(group_comparison)

# Create the ratio graph
ratio_chart <- ggplot(
  team_ratios,
  aes(
    x = reorder(factor(`Team #`), scored_missed_ratio),
    y = scored_missed_ratio,
    fill = scoring_group
  )
) +
  geom_col() +
  labs(
    title = "Average Scored-to-Missed Fuel Ratio by Team",
    subtitle = "Comparing higher-scoring and lower-scoring teams",
    x = "Team Number",
    y = "Average Scored Fuel / Average Missed Fuel",
    fill = "Scoring Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1
    )
  )

print(ratio_chart)

# Save graph
ggsave(
  "fuel_scored_missed_ratio.png",
  plot = ratio_chart,
  width = 12,
  height = 10,
  dpi = 200
)