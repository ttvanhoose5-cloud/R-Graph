library(readxl)
library(dplyr)
library(ggplot2)

# Select your Excel file
match_data <- read_excel(
  file.choose(),
  sheet = "Match_Data"
)

# Calculate average fuel scored per match
team_averages <- match_data %>%
  filter(!is.na(`Team #`)) %>%
  group_by(`Team #`) %>%
  summarise(
    average_scored = mean(`Scored Fuel Tele`, na.rm = TRUE),
    matches_played = sum(!is.na(`Scored Fuel Tele`)),
    .groups = "drop"
  ) %>%
  arrange(desc(average_scored))

# Check the averages
print(team_averages)

# Create the graph
fuel_chart <- ggplot(
  team_averages,
  aes(
    x = factor(`Team #`, levels = team_averages$`Team #`),
    y = average_scored
  )
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Teleop Fuel Scored per Match by Team",
    x = "Team Number",
    y = "Average Fuel Scored per Match"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1
    )
  )

print(fuel_chart)

# Save the graph
ggsave(
  "average_teleop_fuel_scored_by_team.png",
  plot = fuel_chart,
  width = 17,
  height = 7.5,
  dpi = 200
)

