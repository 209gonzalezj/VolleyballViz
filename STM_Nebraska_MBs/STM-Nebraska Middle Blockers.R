library(ncaavolleyballr)
library(dplyr)
library(lubridate)
library(showtext)
library(ggplot2)

#Nebraska font by Daniel Heikkinen
font_add("NebraskaFont", "/mnt/disk2/home/eduardo/MyDropbox/TIDY/nebraska-4.ttf")
showtext_auto()

year_team<-find_team_id(team = "Nebraska", year = 2024, sport = "WVB")
nebraska_contests<-find_team_contests(year_team)
contest_ids<-nebraska_contests$contest


#This For loop takes some time--wait for it to finish until the do.call
results_list <- list()

for (i in seq_along(contest_ids)) {
  contest_id <- contest_ids[i]
  
  result <- tryCatch({
    data <- player_match_stats(contest = contest_id, team = "Nebraska")
    data$contest_id <- contest_id
    data  
  }, error = function(e) {
    message(paste("Error in contest ID", contest_id, ":", e$message))
    NULL
  })
  
  if (!is.null(result)) {
    results_list[[length(results_list) + 1]] <- result
  }
}

contests_df <- do.call(rbind, results_list)


# Filter to desired Positions + team total
filtered_df <- contests_df %>%
  filter(P=="OH" | P=="MB" | P=="") %>%
  mutate(
    Date = ymd(Date),
    contest_order = dense_rank(Date)  # chronological order
  )

rm(contests_df,results_list)

# I am going to combine the data for every 2 matches
filtered_df <- filtered_df %>%
  mutate(
    bin = ((contest_order - 1) %/% 2) + 1
  )


#Creating bin date (month of the first contest per bin)
bin_dates <- filtered_df %>%
  group_by(bin) %>%
  summarise(
    start_date = min(Date),
    .groups = "drop"
  ) %>%
  mutate(
    month_group = floor_date(start_date, "month")
  )


#Summarize stats by Position (and Team) and bin
summary_df <- filtered_df %>%
  group_by(P, bin) %>%
  summarise(
    total_Kills = sum(Kills, na.rm = TRUE),
    total_Errors = sum(Errors, na.rm = TRUE),
    total_Attacks = sum(TotalAttacks, na.rm = TRUE),
    .groups = "drop"
  )

# Join date range info back in
summary_df <- summary_df %>%
  left_join(bin_dates, by = "bin") %>%
  select(P, bin, month_group, total_Kills, total_Errors, total_Attacks)

#Identifying the Team stats
summary_df$P[summary_df$P == ""] <- "TEAM"

df<-summary_df

team_attacks <- df %>%
  filter(P == "TEAM") %>%
  select(month_group,bin, team_Attacks = total_Attacks)


##"ncaavolleyballr" data uses "OH" for all pin hitters positions

positions_df <- df %>%
  filter(P =="MB" | P=="OH") %>%
  left_join(team_attacks, by = c("bin","month_group")) %>%
  mutate(Attacks_pct = total_Attacks / team_Attacks * 100)


# Spring 2025 games are not included the "ncaavolleyballr" package, so we have to add the data manually
# I am combining the 2025-04-26 game, Nebraska vs Kansas, and the 2025-05-03 game Nebraska vs South Dakota SU 

col_names <- c("P", "bin", "date_range", "total_Kills", "total_Errors", "total_Attacks",
               "team_Attacks", "Attacks_pct", "start_date", "month_group")

mb <- c("MB", 19, "2025-04-26 to 2025-05-03", 32, 5, 62, 185, 33.5135, "2025-04-26", "2025-04-01")
oh <- c("OH", 19, "2025-04-26 to 2025-05-03", 52, 19, 113, 185, 61.0810, "2025-04-26", "2025-04-01")

new_data <- as.data.frame(rbind(mb, oh), stringsAsFactors = FALSE)
colnames(new_data) <- col_names

new_data <- new_data %>%
  mutate(
    bin = as.integer(bin),
    total_Kills = as.integer(total_Kills),
    total_Errors = as.integer(total_Errors),
    total_Attacks = as.integer(total_Attacks),
    team_Attacks = as.integer(team_Attacks),
    Attacks_pct = as.numeric(Attacks_pct),
    start_date = as.Date(start_date),
    month_group = as.Date(month_group),
    P = factor(P, levels = c("MB", "OH"))
  )

# Add to our position df
positions_df <- bind_rows(positions_df, new_data)

rm(new_data,summary_df,team_attacks)

#Viz

ggplot(positions_df, aes(x = factor(bin), y = Attacks_pct, fill = P)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  
# Dashed vertical line separating 2024 season from Spring 2025 matches
  geom_vline(xintercept = which(levels(factor(positions_df$bin)) == "19") - 0.5,
             linetype = "dashed", color = "gray", linewidth = 1) +
  
  scale_fill_manual(values = c("MB" = "black", "OH" = "#e41c38")) +
  
# Only label one x-tick per month
  {
    tick_labels <- positions_df %>%
      group_by(month_group) %>%
      slice(1) %>%
      mutate(label = format(month_group, "%b %Y")) %>%
      select(bin, label)
    label_vector <- setNames(tick_labels$label, tick_labels$bin)
    scale_x_discrete(breaks = names(label_vector), labels = label_vector)
  } +
  
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Set The Middle",
    subtitle = "attack % by Nebraska's Middle Blockers and Pin Hitters in 2024",
    x = "",
    y = "Attack %",
    fill = "Position"
  ) +

  theme(plot.title = element_markdown(size = 20, hjust = 0.06),
        plot.subtitle = element_markdown(size=12, hjust = 0.1),
        plot.caption = element_text(margin = margin(t = 10)),
        panel.spacing.y = unit(1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 10),
        plot.background = element_rect(color = NA, fill = "#fdf2d9"),
        panel.background = element_rect(color = NA, fill = "#fdf2d9"),
        text = element_text(family = "NebraskaFont", size = 10, color = NULL),
        axis.title = element_text(family = "NebraskaFont", size = 10, color = "grey20"),
        legend.position = "bottom",
        legend.key = element_rect(colour = "black", linetype = "solid", linewidth = 1.25),
        legend.background = element_rect(color = "black", fill = "#ece5d3", linetype = "solid")
)+
        geom_segment(aes(x = 1.75, xend = 1.75, y = 25, yend = 40), color = "grey", linewidth = 0.5) +
        geom_segment(aes(x = 14.75, xend = 14.75, y = 25, yend = 40), color = "grey", linewidth = 0.5) +
        geom_segment(aes(x = 17.75, xend = 17.75, y = 26, yend = 41), color = "grey", linewidth = 0.5) +
        geom_segment(aes(x = 18.75, xend = 18.75, y = 34, yend = 49), color = "grey", linewidth = 0.5) +
  
        geom_text(aes(x = 1.85, y = 41, label = "Loss to SMU"), angle = 90, vjust = -0.5, hjust = 0, family = "NebraskaFont",
            size = 5)+
        geom_text(aes(x = 14.85, y = 41, label = "Loss to PSU"), angle = 90, vjust = -0.5, hjust = 0, family = "NebraskaFont",
            size = 5)+
        geom_text(aes(x = 17.85, y = 42, label = "Loss to PSU"), angle = 90, vjust = -0.5, hjust = 0, family = "NebraskaFont",
            size = 5)+
        geom_text(aes(x = 18.85, y = 50, label = "Spring 2025 Matches"), angle = 90, vjust = -0.5, hjust = 0, family = "NebraskaFont",
            size = 5)

