#messing around
data <- load_pbp(2024)
head(data)
running <- data %>% select(matches("home_team|away_team"), matches("rush|run"))
sumrush <- sum(running$rushing_yards, na.rm = TRUE)
print(sumrush)

#cleaning, formatting
##just team basic info
teamlist <- load_teams(2024)

## grabs player rush stats each week 2018-2024... for those who logged a rush
playerstats <- load_player_stats(2018:2024) %>% 
  select(player_id, player_name, player_display_name, position, position_group, recent_team, season, week, opponent_team, carries, rushing_yards, rushing_tds, rushing_epa) %>%
  arrange(recent_team, week, opponent_team, season)

## rush total per team each week 2018-2024, based on recentteam col
rushtotalbygame <- playerstats %>%
  group_by(recent_team, opponent_team, week, season) %>%
  summarize(yardsperweek = sum(rushing_yards, na.rm = TRUE)) %>%
  arrange(season, week)

print(rushtotalbygame)

## taking winners of each game to merge with rushtotalbygame data
## this ultimately tells us if recent_team won or lost
gamewinners <- load_schedules(2018:2024)
home_won <- gamewinners %>%
  filter(result >= 0) %>%
  select(home_team, away_team, season, week)

home_lost <- gamewinners %>%
  filter(result < 0) %>%
  select(home_team, away_team, season, week)

### initialize a new result column in rushtotalbygame
rushtotalbygame$result <- NA

### assign "W" or "L" for games where team won at home by iterating through home_won
for (i in 1:nrow(home_won)) {
  rushtotalbygame$result[
    rushtotalbygame$recent_team == home_won$home_team[i] &
      rushtotalbygame$season == home_won$season[i] &
      rushtotalbygame$week == home_won$week[i]
  ] <- "W"
}

for (i in 1:nrow(home_won)) {
  rushtotalbygame$result[
    rushtotalbygame$recent_team == home_won$away_team[i] &
      rushtotalbygame$season == home_won$season[i] &
      rushtotalbygame$week == home_won$week[i]
  ] <- "L"
}

### assign "W" or "L" for games where team lost at home by iterating through home_lost
for (i in 1:nrow(home_lost)) {
  rushtotalbygame$result[
    rushtotalbygame$recent_team == home_lost$home_team[i] &
      rushtotalbygame$season == home_lost$season[i] &
      rushtotalbygame$week == home_lost$week[i]
  ] <- "L"
}

for (i in 1:nrow(home_lost)) {
  rushtotalbygame$result[
    rushtotalbygame$recent_team == home_lost$away_team[i] &
      rushtotalbygame$season == home_lost$season[i] &
      rushtotalbygame$week == home_lost$week[i]
  ] <- "W"
}

## view the updated rushtotalbygame dataset with wins and losses for each team
print(rushtotalbygame)


  