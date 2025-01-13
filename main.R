#messing around
data <- load_pbp(2024)
head(data)
running <- data %>% select(matches("home_team|away_team"), matches("rush|run"))
sumrush <- sum(running$rushing_yards, na.rm = TRUE)
print(sumrush)

#cleaning, formatting ----
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
  #summarize(carriesperweek = sum(carries, na.rm = TRUE)) %>%
  arrange(season, week)

print(rushtotalbygame)

## taking winners of each game to merge with rushtotalbygame data
## this ultimately tells us if recent_team won or lost
# recent_team W/L ----
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

## adding opponent_yardsperweek
rushtotalbygame <- rushtotalbygame %>%
  mutate(opponent_yardsperweek = 0)

for (i in 1:nrow(rushtotalbygame)) {
  current_opponent <- rushtotalbygame$opponent_team[i]
  current_week <- rushtotalbygame$week[i]
  current_season <- rushtotalbygame$season[i]
  
  for (j in 1:nrow(rushtotalbygame)) {
    if (rushtotalbygame$recent_team[j] == current_opponent &&
        rushtotalbygame$week[j] == current_week &&
        rushtotalbygame$season[j] == current_season) {
      rushtotalbygame$opponent_yardsperweek[i] = rushtotalbygame$yardsperweek[j]
      break
    }
  }
}

## construct data frame for wins and losses for each team when over 100 yards on week
teamrecordsbyweek <- rushtotalbygame %>%
  filter(yardsperweek >= 100) %>%
  group_by(recent_team) %>%
  summarise(wins = sum(result == "W", na.rm = TRUE), losses = sum(result == "L", na.rm = TRUE)) %>%
  ungroup()

## pool overall record
uniqueteam <- data.frame(recent_team = unique(rushtotalbygame$recent_team))
teamrecordsoverall <- uniqueteam %>%
  left_join(teamrecordsbyweek, by = "recent_team") %>%
  mutate(wins = ifelse(is.na(wins), 0, wins), losses = ifelse(is.na(losses), 0, losses))

#determining probability ----

##naive log regression approach, no other covar
win_model <- glm(result == "W" ~ yardsperweek + opponent_yardsperweek, data = rushtotalbygame, family = "binomial")

summary(win_model)

## probability with two covariates, unfinished
new_data <- data.frame(yardsperweek = 160, opponent_yardsperweek = 25)
predict(win_model, newdata = new_data, type = "response")

#notes ----
##This implementation is narrow, ignoring confounding##
##future question includes win outcomes based on advanced metrics##

playerstats2 <- load_player_stats(2024)
advancedstats <- load_nextgen_stats(
  seasons = 2024,
  stat_type = c("rushing"),
)

## assessing yards over expected per attempt with rate that rbs face 8 or more in the box
advseasontotalstats <- advancedstats %>%
  filter(week == "0")

ggplot(data = advseasontotalstats, aes(x = rush_yards_over_expected_per_att, y = percent_attempts_gte_eight_defenders)) +
  geom_point() +
  geom_text(aes(label = player_short_name), vjust = -0.5, size = 3)
  labs(
    title = "RBs",
    x = "Average Rush Yards Exceeded Per Carry",
    y = "Rate of 8 or More in the Box"
  )

