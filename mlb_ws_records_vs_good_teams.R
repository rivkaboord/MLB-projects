library(baseballr)
library(rvest)
library(tidyverse)

# function to pull standings for each season, specifically with the record vs. winning teams

pull_record_vs_winning_teams <- function(year) {
    
    standings <- read_html(paste0('https://www.baseball-reference.com/leagues/majors/', year, '-standings.shtml')) %>% 
        html_nodes(xpath = '//comment()') %>% 
        html_text() %>%   
        paste(collapse = '') %>%   
        read_html() %>%   
        html_nodes('table') %>%    
        html_table() %>% 
        .[[2]] %>% 
        tibble()
    
    rec_vs_winning_teams <- standings %>% 
        rename(rec_vs_winning_teams = `=.500`) %>% 
        select(Tm, W, L, rec_vs_winning_teams) %>% 
        mutate(year = year)
    
    return(rec_vs_winning_teams)
    
}

# scraping just the expanded standings for 2022 from Baseball Reference

expanded_baseball_standings_22 <- read_html('https://www.baseball-reference.com/leagues/majors/2022-standings.shtml') %>% 
    html_nodes(xpath = '//comment()') %>% 
    html_text() %>%
    paste(collapse = '') %>%
    read_html() %>%   
    html_nodes('table') %>% 
    html_table() %>% 
    .[[1]] %>% 
    tibble() %>% 
    rename(rec_vs_winning_teams = `=.500`)

# find record vs. winning teams for teams currently in 2022 playoff position

current_playoff_teams_rec_vs_winning_teams <- expanded_baseball_standings_22 %>% 
    head(12) %>% 
    select(Tm, W, L, rec_vs_winning_teams) %>% 
    separate(
        col = rec_vs_winning_teams,
        into = c('wins_vs_winning_teams', 'losses_vs_winning_teams'),
        sep = '-',
        remove = TRUE
    ) %>% 
    mutate(wins_vs_winning_teams = as.numeric(wins_vs_winning_teams),
           losses_vs_winning_teams = as.numeric(losses_vs_winning_teams),
           total_gms_vs_winning_teams = (wins_vs_winning_teams + losses_vs_winning_teams),
           win_pct_vs_winning_teams = (wins_vs_winning_teams / total_gms_vs_winning_teams) %>% round(digits = 3)) %>% 
    arrange(desc(total_gms_vs_winning_teams))

current_playoff_teams_rec_vs_winning_teams %>% 
    select(-W, -L) %>% 
    arrange(desc(win_pct_vs_winning_teams))

# find all postseason teams since 1996, the first full year of Wild Card play

library(Lahman)

Teams <- Teams %>%
    select(yearID, teamID, franchID, teamIDBR)

TeamsFranchises <- TeamsFranchises %>% 
    select(franchID, franchName)

team_id <- Teams %>% 
    left_join(TeamsFranchises, by = c('franchID'))

postseason_teams <- SeriesPost %>% 
    left_join(team_id, by = c('teamIDwinner' = 'teamID', 'yearID')) %>% 
    left_join(team_id, by = c('teamIDloser' = 'teamID', 'yearID')) %>% 
    rename(loser = franchName.y,
           winner = franchName.x) %>% 
    select(yearID, round, winner, lgIDwinner, loser, lgIDloser, wins, losses, ties) %>% 
    mutate(
        winner = case_when(
            yearID >= 2012 ~ str_replace_all(winner, 'Florida', 'Miami'),
            TRUE ~ winner
        ) %>% 
            str_replace_all('Metropolitans', 'Mets'), 
        loser = case_when(
            yearID >= 2012 ~ str_replace_all(loser, 'Florida', 'Miami'),
            TRUE ~ loser
        ) %>%
            str_replace_all('Metropolitans', 'Mets') 
    ) %>%
    rename(year = yearID) %>% 
    filter(year >= 1996)

# find standings from 1996-2021 using the pull-record-vs_winning_teams() function

standings_1996_2021 <- map(c(1996:2021), pull_record_vs_winning_teams) %>% 
    data.table::rbindlist()

standings_1996_2021_cleaned <- standings_1996_2021 %>% 
    separate(
        col = rec_vs_winning_teams,
        into = c('wins_vs_winning_teams', 'losses_vs_winning_teams'),
        sep = '-',
        remove = TRUE
    ) %>% 
    mutate(
        wins_vs_winning_teams      = as.numeric(wins_vs_winning_teams),
        losses_vs_winning_teams    = as.numeric(losses_vs_winning_teams),
        total_gms_vs_winning_teams = (wins_vs_winning_teams + losses_vs_winning_teams),
        win_pct_vs_winning_teams   = (wins_vs_winning_teams / total_gms_vs_winning_teams) %>% round(digits = 3),
        Tm                         = Tm %>%  str_replace_all('Anaheim Angels', 'Los Angeles Angels of Anaheim'),
        year                       = as.numeric(year)) %>%
    filter(Tm != 'Average')

# all playoff teams' record vs. winning teams

playoff_winners <- postseason_teams %>% 
    select(year, winner) %>% 
    rename(team = winner)

playoff_losers <- postseason_teams %>% 
    select(year, loser) %>% 
    rename(team = loser)

playoff_teams_since_96 <- playoff_winners %>% 
    bind_rows(playoff_losers) %>% 
    group_by(year) %>% 
    distinct(team) %>% 
    ungroup() %>% 
    arrange(year)

playoff_teams_recs_vs_winning_teams <- playoff_teams_since_96 %>% 
    left_join(standings_1996_2021_cleaned, by = c('year' = 'year', 'team' = 'Tm')) %>% 
    summarize(
        mean_wins                          = mean(W) %>% round(1),
        mean_wins_vs_winning_teams         = mean(wins_vs_winning_teams) %>% round(1),
        mean_losses_vs_winning_teams       = mean(losses_vs_winning_teams) %>% round(1),
        mean_total_games_vs_winning_teams  = mean(total_gms_vs_winning_teams) %>% round(1),
        mean_win_pct_vs_winning_teams      = mean(win_pct_vs_winning_teams) %>% round(3),
        worst_win_pct_vs_winning_tms       = min(win_pct_vs_winning_teams),
        winning_recs_vs_winning_teams      = sum(win_pct_vs_winning_teams >= .500),
        losing_recs_vs_winning_teams       = sum(win_pct_vs_winning_teams < .500)
    )

playoff_teams_recs_vs_winning_teams

playoff_teams_recs_vs_winning_teams %>% 
    View()
    
# WS winners' record vs. winning teams

ws_winners_record_vs_winning_teams <-  postseason_teams %>% 
    filter(round == 'WS') %>% 
    select(year, round, winner) %>% 
    left_join(standings_1996_2021_cleaned, by = c('winner' = 'Tm', 'year' = 'year')) %>% 
    summarize(
        mean_wins                          = mean(W) %>% round(1),
        mean_wins_vs_winning_teams         = mean(wins_vs_winning_teams) %>% round(1),
        mean_losses_vs_winning_teams       = mean(losses_vs_winning_teams) %>% round(1),
        mean_total_games_vs_winning_teams  = mean(total_gms_vs_winning_teams) %>% round(1),
        mean_win_pct_vs_winning_teams      = mean(win_pct_vs_winning_teams) %>% round(3),
        worst_win_pct_vs_winning_tms       = min(win_pct_vs_winning_teams),
        winning_recs_vs_winning_teams      = sum(win_pct_vs_winning_teams >= .500),
        losing_recs_vs_winning_teams       = sum(win_pct_vs_winning_teams < .500)
    ) %>%
    mutate(round = 'won WS')

# WS losers/LCS winners' record vs. winning teams

lcs_winners_record_vs_winning_teams <-  postseason_teams %>% 
    filter(round == 'WS') %>% 
    select(year, round, loser) %>% 
    left_join(standings_1996_2021_cleaned, by = c('loser' = 'Tm', 'year' = 'year')) %>% 
    summarize(
        mean_wins                          = mean(W) %>% round(1),
        mean_wins_vs_winning_teams         = mean(wins_vs_winning_teams) %>% round(1),
        mean_losses_vs_winning_teams       = mean(losses_vs_winning_teams) %>% round(1),
        mean_total_games_vs_winning_teams  = mean(total_gms_vs_winning_teams) %>% round(1),
        mean_win_pct_vs_winning_teams      = mean(win_pct_vs_winning_teams) %>% round(3),
        worst_win_pct_vs_winning_tms       = min(win_pct_vs_winning_teams),
        winning_recs_vs_winning_teams      = sum(win_pct_vs_winning_teams >= .500),
        losing_recs_vs_winning_teams       = sum(win_pct_vs_winning_teams < .500)
    ) %>%
    mutate(round = 'won LCS')

## LCS losers/LDS winners' record vs. winning teams

lds_winners_record_vs_winning_teams <- postseason_teams %>% 
    filter(round == 'ALCS' | round == 'NLCS') %>% 
    select(year, round, loser) %>% 
    left_join(standings_1996_2021_cleaned, by = c('loser' = 'Tm', 'year' = 'year')) %>% 
    mutate(winners_vs_winning_teams = sum(win_pct_vs_winning_teams >= .500)) %>% 
    summarize(
        mean_wins                          = mean(W) %>% round(1),
        mean_wins_vs_winning_teams         = mean(wins_vs_winning_teams) %>% round(1),
        mean_losses_vs_winning_teams       = mean(losses_vs_winning_teams) %>% round(1),
        mean_total_games_vs_winning_teams  = mean(total_gms_vs_winning_teams) %>% round(1),
        mean_win_pct_vs_winning_teams      = mean(win_pct_vs_winning_teams) %>% round(3),
        worst_win_pct_vs_winning_tms       = min(win_pct_vs_winning_teams),
        winning_recs_vs_winning_teams      = sum(win_pct_vs_winning_teams >= .500),
        losing_recs_vs_winning_teams       = sum(win_pct_vs_winning_teams < .500)
    ) %>%
    mutate(round = 'won LDS')

## LDS losers' record vs. winning teams

lds_losers_record_vs_winning_teams <- postseason_teams %>% 
    filter(round %in% c('ALDS1', 'ALDS2', 'NLDS1', 'NLDS2')) %>% 
    select(year, round, loser) %>% 
    mutate(loser = case_when(year >= 2012 ~ str_replace_all(loser, 'Florida', 'Miami'),
                             TRUE ~ loser)) %>% 
    left_join(standings_1996_2021_cleaned, by = c('loser' = 'Tm', 'year' = 'year')) %>% 
    summarize(
        mean_wins                          = mean(W) %>% round(1),
        mean_wins_vs_winning_teams         = mean(wins_vs_winning_teams) %>% round(1),
        mean_losses_vs_winning_teams       = mean(losses_vs_winning_teams) %>% round(1),
        mean_total_games_vs_winning_teams  = mean(total_gms_vs_winning_teams) %>% round(1),
        mean_win_pct_vs_winning_teams      = mean(win_pct_vs_winning_teams) %>% round(3),
        worst_win_pct_vs_winning_tms       = min(win_pct_vs_winning_teams),
        winning_recs_vs_winning_teams      = sum(win_pct_vs_winning_teams >= .500),
        losing_recs_vs_winning_teams       = sum(win_pct_vs_winning_teams < .500)
    ) %>%
    mutate(round = 'lost LDS')

## find the differences in win rates vs. winning teams for each round of the playoffs

postseason_teams_rec_vs_winning_teams_summarized <-  
    bind_rows(ws_winners_record_vs_winning_teams,
              lcs_winners_record_vs_winning_teams,
              lds_winners_record_vs_winning_teams,
              lds_losers_record_vs_winning_teams) %>% 
    mutate(
        pct_winners_vs_winning_teams = (winning_recs_vs_winning_teams /
                                            (winning_recs_vs_winning_teams + losing_recs_vs_winning_teams)) %>% round(3)
    ) %>% 
    select(round, everything())

postseason_teams_rec_vs_winning_teams_summarized

postseason_teams_rec_vs_winning_teams_summarized_2 <- data.frame(t(postseason_teams_rec_vs_winning_teams_summarized[-1]))
colnames(postseason_teams_rec_vs_winning_teams_summarized_2) <- postseason_teams_rec_vs_winning_teams_summarized[, 1]

library(writexl)

write_excel_csv(postseason_teams_rec_vs_winning_teams_summarized, file = 'postseason_teams_recs_vs_winning_teams_by_round_2.csv')


lds_losers <- postseason_teams %>% 
    filter(round %in% c('ALDS1', 'ALDS2', 'NLDS1', 'NLDS2')) %>% 
    select(year, round, loser) %>% 
    mutate(loser = case_when(year >= 2012 ~ str_replace_all(loser, 'Florida', 'Miami'),
                             TRUE ~ loser)) %>% 
    left_join(standings_1996_2021_cleaned, by = c('loser' = 'Tm', 'year' = 'year')) %>% 
    mutate(win_pct = (W/(W+L)) %>% round(3),
           round_lost = 1) %>% 
    select(win_pct, win_pct_vs_winning_teams, round_lost)

lcs_losers <- postseason_teams %>% 
    filter(round %in% c('ALCS', 'NLCS')) %>% 
    select(year, round, loser) %>% 
    mutate(loser = case_when(year >= 2012 ~ str_replace_all(loser, 'Florida', 'Miami'),
                             TRUE ~ loser)) %>% 
    left_join(standings_1996_2021_cleaned, by = c('loser' = 'Tm', 'year' = 'year')) %>% 
    mutate(win_pct = (W/(W+L)) %>% round(3),
           round_lost = 2) %>% 
    select(win_pct, win_pct_vs_winning_teams, round_lost)

ws_losers <- postseason_teams %>% 
    filter(round == 'WS') %>% 
    select(year, round, loser) %>% 
    mutate(loser = case_when(year >= 2012 ~ str_replace_all(loser, 'Florida', 'Miami'),
                             TRUE ~ loser)) %>% 
    left_join(standings_1996_2021_cleaned, by = c('loser' = 'Tm', 'year' = 'year')) %>% 
    mutate(win_pct = (W/(W+L)) %>% round(3),
           round_lost = 3) %>% 
    select(win_pct, win_pct_vs_winning_teams, round_lost)

ws_winners <- postseason_teams %>% 
    filter(round == 'WS') %>% 
    select(year, round, winner) %>% 
    mutate(loser = case_when(year >= 2012 ~ str_replace_all(winner, 'Florida', 'Miami'),
                             TRUE ~ winner)) %>% 
    left_join(standings_1996_2021_cleaned, by = c('loser' = 'Tm', 'year' = 'year')) %>% 
    mutate(win_pct = (W/(W+L)) %>% round(3),
           round_lost = 4)%>% 
    select(win_pct, win_pct_vs_winning_teams, round_lost)

library(ggplot2)
library(broom)

lds_losers %>% 
    bind_rows(lcs_losers, ws_losers, ws_winners) %>% 
    lm(formula = round_lost ~ win_pct_vs_winning_teams) %>% 
    summary()
