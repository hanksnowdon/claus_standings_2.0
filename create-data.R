library(shiny)
library(cfbfastR)
#library(tidyverse)
library(dplyr)
library(tibble)
library(stringr)
library(readr)
library(tidyr)

error_on_missing_name <- function(name) {
  var <- Sys.getenv(name, unset = NA)
  if(is.na(var)) {
    stop(paste0("cannot find ", name, " !"), call. = FALSE)
  }
  gsub("\"", "", var)
}

# Sys.setenv(CFBD_API_KEY = error_on_missing_name("CFB_SECRET"))
 

livegames <- cfbd_game_info(2024, season_type = "postseason") %>% 
  filter(home_division != "iii" & 
           home_division != "ii" & 
           !str_detect(notes, "(FCS|SWAC) Championship")) %>% 
  select(game_id, start_date, completed, home_id, home_team, home_points, away_id, away_team, away_points, notes)

write_csv(livegames, "/Users/hank/Downloads/bowlgames2024info.csv") #Download games to matchup game ids 
write_csv(livegames %>% select(home_team, home_id, away_team, away_id), "/Users/hank/Downloads/teamids.csv") #Download teams to matchup team ids

picks <- read_csv("/Users/hank/Downloads/testpicks.csv") %>%
 select( -Timestamp) %>%
 pivot_longer(cols = -c(Name), names_to = "game", values_to = "pick") %>%
 mutate(across(c(pick), ~ str_remove(.,"_(by|getting).*$"))) %>% mutate(pick = str_remove(str_remove(pick, "^(Underdog|Favorite): "), " by .*$"))

write_csv(unique(picks %>% select(pick) ), "/Users/hank/Downloads/teams1.csv") #Download teams from picks data to matchup team ids

gamelist <- read_csv("/Users/hank/Downloads/2024 bowl game list - Sheet1.csv") #Read in games with game ids
gamelist$Spread <- as.numeric(gamelist$Spread)
teamlist <- read_csv("/Users/hank/Downloads/teams.csv") #Read in teams with team ids

picksfinal <- picks %>%
 inner_join(gamelist, by = c("game" = "Bowl Name"))   %>%
 left_join(teamlist, by = "pick") %>% left_join(teamlist, by = c("Favorite" = "pick"))  %>% left_join(teamlist, by = c("Underdog" = "pick"))

#saveRDS(picksfinal, file='/Users/hank/Documents/Other/CLAuS-Shiny-App/picksfinal.rds') 
#saveRDS(teamlist, file='/Users/hank/Documents/Other/CLAuS-Shiny-App/teamlist.rds') 
picksfinal <- readRDS('picksfinal.rds')
teamlist <- readRDS('teamlist.rds')

results <- picksfinal %>% inner_join(livegames, by = "game_id") %>%
  rename(pick_id= team_id.x,
         fav_id = team_id.y,
         und_id = team_id) %>% 
  mutate(fav_points = ifelse(
    completed == TRUE & home_id == fav_id, home_points,
    ifelse(
      completed == TRUE & away_id == fav_id, away_points,
      NA
    )
  ), und_points = ifelse(
    completed == TRUE & home_id == und_id, home_points,
    ifelse(
      completed == TRUE & away_id == und_id, away_points,
      NA
    )
  )) %>% 
  mutate(winning_id = ifelse(completed == TRUE, 
                               case_when(fav_points - Spread > und_points ~ fav_id,
                                         fav_points - Spread < und_points ~ und_id,
                                         TRUE ~ fav_id), NA), # Need to fix, edge case for Liberty when everyone took Oregon so I didn't populate a team ID for Liberty
         correct = case_when(str_detect(notes, "CFP Semifinal") ~ ifelse(pick_id == winning_id, 3, 0),
                             str_detect(notes, "CFP National Championship") ~ ifelse(pick_id == winning_id, 5, 0),
                             TRUE ~ ifelse(pick_id == winning_id, 1, 0)
                             )
         )
                                 
standings <- results %>%
  group_by(Name) %>%
  summarise(points = sum(correct, na.rm = TRUE)) %>% arrange(desc(points))

saveRDS(standings, file=file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 'standings.rds'))


############################ Formatting picks screen 
webpage_picks <- results %>% 
  select( game,  Date, `Time (EST)`, Name, pick,  Favorite, fav_points, Underdog, und_points, Spread, pick_id, fav_id, und_id, winning_id, completed, correct) %>% 
  left_join(teamlist, by = c("winning_id" = "team_id")) %>% mutate(winner = ifelse(winning_id == 0, "Push", pick.y)) %>% select(-pick.y) %>% 
  rename(Game = game,  Pick = pick.x ,`Und. Score`= und_points ,   `Fav. Score` = fav_points,   Winner = winner) 
 
saveRDS(webpage_picks, file=file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 'results.rds'))


