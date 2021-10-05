library(jsonlite)
library(httr)
library(tidyverse)
library(knitr)


user_ids <- fromJSON("https://api.sleeper.app/v1/league/726076991400394752/users",flatten = TRUE)

rosters_records_points <- fromJSON("https://api.sleeper.app/v1/league/726076991400394752/rosters",flatten = TRUE)


user_ids <- user_ids %>%
  select(user_id,display_name)

str(matchups)

matchups <- fromJSON("https://api.sleeper.app/v1/league/726076991400394752/matchups/3",flatten = TRUE)

matchups$points <- sum(matchups$starters_points) 

sum(matchups$starters_points[[2]])







matchups <- matchups %>%
  mutate(Point2 = sum(starters_points))

match2 <- matchups %>%
  filter(matchup_id == 1) %>%
  arrange()

test <- as.vector(match2$points)

match(max(test),test)

max(test)


match_func <- function(weeks) {
  
  for(i in 1:length(weeks)){
    
    matchups <- fromJSON(paste0("https://api.sleeper.app/v1/league/726076991400394752/matchups/",weeks[i]),flatten = TRUE)
    
    matchups <- matchups %>%
      select(roster_id,points,matchup_id,starters_points)
      
      
    week <- weeks[i]
    
    for(a in 1:6){
      
      df <- matchups %>%
        filter(matchup_id == a)
      
      mid <- df[1,3]
      tm1 <- df[1,1]
      tm2 <- df[2,1]
      point1 <- sum(df$starters_points[[1]])
      point2 <- sum(df$starters_points[[2]])
      
      row_info <- tibble(week,mid,tm1,point1,tm2,point2)
      
      if(a == 1){
        weekdf <- row_info
      }else{
        weekdf <- rbind(weekdf,row_info)
      }
    }
    
    if(i == 1){
      final <- weekdf
    }else{
      
      final <- rbind(final,weekdf)
    }
    
    
    
  }
  
  
  final <- final %>%
    mutate(win_team = if_else(point1 > point2, tm1,tm2),
           lose_team = if_else(tm1 == win_team,tm2,tm1),
           win_point = if_else(point1 > point2, point1,point2),
           lose_point = if_else(point1 > point2, point2,point1))
  
  
  
  
  return(final)
  
}

blue <- match_func(weeks = c(1:4))

str(blue)





blue <- as.vector()


yellow <- fromJSON("https://api.sleeper.app/v1/league/726076991400394752/matchups/1")

