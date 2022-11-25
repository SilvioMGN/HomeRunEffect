# packages needed

library(readr)
library(tidyverse)
library(baseballr)

# name/id data from __

#playeridmap <- read_csv("~/playeridmap.csv")

## Function annual_statcast_query from Bill Petti

annual_statcast_query <- function(season) {
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates, 
                              end_date = dates + 3)
  
  safe_savant <- purrr::safely(baseballr::scrape_statcast_savant)
  
  payload <- purrr::map(.x = seq_along(date_grid$start_date), 
                        ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                          
                          payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                                 end_date = date_grid$end_date[.x], type = 'pitcher')
                          
                          return(payload)
                        })
  
  payload_df <- purrr::map(payload, 'result')
  
  number_rows <- purrr::map_df(.x = seq_along(payload_df), 
                               ~{number_rows <- tibble::tibble(week = .x, 
                                                               number_rows = length(payload_df[[.x]]$game_date))}) %>%
    dplyr::filter(number_rows > 0) %>%
    dplyr::pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  payload_df_reduced_formatted <- purrr::map(.x = seq_along(payload_df_reduced), 
                                             ~{cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                                                                      "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                                                                      "fielder_8", "fielder_9")
                                             
                                             df <- purrr::pluck(payload_df_reduced, .x) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, as.numeric) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, function(x) {
                                                 ifelse(is.na(x), 999999999, x)
                                               })
                                             
                                             character_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "character") %>%
                                               dplyr::pull(variable)
                                             
                                             numeric_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "numeric") %>%
                                               dplyr::pull(variable)
                                             
                                             integer_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "integer") %>%
                                               dplyr::pull(variable)
                                             
                                             df <- df %>%
                                               dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
                                               dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
                                               dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
                                             
                                             return(df)
                                             })
  
  combined <- payload_df_reduced_formatted %>%
    dplyr::bind_rows()
  
  combined
}

## Function format_append_statcast:

format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set
  
  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3, 
                                                 ifelse(type == "X" & events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1, 
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    
    df <- df %>%
      dplyr::mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      dplyr::mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      dplyr::filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    dplyr::arrange(game_date)
  
  df <- df %>%
    dplyr::filter(!is.na(game_date))
  
  df <- df %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::select(setdiff(names(.), c("error")))
  
  return(df)
}

# Pitching data from the specified year 

data <- annual_statcast_query(2019)

pitching2019 <- format_append_statcast(data)

pitching2019 <- arrange(pitching2019, game_date, pitcher, inning, at_bat_number, pitch_number)

# Only Regular Season and Postseason

pitching2019 <- pitching2019 %>%
  filter(game_type != "S")

# Adding a boolean variable for HRs

pitching2019 <- pitching2019 %>% mutate(HR_index = hit_type == 4) 

# Adding a unique index for pitcher and game combination

pitching2019$index <- pitching2019 %>% group_by(game_date, pitcher) %>% group_indices(game_date, pitcher)

pitching2019 <- pitching2019 %>% group_by(game_date, pitcher) %>% mutate(pitch_counter = row_number(game_date))

# create a helper dataset and two datasets for all pitches before and after the first Home Run
helper <- pitching2019[0,]
afterHR <- helper
beforeHR <- helper

count = 0

# iterate over every pitching performances for afterHR 
## This takes a lot of time, so this needs to be improved in the future

for (i in 1:max(pitching2019$index)) {
  
  count = count + 1
  print(count)
  
  helper <- pitching2019[pitching2019$index == i,]
  
  # iterate over all pitches of a pitching performance
  
  for(a in 1:max(helper$pitch_counter)) {
    
    helper2 <- helper[helper$pitch_counter==a,] 
    
    if(helper2$HR_index == FALSE | is.na(helper2$HR_index)){
      
      # if pitch doesn't resolve in a HR then delete the pitch since it is prior to the first HR
      helper <- helper[!(helper$index == i & helper$pitch_counter == a),] 
      
    } 
    
    else {break}
    
    
  }
  
  afterHR <- rbind(afterHR, helper) 
  
}


## generate beforeHR

# Because of the slow speed of rbind, we generate beforeHR by removing all elements of afterHR and allHR from the pitching data

intermediate <- pitching2019 %>% group_by(index, pitch_counter) %>% anti_join(afterHR, pitching2019, by=c("index", "pitch_counter"))


# remove all Home Runs from the afterHR dataset

allHR_2019 <- afterHR %>% filter(HR_index == TRUE)

afterHR_2019 <- afterHR %>% filter(HR_index==FALSE |is.na(HR_index))

beforeHR_2019 <- anti_join(intermediate, allHR_2019, by=c("index", "pitch_counter"))



# calculate Strike Percentage (StrPct), Ball Percentage (BallPct) and the Ball-in-Play Percentage (BipPct) for afterHR and beforeHR

## afterHR

# total

afterHR_2019 <- afterHR_2019 %>% group_by(pitcher) %>% mutate(totalStrPct = sum(type=="S")/(sum(type=="S") + sum(type=="X") + sum(type=="B")))

afterHR_2019 <- afterHR_2019 %>% group_by(pitcher) %>% mutate(totalBallPct = sum(type=="B")/(sum(type=="S") + sum(type=="X") + sum(type=="B")))

afterHR_2019 <- afterHR_2019 %>% group_by(pitcher) %>% mutate(totalBIPPct = 1 - (totalStrPct + totalBallPct))


## beforeHR

# total

beforeHR_2019 <- beforeHR_2019 %>% group_by(pitcher) %>% mutate(totalStrPct = sum(type=="S")/(sum(type=="S") + sum(type=="X") + sum(type=="B")))

beforeHR_2019 <- beforeHR_2019 %>% group_by(pitcher) %>% mutate(totalBallPct = sum(type=="B")/(sum(type=="S") + sum(type=="X") + sum(type=="B")))

beforeHR_2019 <- beforeHR_2019 %>% group_by(pitcher) %>% mutate(totalBIPPct = 1 - (totalStrPct + totalBallPct))

## Season total

pitching2019 <- pitching2019 %>% group_by(pitcher) %>% mutate(totalStrPct = sum(type=="S")/(sum(type=="S") + sum(type=="X") + sum(type=="B")))

pitching2019 <- pitching2019 %>% group_by(pitcher) %>% mutate(totalBallPct = sum(type=="B")/(sum(type=="S") + sum(type=="X") + sum(type=="B")))

pitching2019 <- pitching2019 %>% group_by(pitcher) %>% mutate(totalBIPPct = 1 - (totalStrPct + totalBallPct))


# remove columns that we do not need

afterHR_2019 <- afterHR_2019 %>% select(pitcher, p_throws, totalStrPct, totalBallPct, totalBIPPct)

beforeHR_2019 <- beforeHR_2019 %>% select(pitcher, p_throws, totalStrPct, totalBallPct, totalBIPPct)


# because we are only interested in the total numbers of StrPct, etc., we can use only one row per player

beforeHR_2019 <- beforeHR_2019 %>% distinct(pitcher, .keep_all = TRUE)

afterHR_2019 <- afterHR_2019 %>% distinct(pitcher, .keep_all = TRUE)


# create new csv files

write.csv(afterHR_2019, 'afterHR_2019.csv')
write.csv(beforeHR_2019, 'beforeHR_2019.csv')
write.csv(allHR_2019, 'allHR_2019.csv')


# combine afterHR and beforeHR

combined <- merge(x=beforeHR_2019, y=afterHR_2019, by="pitcher", all.y = TRUE)

combined$p_throws.y <- NULL
names(combined)[6]  <- "totalStrPct_after"
names(combined)[7]  <- "totalBallPct_after"
names(combined)[8]  <- "totalBIPPct_after"
names(combined)[3]  <- "totalStrPct_before"
names(combined)[4]  <- "totalBallPct_before"
names(combined)[5]  <- "totalBIPPct_before"


# three new columns that calculate the difference between Balls in Play (BIP), Strikes (Str) and Balls percentages before and after first Home Run

combined$difference_totalBIP <- (combined$totalBIPPct_after - combined$totalBIPPct_before)

combined$difference_totalStr <- (combined$totalStrPct_after - combined$totalStrPct_before)

combined$difference_totalBall <- (combined$totalBallPct_after - combined$totalBallPct_before)

# write a csv with combined data

write.csv(combined, 'combined_2019.csv')

# add player names 

combined <- merge(x = combined, y = playeridmap, by.x = "pitcher", by.y = "MLBID", all.x = TRUE)



# pitching data from qualified pitchers 

fangraphsPitching2019_qualified <- read_csv("~/fangraphsPitching2019_qualified.csv")

combined <- merge(x = combined, y = fangraphsPitching2019, by.x = "IDFANGRAPHS", by.y = "playerid", all.y = TRUE)


# new csv file that contains the qualified pitchers and their StrPct, BallPct, BIPPct difference

write.csv(combined, 'differenceAfterHR_2019_qualified')
