## Load Important Libraries ## 
library(tidyverse)

## Set Working Directory ##
setwd('/Users/kirankrishnamurthi/OneDrive - USC Marshall School of Business/Spring 2022/DSO 510/Team Project/DSO-510-Project-1-Tennis-Match-Length-Analysis')

## Load in Data ## 
atp_2019 = read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2019.csv')

## What does our data look like? ## 
glimpse(atp_2019)

## Create a working data dataframe ## 
wdata = atp_2019

## Get a sense of our 'response' variable being Minutes ## 
summary(wdata$minutes)

## Looking at rows where the minute value is NA ## 
wdata %>% filter(is.na(minutes) == TRUE) %>% view()

#### Remove all rows from wdata where the minutes values are NA ####
wdata = wdata %>% filter(is.na(minutes) == FALSE)
wdata %>% select(minutes) %>% summary()

#### Changing surface to a factor variable #### 
wdata %>% select(surface) %>% table() 

wdata %>% mutate(surface = factor(surface)) %>% select(surface) %>% 
  glimpse()

wdata = wdata %>% mutate(surface = factor(surface))

#### Remove all rows where the player has no ATP ranking or ATP points #### 
wdata %>% select(contains('name') | contains('rank')) %>% 
  filter(is.na(winner_rank) == TRUE | is.na(winner_rank_points) == TRUE | is.na(loser_rank) == TRUE | is.na(loser_rank_points) == TRUE)

wdata = wdata %>% filter(is.na(winner_rank) == FALSE & is.na(winner_rank_points) == FALSE & is.na(loser_rank) == FALSE & is.na(loser_rank_points) == FALSE) 

## Creating a "Ranking Distance' and 'Point Distance' column to show opponent balance ## 
wdata %>% mutate(ranking_distance = loser_rank - winner_rank,
                 point_distance = winner_rank_points - loser_rank_points) %>% 
  select(contains('rank') | contains('distance') | minutes)

wdata = wdata %>% mutate(ranking_distance = loser_rank - winner_rank,
                         point_distance = winner_rank_points - loser_rank_points)

## Convert draw_size into a factor variable ## 
wdata %>% select(draw_size) %>% table()

wdata %>% filter(draw_size == 4 | draw_size == 8)

wdata = wdata %>% mutate(draw_size = factor(draw_size))

## Convert round to an ordered factor where the final is the most important ## 
wdata %>% select(round) %>% table()

wdata = wdata %>% mutate(round = factor(round, 
                                        ordered = TRUE, 
                                        levels = c('F', 'SF', 'QF', 'R16', 'R32', 'R64', 'R128', 'RR'))) 

## Convert tourney_level to an ordered factor where grand slams have the most importance ## 
wdata %>% select(tourney_level) %>% table()


wdata = wdata %>% mutate(tourney_level = factor(tourney_level, 
                                                ordered = TRUE, 
                                                levels = c('G', 'F', 'M', 'A', 'D'))) 

## Create a dataframe without Davis Cup Data ## 
wdata_nd = wdata %>% filter(tourney_level != 'D')

## Generate values for NA's in winner and loser entry column ## 
wdata_nd %>% select(winner_entry) %>% table()
wdata_nd %>% select(loser_entry) %>% table()

wdata_nd[which(is.na(wdata_nd$winner_entry) == TRUE),'winner_entry'] 

wdata_nd[which(is.na(wdata_nd$winner_entry) == TRUE),'winner_entry'] = 'MD'

wdata_nd[which(is.na(wdata_nd$loser_entry) == TRUE),'loser_entry']

wdata_nd[which(is.na(wdata_nd$loser_entry) == TRUE),'loser_entry'] = 'MD'

wdata_nd %>% select(winner_entry) %>% table()
wdata_nd %>% select(loser_entry) %>% table()

## Convert Tournament Date to a Date Time Object ## 
wdata_nd$tourney_date = ymd(wdata_nd$tourney_date)

glimpse(wdata_nd$tourney_date)

## Parsing the score column into multiple columns ## 
wdata_nd %>% separate(score, into = c('first_set', 'second_set', 'third_set', 'fourth_set', 'fifth_set'),
                      sep = " ") %>% select(contains('set'))

wdata_nd = wdata_nd %>% separate(score, into = c('first_set', 'second_set', 'third_set', 'fourth_set', 'fifth_set'),
                                 sep = " ")

wdata_nd %>% select(contains('set'))


#### Extracting Games from Sets #### 

# First Set
wdata_nd$first_sg1 = rep(NA,length(wdata_nd$first_set))
for (i in 1:length(wdata_nd$first_set)){
  wdata_nd$first_sg1[i] = str_split_fixed(wdata_nd$first_set, "-", n = 2)[i,1]
}

wdata_nd$first_sg2 = rep(NA,length(wdata_nd$first_set))
for (i in 1:length(wdata_nd$first_set)){
  wdata_nd$first_sg2[i] = str_split_fixed(wdata_nd$first_set, "-", n = 2)[i,2]
}

# Second Set
wdata_nd$second_sg1 = rep(NA,nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)){
  wdata_nd$second_sg1[i] = str_split_fixed(wdata_nd$second_set, "-", n = 2)[i,1]
}

wdata_nd$second_sg2 = rep(NA,nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)){
  wdata_nd$second_sg2[i] = str_split_fixed(wdata_nd$second_set, "-", n = 2)[i,2]
}

# Third Set
wdata_nd$third_sg1 = rep(NA,nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)){
  wdata_nd$third_sg1[i] = str_split_fixed(wdata_nd$third_set, "-", n = 2)[i,1]
}

wdata_nd$third_sg2 = rep(NA,nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)){
  wdata_nd$third_sg2[i] = str_split_fixed(wdata_nd$third_set, "-", n = 2)[i,2]
}

# Fourth Set
wdata_nd$fourth_sg1 = rep(NA,nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)){
  wdata_nd$fourth_sg1[i] = str_split_fixed(wdata_nd$fourth_set, "-", n = 2)[i,1]
}

wdata_nd$fourth_sg2 = rep(NA,nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)){
  wdata_nd$fourth_sg2[i] = str_split_fixed(wdata_nd$fourth_set, "-", n = 2)[i,2]
}

# Fifth Set
wdata_nd$fifth_sg1 = rep(NA,nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)){
  wdata_nd$fifth_sg1[i] = str_split_fixed(wdata_nd$fifth_set, "-", n = 2)[i,1]
}

wdata_nd$fifth_sg2 = rep(NA,nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)){
  wdata_nd$fifth_sg2[i] = str_split_fixed(wdata_nd$fifth_set, "-", n = 2)[i,2]
}

## Determine if any tie break scores are held in the 1st of the set games columns ##

# First Set
str_extract(wdata_nd$first_sg1, '\\(\\d\\)') %>% is.na() %>% table()
# Second Set
str_extract(wdata_nd$second_sg1, '\\(\\d\\)') %>% is.na() %>% table()
# Third Set
str_extract(wdata_nd$third_sg1, '\\(\\d\\)') %>% is.na() %>% table()
# Fourth Set
str_extract(wdata_nd$fourth_sg1, '\\(\\d\\)') %>% is.na() %>% table()
# Fifth Set
str_extract(wdata_nd$fifth_sg1, '\\(\\d\\)') %>% is.na() %>% table()

## Determine if any tie break scores are held in the 2nd of the set games columns ##

# First Set
str_extract(wdata_nd$first_sg2, '\\(\\d\\)') %>% is.na() %>% table()
# Second Set
str_extract(wdata_nd$second_sg2, '\\(\\d\\)') %>% is.na() %>% table()
# Third Set
str_extract(wdata_nd$third_sg2, '\\(\\d\\)') %>% is.na() %>% table()
# Fourth Set
str_extract(wdata_nd$fourth_sg2, '\\(\\d\\)') %>% is.na() %>% table()
# Fifth Set
str_extract(wdata_nd$fifth_sg2, '\\(\\d\\)') %>% is.na() %>% table()

#### Extract all tiebreak scores from first set games column 2 into new column #### 

# First Set
wdata_nd$first_sts = str_extract(wdata_nd$first_sg2, '\\(\\d\\)')
# Second Set
wdata_nd$second_sts = str_extract(wdata_nd$second_sg2, '\\(\\d\\)')
# Third Set
wdata_nd$third_sts = str_extract(wdata_nd$third_sg2, '\\(\\d\\)')
# Fourth Set
wdata_nd$fourth_sts = str_extract(wdata_nd$fourth_sg2, '\\(\\d\\)')
# Fifth Set
wdata_nd$fifth_sts = str_extract(wdata_nd$fifth_sg2, '\\(\\d\\)')

## View All Set Tiebreaker Score Columns ## 

wdata_nd %>% select(contains('sts'))

## Dropping All () in the Set Tiebreaker Score Columns ##

# First Set
wdata_nd$first_sts = str_remove(wdata_nd$first_sts, '\\(')
wdata_nd$first_sts = str_remove(wdata_nd$first_sts, '\\)')

# Second Set
wdata_nd$second_sts = str_remove(wdata_nd$second_sts, '\\(')
wdata_nd$second_sts = str_remove(wdata_nd$second_sts, '\\)')

# Third Set
wdata_nd$third_sts = str_remove(wdata_nd$third_sts, '\\(')
wdata_nd$third_sts = str_remove(wdata_nd$third_sts, '\\)')

# Fourth Set
wdata_nd$fourth_sts = str_remove(wdata_nd$fourth_sts, '\\(')
wdata_nd$fourth_sts = str_remove(wdata_nd$fourth_sts, '\\)')

# Fifth Set
wdata_nd$fifth_sts = str_remove(wdata_nd$fifth_sts, '\\(')
wdata_nd$fifth_sts = str_remove(wdata_nd$fifth_sts, '\\)')

## Convert Set Tiebreaker Score Columns to Numeric ##

wdata_nd$first_sts = as.numeric(wdata_nd$first_sts)
wdata_nd$second_sts = as.numeric(wdata_nd$second_sts)
wdata_nd$third_sts = as.numeric(wdata_nd$third_sts)
wdata_nd$fourth_sts = as.numeric(wdata_nd$fourth_sts)
wdata_nd$fifth_sts = as.numeric(wdata_nd$fifth_sts)

## View All Set Tiebreaker Score Columns ## 

wdata_nd %>% select(contains('sts'))

#### Get total points played in the tie breaker #### 

# First Set
wdata_nd$first_sttp = rep(NA, nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)[1]){
  wdata_nd$first_sttp[i] = ifelse(wdata_nd$first_sts[i] >= 6, (wdata_nd$first_sts[i]+2) + wdata_nd$first_sts[i], 
                                  wdata_nd$first_sts[i] + 7)
}

# Second Set
wdata_nd$second_sttp = rep(NA, nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)[1]){
  wdata_nd$second_sttp[i] = ifelse(wdata_nd$second_sts[i] >= 6, (wdata_nd$second_sts[i]+2) + wdata_nd$second_sts[i], 
                                   wdata_nd$second_sts[i] + 7)
}

# Third Set
wdata_nd$third_sttp = rep(NA, nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)[1]){
  wdata_nd$third_sttp[i] = ifelse(wdata_nd$third_sts[i] >= 6, (wdata_nd$third_sts[i]+2) + wdata_nd$third_sts[i], 
                                  wdata_nd$third_sts[i] + 7)
}

# Fourth Set
wdata_nd$fourth_sttp = rep(NA, nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)[1]){
  wdata_nd$fourth_sttp[i] = ifelse(wdata_nd$fourth_sts[i] >= 6, (wdata_nd$fourth_sts[i]+2) + wdata_nd$fourth_sts[i], 
                                   wdata_nd$fourth_sts[i] + 7)
}

# Fifth Set
wdata_nd$fifth_sttp = rep(NA, nrow(wdata_nd))
for (i in 1:nrow(wdata_nd)[1]){
  wdata_nd$fifth_sttp[i] = ifelse(wdata_nd$fifth_sts[i] >= 6, (wdata_nd$fifth_sts[i]+2) + wdata_nd$fifth_sts[i], 
                                  wdata_nd$fifth_sts[i] + 7)
}

## Diagnostic Check to See if Total Tiebreaker Points is Correct ##
wdata_nd %>% select(contains('name') | contains('sg') & contains('fifth')) %>% 
  filter(is.na(fifth_sg1) == FALSE) %>% arrange(desc(fifth_sg1))


## Understanding any non-numeric values in the Set Games Columns ## 
pivot_longer(wdata_nd, cols = contains('sg'), 
             names_to = 'set_number',
             values_to = 'set_games',
             values_drop_na = FALSE) %>% select(contains('name') | contains('set')) %>%
  filter(set_games == 'RET')

#### Cleaning first_sg2 to remove all tiebreaker values #### 
wdata_nd$first_sg2 = str_sub(wdata_nd$first_sg2, start = 1, end = 1)
wdata_nd$second_sg2 = str_sub(wdata_nd$second_sg2, start = 1, end = 1)
wdata_nd$third_sg2 = str_sub(wdata_nd$third_sg2, start = 1, end = 1)
wdata_nd$fourth_sg2 = str_sub(wdata_nd$fourth_sg2, start = 1, end = 1)
wdata_nd$fifth_sg2 = str_sub(wdata_nd$fifth_sg2, start = 1, end = 1)

## Filter Data to See Only Rows Where 'sg' columns have RET values ## 
wdata_nd  %>% filter_at(vars(contains('sg')), any_vars(str_detect(., pattern = "RET")))

#### Remove All Rows with RET #### 
wdata_nd %>% filter(first_sg1 != 'RET' &
                      second_sg1 != 'RET' &
                      third_sg1 != 'RET' &
                      fourth_sg1 != 'RET' &
                      fifth_sg1 != 'RET' &
                      first_sg2 != 'RET' &
                      second_sg2 != 'RET' &
                      third_sg2 != 'RET' &
                      fourth_sg2 != 'RET' &
                      fifth_sg2 != 'RET') %>% 
  pivot_longer(cols = contains('sg'), 
               names_to = 'set_number',
               values_to = 'set_games',
               values_drop_na = FALSE) %>% select(contains('name') | contains('set')) %>% 
  select(set_games) %>% table()

wdata_nd = wdata_nd %>% filter(first_sg1 != 'RET' &
                                 second_sg1 != 'RET' &
                                 third_sg1 != 'RET' &
                                 fourth_sg1 != 'RET' &
                                 fifth_sg1 != 'RET' &
                                 first_sg2 != 'RET' &
                                 second_sg2 != 'RET' &
                                 third_sg2 != 'RET' &
                                 fourth_sg2 != 'RET' &
                                 fifth_sg2 != 'RET')

#### Converting All Set Games to Numeric ####

wdata_nd %>% mutate(first_sg1 = as.numeric(first_sg1),
                    second_sg1 = as.numeric(second_sg1),
                    third_sg1 = as.numeric(third_sg1),
                    fourth_sg1 = as.numeric(fourth_sg1),
                    fifth_sg1 = as.numeric(fifth_sg1),
                    first_sg2 = as.numeric(first_sg2),
                    second_sg2 = as.numeric(second_sg2),
                    third_sg2 = as.numeric(third_sg2),
                    fourth_sg2 = as.numeric(fourth_sg2),
                    fifth_sg2 = as.numeric(fifth_sg2)) %>% select(contains('sg')) %>% glimpse()


wdata_nd = wdata_nd %>% mutate(first_sg1 = as.numeric(first_sg1),
                               second_sg1 = as.numeric(second_sg1),
                               third_sg1 = as.numeric(third_sg1),
                               fourth_sg1 = as.numeric(fourth_sg1),
                               fifth_sg1 = as.numeric(fifth_sg1),
                               first_sg2 = as.numeric(first_sg2),
                               second_sg2 = as.numeric(second_sg2),
                               third_sg2 = as.numeric(third_sg2),
                               fourth_sg2 = as.numeric(fourth_sg2),
                               fifth_sg2 = as.numeric(fifth_sg2))



#### Creating a Total Set Games Column and Removing Individual Set Games #### 

wdata_nd %>% mutate(first_sg = first_sg1 + first_sg2,
                    second_sg = second_sg1 + second_sg2,
                    third_sg = third_sg1 + third_sg2, 
                    fourth_sg = fourth_sg1 + fourth_sg2,
                    fifth_sg = fifth_sg1 + fifth_sg2) %>% select(-contains('sg1') & -contains('sg2'))

wdata_nd = wdata_nd %>% mutate(first_sg = first_sg1 + first_sg2,
                               second_sg = second_sg1 + second_sg2,
                               third_sg = third_sg1 + third_sg2, 
                               fourth_sg = fourth_sg1 + fourth_sg2,
                               fifth_sg = fifth_sg1 + fifth_sg2)

#### Creating a Column for Total Sets Played ####
a = ifelse(is.na(wdata_nd$first_sg[1]), 0,1) + 
  ifelse(is.na(wdata_nd$second_sg[1]), 0,1) + 
  ifelse(is.na(wdata_nd$third_sg[1]), 0,1) + 
  ifelse(is.na(wdata_nd$fourth_sg[1]), 0,1) + 
  ifelse(is.na(wdata_nd$fifth_sg[1]), 0,1)


wdata_nd %>% select(contains('sg'))

wdata_nd$sets_played = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$sets_played[i] = ifelse(is.na(wdata_nd$first_sg[i]), 0,1) + 
    ifelse(is.na(wdata_nd$second_sg[i]), 0,1) + 
    ifelse(is.na(wdata_nd$third_sg[i]), 0,1) + 
    ifelse(is.na(wdata_nd$fourth_sg[i]), 0,1) + 
    ifelse(is.na(wdata_nd$fifth_sg[i]), 0,1)
}
end = Sys.time()
end - start

wdata_nd %>% select(contains('sg') | contains('sets'))
wdata_nd %>% select(sets_played, minutes)

#### Creating a column for games played #### 
a = ifelse(is.na(wdata_nd$first_sg[1]), yes = 0, no = wdata_nd$first_sg[1]) + 
  ifelse(is.na(wdata_nd$second_sg[1]), yes = 0, no = wdata_nd$second_sg[1]) + 
  ifelse(is.na(wdata_nd$third_sg[1]), yes = 0, no = wdata_nd$third_sg[1]) + 
  ifelse(is.na(wdata_nd$fourth_sg[1]), yes = 0, no = wdata_nd$fourth_sg[1]) + 
  ifelse(is.na(wdata_nd$fifth_sg[1]), yes = 0, no = wdata_nd$fifth_sg[1])

wdata_nd %>% select(contains('sg'))
a

wdata_nd$games_played = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$games_played[i] = ifelse(is.na(wdata_nd$first_sg[i]), yes = 0, no = wdata_nd$first_sg[i]) + 
    ifelse(is.na(wdata_nd$second_sg[i]), yes = 0, no = wdata_nd$second_sg[i]) + 
    ifelse(is.na(wdata_nd$third_sg[i]), yes = 0, no = wdata_nd$third_sg[i]) + 
    ifelse(is.na(wdata_nd$fourth_sg[i]), yes = 0, no = wdata_nd$fourth_sg[i]) + 
    ifelse(is.na(wdata_nd$fifth_sg[i]), yes = 0, no = wdata_nd$fifth_sg[i])
}
end = Sys.time()
end - start

wdata_nd %>% select(contains('sg') | contains('games'))
wdata_nd %>% select(sets_played, games_played, minutes)

#### Creating Two New Columns for Average Set Length and Average Game Length ####
wdata_nd %>% mutate(average_set_length = minutes / sets_played,
                    average_game_length = minutes / games_played) %>% 
  select(contains('sg') | contains('games') | contains('length'))

wdata_nd = wdata_nd %>% mutate(average_set_length = minutes / sets_played,
                               average_game_length = minutes / games_played)

#### Creating an Ace Gap Interaction Variable #### 
wdata_nd %>% mutate(ace_gap = w_ace - l_ace) 
wdata_nd = wdata_nd %>% mutate(ace_gap = w_ace - l_ace) 

#### Creating an Ace Interaction Variable with 1st and 2nd Serve Points Won ####
wdata_nd %>% mutate(w_ace_win_ratio = w_ace / (w_1stWon + w_2ndWon),
                    w_ace_serve_ratio = w_ace / (w_svpt),
                    l_ace_win_ratio = l_ace / (l_1stWon + l_2ndWon),
                    l_ace_serve_ratio = l_ace / (l_svpt)) %>% 
  select(contains('ace') & contains('ratio')) %>% summary()

wdata_nd = wdata_nd %>% mutate(w_ace_win_ratio = w_ace / (w_1stWon + w_2ndWon),
                               w_ace_serve_ratio = w_ace / (w_svpt),
                               l_ace_win_ratio = l_ace / (l_1stWon + l_2ndWon),
                               l_ace_serve_ratio = l_ace / (l_svpt))

wdata_nd %>% ggplot() + 
  geom_point(aes(x = w_ace_win_ratio*100, y = average_set_length, color = best_of)) 
summary(lm(average_set_length ~ w_ace_win_ratio, data = wdata_nd))

#### Creating a Break Points Faced Interaction Variable ####
wdata_nd %>% select(contains('bp'))

wdata_nd %>% mutate(w_bpRatio = w_bpFaced / ((w_bpFaced - w_bpSaved) + 1),
                    l_bpRatio = l_bpFaced / ((l_bpFaced - l_bpSaved) + 1)) %>% 
  select(contains('bp') | contains('minutes')) %>% ggplot() + 
  geom_point(aes(x = w_bpFaced, y = w_bpRatio, color = minutes))

wdata_nd = wdata_nd %>% mutate(w_bpRatio = w_bpFaced / ((w_bpFaced - w_bpSaved) + 1),
                               l_bpRatio = l_bpFaced / ((l_bpFaced - l_bpSaved) + 1))


#### Creating a Serve Points to Double Fault Ratio ####
wdata_nd %>% select(w_svpt) %>% table()
wdata_nd %>% select(w_df) %>% table()

wdata_nd %>% mutate(w_svRatio = w_svpt / (w_svpt - w_df)) %>% 
  select(w_svRatio) %>% summary()

wdata_nd = wdata_nd %>% mutate(w_svRatio = w_svpt / (w_svpt - w_df)) 
wdata_nd = wdata_nd %>% mutate(l_svRatio = l_svpt / (l_svpt - l_df))

#### Creating a First Serve Points Won Ratio ####

wdata_nd %>% mutate(w_1stWinRatio = (w_1stIn / w_1stWon)*100,
                    l_1stWinRatio = (l_1stIn / l_1stWon)*100) %>% 
  select(w_1stWinRatio, l_1stWinRatio) %>% summary()

wdata_nd = wdata_nd %>% mutate(w_1stWinRatio = (w_1stIn / w_1stWon)*100,
                               l_1stWinRatio = (l_1stIn / l_1stWon)*100)

#### Creating a Variable for Total Height #### 
wdata_nd %>% mutate(total_height = winner_ht + loser_ht) %>% select(total_height) %>% 
  summary()

wdata_nd = wdata_nd %>% mutate(total_height = winner_ht + loser_ht)

#### Creating a Serve Games / Total Games Ratio Column ####
wdata_nd %>% mutate(w_SvGms_ratio = log(w_SvGms * games_played)) %>% 
  ggplot() + 
  geom_histogram(aes(x = w_SvGms_ratio), fill = 5, color = 'black', bins = 20)

wdata_nd %>% mutate(w_SvGms_ratio = log10(w_SvGms * games_played)) %>% 
  ggplot() + 
  geom_point(aes(x = w_SvGms_ratio, y = average_set_length, color = total_height))

wdata_nd = wdata_nd %>% mutate(w_SvGms_TotGms = w_SvGms * games_played,
                  w_SvGms_TotGms_Log = log10(w_SvGms * games_played),
                  l_SvGms_TotGms = l_SvGms * games_played,
                  l_SvGms_TotGms_Log = log10(l_SvGms * games_played))

## Creating a Column for Winner and Loser Age Gap ## 
wdata_nd %>% mutate(age_gap = winner_age - loser_age) %>% select(age_gap) %>% 
  ggplot() + geom_histogram(aes(x = age_gap), fill = 5, color = 'black', bins = 20)

wdata_nd = wdata_nd %>% mutate(age_gap = winner_age - loser_age)

## Creating a Hand Combinations Factor Variable ##
wdata_nd %>% filter(winner_hand != 'U' & loser_hand != 'U') %>% select(contains('hand')) %>% 
  table()

wdata_nd %>% filter(winner_hand != 'U' & loser_hand != 'U') %>% 
  mutate(hand_combo = str_c(winner_hand, loser_hand, sep = "")) %>% 
  select(hand_combo) %>% table()

wdata_nd = wdata_nd %>% filter(winner_hand != 'U' & loser_hand != 'U') %>% 
  mutate(hand_combo = str_c(winner_hand, loser_hand, sep = ""))

ggplot(data = wdata_nd) + 
  geom_boxplot(mapping = aes(x = hand_combo, y = average_set_length, color = hand_combo),
               outlier.colour = 'red') + 
  scale_color_viridis_d()

#### Creating Columns for Match Competitiveness ####

# First Set
wdata_nd$first_set_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$first_set_comp[i] = ifelse(wdata_nd$first_sg1[i] > wdata_nd$first_sg2[i],
                                      wdata_nd$first_sg2[i] / wdata_nd$first_sg1[i],
                                      wdata_nd$first_sg1[i] / wdata_nd$first_sg2[i])
}
end = Sys.time()
end - start 

summary(wdata_nd$first_set_comp)

# Second Set
wdata_nd$second_set_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$second_set_comp[i] = ifelse(wdata_nd$second_sg1[i] > wdata_nd$second_sg2[i],
                                       wdata_nd$second_sg2[i] / wdata_nd$second_sg1[i],
                                       wdata_nd$second_sg1[i] / wdata_nd$second_sg2[i])
}
end = Sys.time()
end - start 

summary(wdata_nd$second_set_comp)

# Third Set
wdata_nd$third_set_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$third_set_comp[i] = ifelse(wdata_nd$third_sg1[i] > wdata_nd$third_sg2[i],
                                      wdata_nd$third_sg2[i] / wdata_nd$third_sg1[i],
                                      wdata_nd$third_sg1[i] / wdata_nd$third_sg2[i])
}
end = Sys.time()
end - start 

summary(wdata_nd$third_set_comp)

# Fourth Set
wdata_nd$fourth_set_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$fourth_set_comp[i] = ifelse(wdata_nd$fourth_sg1[i] > wdata_nd$fourth_sg2[i],
                                       wdata_nd$fourth_sg2[i] / wdata_nd$fourth_sg1[i],
                                       wdata_nd$fourth_sg1[i] / wdata_nd$fourth_sg2[i])
}
end = Sys.time()
end - start 

summary(wdata_nd$fourth_set_comp)

# Fifth Set
wdata_nd$fifth_set_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$fifth_set_comp[i] = ifelse(wdata_nd$fifth_sg1[i] > wdata_nd$fifth_sg2[i],
                                      wdata_nd$fifth_sg2[i] / wdata_nd$fifth_sg1[i],
                                      wdata_nd$fifth_sg1[i] / wdata_nd$fifth_sg2[i])
}
end = Sys.time()
end - start 

summary(wdata_nd$fifth_set_comp)



#### Creating a Column for Total and Standardized Match Competitiveness ####
wdata_nd$match_comp_tot = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$match_comp_tot[i] = 
    ifelse(is.na(wdata_nd$first_set_comp[i]), 0, wdata_nd$first_set_comp[i]) + 
    ifelse(is.na(wdata_nd$second_set_comp[i]), 0, wdata_nd$second_set_comp[i]) + 
    ifelse(is.na(wdata_nd$third_set_comp[i]), 0, wdata_nd$third_set_comp[i]) + 
    ifelse(is.na(wdata_nd$fourth_set_comp[i]), 0, wdata_nd$fourth_set_comp[i]) + 
    ifelse(is.na(wdata_nd$fifth_set_comp[i]), 0, wdata_nd$fifth_set_comp[i])
}
end = Sys.time()
end - start
summary(wdata_nd$match_comp_tot)

wdata_nd %>% mutate(match_comp_standardized = match_comp_tot / sets_played) %>% 
  select(match_comp_standardized) %>% summary()

wdata_nd = wdata_nd %>% mutate(match_comp_standardized = match_comp_tot / sets_played)         

#### Creating Columns for Tie Breaker Competitiveness ####

# First Set
wdata_nd$first_tiebreaker_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$first_tiebreaker_comp[i] = wdata_nd$first_sts[i] / (wdata_nd$first_sttp[i] - wdata_nd$first_sts[i])
}
end = Sys.time()
end - start
summary(wdata_nd$first_tiebreaker_comp)

# Second Set
wdata_nd$second_tiebreaker_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$second_tiebreaker_comp[i] = wdata_nd$second_sts[i] / (wdata_nd$second_sttp[i] - wdata_nd$second_sts[i])
}
end = Sys.time()
end - start
summary(wdata_nd$second_tiebreaker_comp)

# Third Set
wdata_nd$third_tiebreaker_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$third_tiebreaker_comp[i] = wdata_nd$third_sts[i] / (wdata_nd$third_sttp[i] - wdata_nd$third_sts[i])
}
end = Sys.time()
end - start
summary(wdata_nd$third_tiebreaker_comp)

# Fourth Set
wdata_nd$fourth_tiebreaker_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$fourth_tiebreaker_comp[i] = wdata_nd$fourth_sts[i] / (wdata_nd$fourth_sttp[i] - wdata_nd$fourth_sts[i])
}
end = Sys.time()
end - start
summary(wdata_nd$fourth_tiebreaker_comp)

# Fifth Set
wdata_nd$fifth_tiebreaker_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$fifth_tiebreaker_comp[i] = wdata_nd$fifth_sts[i] / (wdata_nd$fifth_sttp[i] - wdata_nd$fifth_sts[i])
}
end = Sys.time()
end - start
summary(wdata_nd$fifth_tiebreaker_comp)



#### Creating a Column for Total and Standardized Tie Breaker Competitiveness ####
wdata_nd$total_tiebreaker_comp = rep(NA, nrow(wdata_nd))
start = Sys.time()
for (i in 1:nrow(wdata_nd)){
  wdata_nd$total_tiebreaker_comp[i] = 
    ifelse(is.na(wdata_nd$first_tiebreaker_comp[i]), 0, wdata_nd$first_tiebreaker_comp[i]) + 
    ifelse(is.na(wdata_nd$second_tiebreaker_comp[i]), 0, wdata_nd$second_tiebreaker_comp[i]) + 
    ifelse(is.na(wdata_nd$third_tiebreaker_comp[i]), 0, wdata_nd$third_tiebreaker_comp[i]) + 
    ifelse(is.na(wdata_nd$fourth_tiebreaker_comp[i]), 0, wdata_nd$fourth_tiebreaker_comp[i]) + 
    ifelse(is.na(wdata_nd$fifth_tiebreaker_comp[i]), 0, wdata_nd$fifth_tiebreaker_comp[i])
}
end = Sys.time()
end - start
summary(wdata_nd$total_tiebreaker_comp)

wdata_nd %>% mutate(standard_tiebreaker_comp = total_tiebreaker_comp / sets_played) %>% 
  select(standard_tiebreaker_comp) %>% summary()

wdata_nd = wdata_nd %>% mutate(standard_tiebreaker_comp = total_tiebreaker_comp / sets_played)     

#### Creating Absolute Value Columns for Ranking and Point Distance ####
wdata_nd %>% mutate(abs_ranking_distance = abs(ranking_distance),
                    abs_point_distance = abs(point_distance)) %>% 
  select(contains('abs')) %>% summary()

wdata_nd = wdata_nd %>% mutate(abs_ranking_distance = abs(ranking_distance),
                               abs_point_distance = abs(point_distance))

### Get Final Column Names ####
print(colnames(wdata_nd))

## Saving Final Data to a CSV File ## 
write_csv(wdata_nd, file = 'atp_2019_cleaned_data.csv')



