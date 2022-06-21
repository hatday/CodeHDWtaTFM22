setwd("C:/Users/hatem/Desktop/data2")


# Analysis ----------------------------------------------------------------
library(tidyverse)
library(skimr)
#### Filtering all data before 2000 and keeping Grand slams
df <- read_csv('wta_matches_all.csv') %>%
  filter(tourney_level=='G') %>% 
  mutate(tournament_date = as.Date(as.character(tourney_date),"%Y%m%d"),
         year = lubridate::year(tournament_date)) %>%
  filter(year>2000)

colnames(df)


### Experience and performance
df_win_experience <- df %>% 
  mutate(round_factor = factor(round,levels = c("R128","R64","R32","R16",'Q1','Q2','Q3',"QF",'SF','F'))) %>%
  arrange(winner_id,tourney_id,tournament_date) %>% 
  mutate(year = lubridate::year(tournament_date)) %>%
  group_by(winner_id) %>%
  mutate(min_year = min(year)) %>%
  mutate(experience = year - min_year) %>%
  ungroup() %>%
  group_by(tourney_id,winner_id) %>%
  arrange(tournament_date) %>%
  mutate(max_round = last(round_factor)) %>%
  dplyr::select(winner_id,tourney_id,tournament_date,year,min_year,experience,max_round)


df_win_experience 


df_lost_experience <- df %>% 
  mutate(round_factor = factor(round,levels = c("R128","R64","R32","R16",'Q1','Q2','Q3',"QF",'SF','F'))) %>%
  arrange(loser_id,tourney_id,tournament_date) %>% 
  mutate(year = lubridate::year(tournament_date)) %>%
  group_by(loser_id) %>%
  mutate(min_year = min(year)) %>%
  mutate(experience = year - min_year) %>%
  ungroup() %>%
  group_by(tourney_id,loser_id) %>%
  arrange(tournament_date) %>%
  mutate(max_round = last(round_factor)) %>%
  dplyr::select(loser_id,tourney_id,tournament_date,year,min_year,experience,max_round)



#### Last three tournament record vs current tournament record

df_win_experience_last3 <- df_win_experience %>%
  group_by(winner_id,tourney_id) %>%
  dplyr::summarise(date = max(tournament_date),
                   result = last(max_round)) %>%
  ungroup() %>%
  group_by(winner_id) %>%
  arrange(date) %>%
  mutate(last_result = lag(result),
         last_2_result = lag(result,2),
         last_3_result = lag(result,3))

df_lost_experience_last3 <- df_lost_experience %>%
  group_by(loser_id,tourney_id) %>%
  dplyr::summarise(date = max(tournament_date),
                   result = last(max_round)) %>%
  ungroup() %>%
  group_by(loser_id) %>%
  arrange(date) %>%
  mutate(last_result = lag(result),
         last_2_result = lag(result,2),
         last_3_result = lag(result,3))


### Data for model

df_subset <- df %>%
  dplyr::select(tourney_id,tourney_name,surface,round,tourney_level,tournament_date,winner_id,winner_seed,winner_hand,winner_age,winner_ht,
         loser_id,loser_seed,loser_hand,loser_age,loser_ht,winner_rank,loser_rank) %>%
  mutate(diff_seed = winner_seed - loser_seed,
         age_diff = winner_age - loser_age,
         ht_diff = winner_ht - loser_ht,
         rank_diff = winner_rank - loser_rank)


df_subset_winner <- df_subset %>%
  left_join(df_win_experience %>% dplyr::select(winner_id,tourney_id,experience)) %>%
  left_join(df_win_experience_last3 %>% dplyr::select(winner_id,tourney_id,last_result)) %>%
  dplyr::select(surface,round,tourney_level,diff_seed,age_diff,ht_diff,rank_diff,experience,last_result) %>%
  drop_na() %>%
  mutate(win_lose = 1) 

df_subset_loser <- df_subset %>%
  left_join(df_lost_experience %>% dplyr::select(loser_id,tourney_id,experience)) %>%
  left_join(df_lost_experience_last3 %>% dplyr::select(loser_id,tourney_id,last_result)) %>%
  dplyr::select(surface,round,tourney_level,diff_seed,age_diff,ht_diff,rank_diff,experience,last_result) %>%
  drop_na() %>%
  mutate(win_lose = -1)


df_model <- df_subset_winner  %>%
  rbind(df_subset_loser)






library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

colnames(df_model)
df_model$win_lose = as.factor(df_model$win_lose)

if_one_level = function(fac){
  if (is.factor(fac)) {
    return(!length(levels(as.factor(fac)))==1)
  }else{
    return(T)
  }
}

df_model = df_model %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select_if(if_one_level)
df_model %>% skim()

# logistic regresion
glm_fit_all = glm(data = df_model,
                  formula = win_lose~., 
                  family = binomial())

summary(glm_fit_all)

# get best column that minimize AIC
glm_fit_step = step(object = glm_fit_all, direction = "both")
summary(glm_fit_step)

# library(gtsummary)
# tbl_regression(glm_fit_step, exponentiate = TRUE)

### Feature Importance
library(readxl)
library(dplyr)
library(ggplot2)
library(randomForest)
library(varImp)
library(janitor)


model_randomForest <- randomForest(formula = win_lose~.,
                                   data = df_model, 
                                   importance=TRUE) 

model_randomForest #Review the Random Forest Results
plot(model_randomForest) #Plot the Random Forest Results
varImpPlot(model_randomForest)
#plot random trees

### Decision Tree
library(rpart)
library(rpart.plot)
fit_rpart <- rpart(formula = win_lose~.,data = df_model, method = 'class')
rpart.plot(fit_rpart, cex = .7)

### Removing Last result
fit_rpart2 <- rpart(formula = win_lose~. - last_result,
                    data = df_model, 
                    method = 'class')
rpart.plot(fit_rpart2, cex = 1)


### LDA
library(MASS)
# df_transformed <- df_model %>%
#   dplyr::dplyr::select(diff_seed,age_diff,ht_diff,rank_diff,experience,win_lose) %>%
#   scale() %>%
#   as.data.frame()
# Fit the model
model_lda <- lda(win_lose~diff_seed+age_diff+ht_diff+rank_diff+experience,data = df_model)
model_lda

#add eda
#Visualising distributions
#categorical
library(tidyverse)
library(dplyr)
library(ggplot2)
ggplot(data = df) +
  geom_bar(mapping = aes(x = surface))

ggplot(data = df) +
  geom_bar(mapping = aes(x = tourney_name))

ggplot(data = df) +
  geom_bar(mapping = aes(x = winner_hand))
ggplot(data = df) +
  geom_bar(mapping = aes(x = loser_hand))
#numerical 

