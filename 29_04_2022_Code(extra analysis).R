library(RSelenium)
library(tidyverse)
library(rvest)
library(xml2)
startServer()


rD <- rsDriver(browser = 'firefox',port = 6632L,verbose = F
)
remDr <- rD[['Client']]

rD$client$navigate("http://www.tennisabstract.com/cgi-bin/leaders_wta.cgi")

a <- rD$client$findElement(using='id',value='stats')

html <- rD$client$getPageSource()[[1]]


data <- read_html(html) %>%
  html_nodes("table.tablesorter") %>%
  html_table() %>%
  .[[1]]


data.table::fwrite(data,'Scrapped-data.csv')


dfscr <- read_csv('scrapped-data.csv')

#EDA on this dataframe:
library(DataExplorer)
## View basic description 
introduce(dfscr)
## Plot basic description for data
plot_intro(dfscr)
## View histogram of all continuous variables
plot_histogram(dfscr)
## View estimated density distribution of all continuous variables
plot_density(dfscr)

#not all results are used, some give understanding and insights for other work in other scripts

# Analysis ----------------------------------------------------------------
library(tidyverse)
df <- read_csv('wta_matches_1920.csv')
for (i in 1921:2022){
  str_temp <- paste('wta_matches_',i,'.csv',sep="")
  df_temp <- read_csv(str_temp)
  df <- rbind(df,df_temp)
}


data.table::fwrite(df,'wta_matches_all_data.csv')

df_1 <- read_csv('wta_matches_qual_itf_1920.csv')
for (i in 1921:2022){
  tryCatch({
    str_temp <- paste('wta_matches_qual_itf_',i,'.csv',sep="")
    df_temp <- read_csv(str_temp)
    df_1 <- rbind(df_1,df_temp)
  },
  error=function(cond) {
    #message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    df_1 <- df_1
  }
  )
}

data.table::fwrite(df_1 %>% unique(),'wta_matches_all.csv')

# Data Exploration --------------------------------------------------------


#### Filtering all data before 2000
df <- read_csv('wta_matches_all.csv') %>%
  mutate(tournament_date = as.Date(as.character(tourney_date),"%Y%m%d"),
         year = lubridate::year(tournament_date)) %>%
  filter(year>2000)


colnames(df)

#EDA on this dataframe:
library(DataExplorer)
## View basic description for airquality data
introduce(df)
## Plot basic description for data
plot_intro(df)
## View histogram of all continuous variables
plot_histogram(df)
## View estimated density distribution of all continuous variables
plot_density(df)

##frequency distribution of all discrete variables
plot_bar(df)

print (length(unique(df$tourney_id))) 
### 10456 tournaments

print (length(unique(df$surface)))
### 4 surfaces

df %>%
  select(surface,tourney_id) %>%
  unique()%>%
  group_by(surface) %>%
  summarise(count = n())

print (length(unique(df$tourney_level)))
### 20 levels

hist(df$winner_age)
hist(df$loser_age)



### Comparing Effect of Age with winning/losing

print (paste(mean(df$winner_age,na.rm = T),",",sd(df$winner_age,na.rm=T)))

print (paste(mean(df$loser_age,na.rm = T),",",sd(df$loser_age,na.rm=T)))

#### As we can see there is little difference between the ages of loser and winner, also seen by the histograms


### We this move on to check if the relative ages have anything to play
df_age <- transform(df, winner_group=cut(winner_age,  breaks=c(-Inf,5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, Inf),
                                         labels=c('<5', '5-10', '10-15', '15-20','20-25','25-30','30-35','35-40','40-45',
                                                  '45-50','50-55','55-60','>65'))) %>%
  transform(loser_group=cut(loser_age,  breaks=c(-Inf,5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, Inf),
                             labels=c('<5', '5-10', '10-15', '15-20','20-25','25-30','30-35','35-40','40-45',
                                      '45-50','50-55','55-60','>65'))) %>%
  select(winner_age,winner_group,loser_age,loser_group) %>%
  drop_na()

df_1 <- df_age %>%
  select(winner_group,loser_group) %>%
  group_by(winner_group) %>% summarise(winners = n()) %>%
  rename(age = winner_group)

df_2 <- df_age %>%
  select(winner_group,loser_group) %>%
  group_by(loser_group) %>% summarise(losers = n()) %>%
  rename(age = loser_group)

df_age_f <- df_1 %>%
  left_join(df_2) %>%
  mutate(Probability_win = winners*100/(winners+losers)) 

# Barplot
ggplot(df_age_f, aes(x=age, y=Probability_win, fill=Probability_win)) + 
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_continuous(low="red", high="green") +
  geom_hline(yintercept = 50)

####  The above chart shows the relative probability of winning for payers wrt age group

df_age_comp_1 <- df_age %>% 
  mutate(band = paste(winner_group,":",loser_group)) %>% 
  group_by(band) %>% 
  summarise(winner = n())

df_age_comp_2 <- df_age %>% 
  mutate(band = paste(loser_group,":",winner_group)) %>% 
  group_by(band) %>% 
  summarise(loser = n())

df_age_comp_final <- df_age_comp_1 %>%
  left_join(df_age_comp_2) %>%
  separate(band,sep = ": ",c('first','second')) %>%
  mutate(prob = winner/(winner+loser)) %>%
  arrange(first,second) %>%
  drop_na()

level_order <- c('<5', '5-10', '10-15', '15-20','20-25','25-30','30-35','35-40','40-45',
                 '45-50','50-55','55-60','>65')


ggplot(df_age_comp_final, aes(x = first, y = factor(second, level_order), fill= prob)) + 
  geom_tile()  +
  scale_fill_gradient(low="red", high="green") +
  theme_classic() +
  xlab('1st Player Age') + 
  ylab('2nd Player Age') +
  ggtitle("Probability of winning (1st Player)")

##### The above chart shows the probability of 1st player winning wrt age of the second player

### Comparing Effect of Height with winning/losing

print (paste(mean(df$winner_ht,na.rm = T),",",sd(df$winner_ht,na.rm=T)))

print (paste(mean(df$loser_ht,na.rm = T),",",sd(df$loser_ht,na.rm=T)))

df_ht <- transform(df, winner_group=cut(winner_ht,  breaks=c(-Inf,155, 160, 165, 170, 175, 180, 185, 190, Inf),
                                         labels=c('<155', '155-160', '160-165', '165-170','170-175','175-180','180-185','185-190','>190'))) %>%
  transform(loser_group=cut(loser_ht,  breaks=c(-Inf,155, 160, 165, 170, 175, 180, 185, 190, Inf),
                            labels=c('<155', '155-160', '160-165', '165-170','170-175','175-180','180-185','185-190','>190'))) %>%
  select(winner_ht,winner_group,loser_ht,loser_group) %>%
  drop_na()

df_1 <- df_ht %>%
  select(winner_group,loser_group) %>%
  group_by(winner_group) %>% summarise(winners = n()) %>%
  rename(age = winner_group)

df_2 <- df_ht %>%
  select(winner_group,loser_group) %>%
  group_by(loser_group) %>% summarise(losers = n()) %>%
  rename(age = loser_group)

df_ht_f <- df_1 %>%
  left_join(df_2) %>%
  mutate(Probability_win = winners*100/(winners+losers)) 

# Barplot
ggplot(df_ht_f, aes(x=age, y=Probability_win, fill=Probability_win)) + 
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_continuous(low="red", high="green") +
  geom_hline(yintercept = 50)

####  The above chart shows the relative probability of winning for payers wrt height group

df_ht_comp_1 <- df_ht %>% 
  mutate(band = paste(winner_group,":",loser_group)) %>% 
  group_by(band) %>% 
  summarise(winner = n())

df_ht_comp_2 <- df_ht %>% 
  mutate(band = paste(loser_group,":",winner_group)) %>% 
  group_by(band) %>% 
  summarise(loser = n())

df_ht_comp_final <- df_ht_comp_1 %>%
  left_join(df_ht_comp_2) %>%
  separate(band,sep = ": ",c('first','second')) %>%
  mutate(prob = winner/(winner+loser)) %>%
  arrange(first,second) %>%
  drop_na()

level_order <- c('<155', '155-160', '160-165', '165-170','170-175','175-180','180-185','185-190','>190')

ggplot(df_ht_comp_final, aes(x = first, y = factor(second, level_order), fill= prob)) + 
  geom_tile()  +
  scale_fill_gradient(low="red", high="green") +
  theme_classic() +
  xlab('1st Player Ht') + 
  ylab('2nd Player Ht') +
  ggtitle("Probability of winning (1st Player)")


df %>%
  mutate(ht_diff = winner_ht-loser_ht) %>%
  ggplot(aes(x = ht_diff)) +
  geom_density() +
  labs(x= "Height Difference",y = "Count", title = paste("Distribution of", " Height Difference ")) +
  theme_bw()

### Only Grandslame
df %>%
  filter(tourney_level=='G') %>%
  mutate(ht_diff = winner_ht-loser_ht) %>%
  ggplot(aes(x = ht_diff)) +
  geom_density() +
  labs(x= "Height Difference",y = "Count", title = paste("Distribution of", " Height Difference for Grand-slams")) +
  theme_bw()


#### Correlation between height and win %

df_height_corr <- df %>%
  mutate(winner_ht_int = as.integer(winner_ht),
         loser_ht_int = as.integer(loser_ht)) %>%
  select(tourney_level,winner_ht_int,loser_ht_int)

df_height_corr_1 <- df_height_corr %>%
  group_by(winner_ht_int) %>%
  summarise(win  = n()) %>%
  rename(ht = winner_ht_int)



df_height_corr_2 <- df_height_corr %>%
  group_by(loser_ht_int) %>%
  summarise(loss  = n()) %>%
  rename(ht = loser_ht_int)

df_height_corr_fin <- df_height_corr_1 %>%
  left_join(df_height_corr_2) %>%
  mutate(perct = win/(win+loss)) %>%
  drop_na()

cor(df_height_corr_fin$ht,df_height_corr_fin$perct)

### We can see that the correlation of height with winning % is less

### Age correlation

df_age_corr <- df %>%
  mutate(winner_age_int = as.integer(winner_age),
         loser_age_int = as.integer(loser_age)) %>%
  select(tourney_level,winner_age_int,loser_age_int)

df_age_corr_1 <- df_age_corr %>%
  group_by(winner_age_int) %>%
  summarise(win  = n()) %>%
  rename(age = winner_age_int)



df_age_corr_2 <- df_age_corr %>%
  group_by(loser_age_int) %>%
  summarise(loss  = n()) %>%
  rename(age = loser_age_int)

df_age_corr_fin <- df_age_corr_1 %>%
  left_join(df_age_corr_2) %>%
  mutate(perct = win/(win+loss)) %>%
  drop_na()

cor(df_age_corr_fin$age,df_age_corr_fin$perct)

### Age has a small negative correlation to winning percentage

### Experience and performance
df_win_experience <- df %>% 
  #filter(tourney_level=='G') %>%
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
  select(winner_id,tourney_id,tournament_date,year,min_year,experience,max_round)


df_win_experience 


ggplot(df_win_experience %>% filter(max_round %in% c('F')), aes(x=experience, colour=max_round)) + geom_density()

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
  select(loser_id,tourney_id,tournament_date,year,min_year,experience,max_round)



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

#to be completed

### Data for model

df_subset <- df %>%
  select(tourney_id,tourney_name,surface,round,tourney_level,tournament_date,winner_id,winner_seed,winner_hand,winner_age,winner_ht,
         loser_id,loser_seed,loser_hand,loser_age,loser_ht,winner_rank,loser_rank) %>%
  mutate(diff_seed = winner_seed - loser_seed,
         age_diff = winner_age - loser_age,
         ht_diff = winner_ht - loser_ht,
         rank_diff = winner_rank - loser_rank)


df_subset_winner <- df_subset %>%
  left_join(df_win_experience %>% select(winner_id,tourney_id,experience)) %>%
  left_join(df_win_experience_last3 %>% select(winner_id,tourney_id,last_result)) %>%
  select(surface,round,tourney_level,diff_seed,age_diff,ht_diff,rank_diff,experience,last_result) %>%
  drop_na() %>%
  mutate(win_lose = 1) 

df_subset_loser <- df_subset %>%
  left_join(df_lost_experience %>% select(loser_id,tourney_id,experience)) %>%
  left_join(df_lost_experience_last3 %>% select(loser_id,tourney_id,last_result)) %>%
  select(surface,round,tourney_level,diff_seed,age_diff,ht_diff,rank_diff,experience,last_result) %>%
  drop_na() %>%
  mutate(win_lose = -1)


df_model <- df_subset_winner  %>%
  rbind(df_subset_loser)

#### Statistical tests

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

colnames(df_model)


### Difference in Seed


#lm = lm(win_lose~surface+tourney_level+diff_seed+age_diff+ht_diff+rank_diff+experience+last_result, data = df_model) #Create the linear regression
#summary(lm)

glm = glm(win_lose~surface+tourney_level+diff_seed+age_diff+ht_diff+rank_diff+experience+last_result, data = df_model) #Create the logistic regression
summary(glm)

library(gtsummary)
tbl_regression(glm, exponentiate = TRUE)

### Feature Importance
library(readxl)
library(dplyr)
library(ggplot2)
library(randomForest)
library(varImp)
library(janitor)


model <- randomForest(win_lose~surface+tourney_level+diff_seed+age_diff+ht_diff+rank_diff+experience+last_result,
                      df_model, importance=TRUE) 

model #Review the Random Forest Results
plot(model) #Plot the Random Forest Results
varImpPlot(model)
#plot random trees

### Decision Tree
library(rpart)
library(rpart.plot)
fit <- rpart(win_lose~surface+tourney_level+diff_seed+age_diff+ht_diff+rank_diff+experience+last_result,
             df_model, method = 'class')
rpart.plot(fit, extra = 106)

### Removing Last result
fit <- rpart(win_lose~surface+tourney_level+diff_seed+age_diff+ht_diff+rank_diff+experience,
             df_model, method = 'class')
rpart.plot(fit, extra = 106)


### LDA
library(MASS)

df_transformed <- df_model %>%
  dplyr::select(diff_seed,age_diff,ht_diff,rank_diff,experience,win_lose) %>%
  scale() %>%
  as.data.frame()
# Fit the model
model <- lda(win_lose~diff_seed+age_diff+ht_diff+rank_diff+experience,
             df_transformed)
model

tbl_regression(model, exponentiate = TRUE)

##### Clustering

df_winner_attr <- df %>%
  filter(tourney_level=='G') %>%
  dplyr::group_by(winner_id,winner_name,surface) %>%
  dplyr::summarise(winner_hand = max(winner_hand),
            winner_seed = mean(winner_seed,na.rm=T),
            winner_ht = mean(winner_ht,na.rm=T),
            matches_won = n(),
            win_ace = mean(w_ace,na.rm=T),
            winner_rank_points = mean(winner_rank_points,na.rm=T)) %>%
  ungroup() %>%
  dplyr::rename(player_id = winner_id,
         player_name = winner_name)



df_loser_attr <- df %>%
  filter(tourney_level=='G') %>%
  dplyr::group_by(loser_id,loser_name,surface) %>%
  dplyr::summarise(loser_hand = max(loser_hand),
            loser_seed = mean(loser_seed,na.rm=T),
            loser_ht = mean(loser_ht,na.rm=T),
            matches_lost = n(),
            lost_ace = mean(l_ace,na.rm=T),
            loser_rank_points = mean(loser_rank_points,na.rm=T)) %>%
  ungroup() %>%
  dplyr::rename(player_id = loser_id,
         player_name = loser_name)



df_player <- df_winner_attr %>%
  left_join(df_loser_attr) %>%
  dplyr::select(-c(winner_rank_points,loser_rank_points,loser_hand)) %>%
  group_by(player_id) %>%
  mutate(winner_seed=ifelse(is.na(winner_seed),mean(winner_seed,na.rm=TRUE),winner_seed),
         win_ace =ifelse(is.na(win_ace),mean(win_ace,na.rm=TRUE),win_ace),
         loser_seed =ifelse(is.na(loser_seed),mean(loser_seed,na.rm=TRUE),loser_seed),
         lost_ace =ifelse(is.na(lost_ace),mean(lost_ace,na.rm=TRUE),lost_ace)) %>%
  mutate(win_perct = matches_won/(matches_won+matches_lost)) %>%
  dplyr::select(-c('matches_won','matches_lost','loser_ht')) %>%
  ungroup() %>%
  mutate(winner_seed=ifelse(is.na(winner_seed),mean(winner_seed,na.rm=TRUE),winner_seed),
         win_ace =ifelse(is.na(win_ace),mean(win_ace,na.rm=TRUE),win_ace),
         loser_seed =ifelse(is.na(loser_seed),mean(loser_seed,na.rm=TRUE),loser_seed),
         lost_ace =ifelse(is.na(lost_ace),mean(lost_ace,na.rm=TRUE),lost_ace)) %>%
  drop_na() %>%
  mutate(surface_num = ifelse(surface=='Clay',1,ifelse(surface=='Grass',2,3)),
         winner_hand = ifelse(winner_hand=='R',1,2))



df_player_cluster <- df_player %>%
  dplyr::select(-c('player_id','player_name','surface','winner_hand','surface_num')) %>%
  scale()


players.pca <- prcomp(df_player_cluster, center = TRUE,scale. = TRUE)

summary(players.pca)

library(ggbiplot)

ggbiplot(players.pca)



wssplot <- function(data, nc=20, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(df_player_cluster)

kmean <- kmeans(df_player_cluster, 5)
kmean$centers

library(factoextra)
fviz_cluster(kmean, data = df_player_cluster,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


df_player$cluster = kmean$cluster


df_cluster <- df_player %>% 
  group_by(cluster) %>%
  dplyr::summarise(winner_seed = mean(winner_seed),
            winner_ht = mean(winner_ht),
            win_ace = mean(win_ace),
            loser_seed = mean(loser_seed),
            lost_ace = mean(lost_ace),
            win_perct = mean(win_perct)) 

print (df_cluster)
#fix dendogram
### Hierarchical Clustering

distance_mat <- dist(df_player_cluster, method = 'euclidean')
distance_mat


Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl

plot(Hierar_cl)

# Choosing no. of clusters
# Cutting tree by height
abline(h = 110, col = "green")

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 5 )
fit

table(fit)
### As can be seen hierarchical clustering doesn't give good results here

### Trying DBscan

library(fpc)

df_player_cluster_db <- df_player %>%
  dplyr::select(-c('player_id','player_name','surface','winner_hand','surface_num'))


Dbscan_cl <- dbscan(df_player_cluster_db, eps = 3, MinPts = 100)
Dbscan_cl

# Checking cluster
Dbscan_cl$cluster

# Table
table(Dbscan_cl$cluster)

# Plotting Cluster
plot(Dbscan_cl, df_player_cluster_db, main = "DBScan")

############## Upset analysis

df_upset <- df %>%
  mutate(diff_seed = winner_seed - loser_seed) %>%
  mutate(upset = ifelse(diff_seed>5,"Upset","Not-upset"))


### Distribution of upset wrt surface
df_upset %>%
  group_by(surface,upset) %>%
  dplyr::summarise(count = n()) %>%
  drop_na() %>%
  ungroup() %>%
  group_by(upset) %>%
  mutate(ratio = count/sum(count)) %>%
  ungroup() %>%
  filter(upset == 'Upset') %>%
  select(surface,ratio)



### Upset and height
df_upset_surface <- df_upset %>%
  mutate(ht_diff = winner_ht,loser_ht) %>%
  dplyr::select(ht_diff,upset,surface) %>%
  arrange(surface) %>%
  drop_na()

ggplot(df_upset_surface, aes(x=upset, y=ht_diff)) + 
  geom_boxplot(notch=TRUE)


df_upset_1 <- df_upset_surface %>%
  filter(upset == 'Upset') %>%
  dplyr::select(ht_diff)


df_upset_2 <- df_upset_surface %>%
  filter(upset != 'Upset') %>%
  dplyr::select(ht_diff)

t.test(df_upset_1,df_upset_2)


### Upset in smaller tournamanet
df_upset %>%
  group_by(upset,tourney_level) %>%
  dplyr::summarise(count = n()) %>%
  drop_na() %>%
  ungroup() %>%
  group_by(upset) %>%
  mutate(ratio = count/sum(count)) %>%
  ungroup() %>%
  filter(upset == 'Upset') %>%
  dplyr::select(tourney_level,ratio) %>%
  ggplot(aes(x=reorder(tourney_level,ratio), y=ratio)) +
  geom_bar(stat='identity')



#### Comebacks

library(purrr)


df_comeback <- df %>%
  mutate(Score_2 = (strsplit(df$score,split = " "))) 
  
df_comeback$num_sets <- lapply(df_comeback$Score_2, length)



df_comeback <- df_comeback %>%
  mutate(num_sets_2 = as.numeric(num_sets),
         first_set = as.character(map(df_comeback$Score_2, 1)))

df_comeback_v2 <- df_comeback %>%
  filter(num_sets_2 == 3) %>%
  mutate(
    win_score = as.numeric(map((strsplit(first_set,split = "-")),1)),
    lose_score = as.numeric(map((strsplit(first_set,split = "-")),2))
         ) %>%
  dplyr::select(win_score,lose_score) %>%
  mutate(
    first_set = ifelse(win_score>lose_score,1,0)
  )


a <- nrow(df_comeback_v2 %>% filter(first_set==0)) +

nrow(df_comeback %>% filter(num_sets_2==2))

b <- nrow(df_comeback)


a/b


### We see that the total matches 349636, and the number of matches where the player lost after loseing the first set 
### is 292051, therefore the % chance of losing after losing the first set is: 83%, and conversely the chance of winning is 17%



#### Left handed player over right handed
df_lr <- df %>%
  dplyr::select(winner_hand,loser_hand) %>%
  drop_na() %>%
  filter(winner_hand != loser_hand) %>%
  mutate(hc = paste(winner_hand,loser_hand),
         hc2 = paste(loser_hand,winner_hand)) %>%
  group_by(hc,hc2) %>%
  dplyr::summarise(count = n())

df_lr <- left_join(
  df_lr %>% select(hc,count) %>% dplyr::rename(hand = hc,count_win=count),
  df_lr %>% select(hc2,count) %>% dplyr::rename(hand = hc2,count_lose=count)
) %>%
  mutate(prob = count_win/(count_win+count_lose))
  

#### we see that Left handed players have a slight advantage over right handed players, both have a huge advantage over U players




####################################

#### Using scrapped data
### Predicting win % basis different features

df_scrapped <- read_csv('scrapped-data.csv')

colnames(df_scrapped)

df_scrapped_model <-df_scrapped %>%
  dplyr::select(`M W%`,SPW,`Ace%`,`DF%`,`DF/2s`,`1stIn`,`1st%`,`2nd%`,`2%-InP`,`Hld%`)
  
dat <- as.data.frame(sapply( df_scrapped_model, parse_number )) %>%
  as.tibble()

model <- lm(`M W%`~.,dat)
summary(model)


### Feature Importance
library(readxl)
library(dplyr)
library(ggplot2)
library(randomForest)
library(varImp)
library(janitor)
dat_model <- dat %>% clean_names()

model <- randomForest(m_w_percent   ~.,dat_model, importance=TRUE) 

model #Review the Random Forest Results
plot(model) #Plot the Random Forest Results
varImpPlot(model)

save.image(file = 'workspace.RData')

