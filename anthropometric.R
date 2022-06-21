library(tidyverse)
library(ggthemes)
library(scales)
theme_set(theme_fivethirtyeight())

path = "datasets/"
allfiles = list.files(path)
wta_matches = str_detect(string = allfiles, pattern = "wta_matches_[:digit:]")
wta_matches = allfiles[wta_matches]

wta_matches_years = wta_matches %>% 
  str_extract_all(pattern = "[:digit:]") %>% 
  map_dbl(~as.double(paste0(.x,collapse = '')))
  
wta_matches_2001_2022 = wta_matches[wta_matches_years>2000]

flag = TRUE
options(warn = -1)

for (file in wta_matches_2001_2022) {
  if (flag) {
    df = read.csv(paste0(path,file))
    flag = FALSE
  }else{
    df = rbind(df,read.csv(paste0(path,file)))
  }
}

df["Year"] = df$tourney_id %>% str_sub(start = 1,end = 4)
df["Sets"] = df$score %>% str_count(pattern = "-")
df['Rank_Diff'] =  df$loser_rank - df$winner_rank
df['Rank_Diff_Round'] = 10*round(df$Rank_Diff/10)
df['ind'] = 1:dim(df)[1]
df = df %>% column_to_rownames(var = "ind")

df['Height_Diff'] = df$winner_ht - df$loser_ht
df %>% 
  ggplot(aes(x = Height_Diff)) +
  geom_density(col = "dodgerblue1", size = 2) +
  xlim(c(-25,25)) + 
  xlab("Height Difference") + ylab("") + 
  theme(axis.title = element_text())

winners = unique(df$winner_name)
losers = unique(df$loser_name)
all_players = c(winners,losers)
players = unique(all_players)

players_df = data.frame(Name = players)
Wins = df %>% group_by(winner_name) %>% summarise(Wins = n())
Losses = df %>% group_by(loser_name) %>% summarise(Losses = n())
players_df = left_join(x = players_df, y = Wins, by = c("Name" = "winner_name"))
players_df = left_join(x = players_df, y = Losses, by = c("Name" = "loser_name"))


players_df = players_df %>% 
  mutate(Games = replace_na(Wins, replace = 0) + replace_na(Losses, replace = 0),
         PCT = replace_na(Wins, replace = 0) / Games)


surfaces = c('Hard','Grass','Clay','Carpet')
for (surfaceee in surfaces) {
  Wins = df %>% dplyr::filter(surface == surfaceee) %>% group_by(winner_name) %>% summarise(Wins = n())
  colnames(Wins)[2] = paste0(surfaceee,'_wins')
  Losses = df %>% filter(surface == surfaceee) %>% group_by(loser_name) %>% summarise(Losses = n())
  colnames(Losses)[2] = paste0(surfaceee,'_losses')
  players_df = left_join(x = players_df, y = Wins, by = c("Name" = "winner_name"))
  players_df = left_join(x = players_df, y = Losses, by = c("Name" = "loser_name"))
  players_df = players_df %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(test = get(paste0(surfaceee,'_wins'))/(get(paste0(surfaceee,'_wins'))+get(paste0(surfaceee,'_losses')))) %>% 
    mutate_if(is.numeric, replace_na, replace = 0)
  
  colnames(players_df)[length(players_df)] = paste0(surfaceee,'PCT')
}



serious_players = players_df[players_df$Games>40,]

Height = df %>% group_by(winner_name) %>% summarise(Height = mean(winner_ht, na.rm = T))
serious_players = left_join(x = serious_players, y = Height, by = c("Name" = "winner_name"))

Best_Rank = df %>% group_by(winner_name) %>% summarise(Best_Rank = min(winner_rank, na.rm = T))
serious_players = left_join(x = serious_players, y = Best_Rank, by = c("Name" = "winner_name"))

Win_Aces = df %>% group_by(winner_name) %>% summarise(Win_Aces = mean(w_ace, na.rm = T))
serious_players = left_join(x = serious_players, y = Win_Aces, by = c("Name" = "winner_name"))

Lose_Aces = df %>% group_by(loser_name) %>% summarise(Lose_Aces = mean(l_ace, na.rm = T))
serious_players = left_join(x = serious_players, y = Lose_Aces, by = c("Name" = "loser_name"))

serious_players = serious_players %>% 
  # mutate(Aces = (replace_na(Win_Aces,replace = 0)*replace_na(Wins,replace = 0) + 
  #                  replace_na(Lose_Aces,replace = 0)*replace_na(Losses,replace = 0)) / 
  #          replace_na(Games,replace = 0)) %>% 
  mutate(Aces = (Win_Aces*Wins + Lose_Aces*Losses) / Games)


serious_players %>% 
  ggplot(aes(x = Height)) +
  geom_density(col = "dodgerblue1", size = 2) +
  xlab("Height") + ylab("") + 
  theme(axis.title = element_text())


serious_players = serious_players %>% 
  filter(!is.na(Height))


serious_players %>% 
  mutate(Height = Height + rnorm(dim(serious_players)[1]),
         Aces = Aces + rnorm(dim(serious_players)[1])) %>% 
  ggplot(aes(x = Height, y = Aces)) +
  geom_point(size = 2, alpha = 0.7, col = "dodgerblue1")+
  geom_smooth(method = "lm", col = "red", size = 1.5) + 
  xlab("Height [cm]") + ylab("Aces Per Match") + 
  labs(title = 'Height Matters #1 - More Aces') + 
  theme(axis.title = element_text())


serious_players %>% 
  mutate(Height = Height + rnorm(dim(serious_players)[1]),
         Best_Rank = Best_Rank + rnorm(dim(serious_players)[1])) %>% 
  ggplot(aes(x = Height, y = Best_Rank)) +
  geom_point(size = 2, alpha = 0.7, col = "dodgerblue1")+
  geom_smooth(method = "lm", col = "red", size = 1.5) + 
  xlab("Height [cm]") + ylab("Highest Career Rank") + 
  labs(title = 'Height Matters #2 - Best Career Rank') + 
  theme(axis.title = element_text())


#serious_players %>% 
#  mutate(Height = Height + rnorm(dim(serious_players)[1]),
#        PCT = PCT + rnorm(dim(serious_players)[1])) %>% 
# ggplot(aes(x = Height, y = PCT)) +
#  geom_point(size = 2, alpha = 0.7, col = "dodgerblue1")+
#  geom_smooth(method = "lm", col = "red") + 
#  xlab("Height [cm]") + ylab("Aces Per Match") + 
#  labs(title = 'Height Matters #3 - General PCT') + 
#  theme(axis.title = element_text())


paste0('Average Player Height : ', mean(serious_players$Height,na.rm = T))
paste0('Average Height of Rank Leaders : ', mean(serious_players$Height[serious_players$Best_Rank == 1],na.rm = T))

height_diff_df = data.frame(height_bins = unique(df$Height_Diff))

Height_Prob = numeric(dim(height_diff_df)[1])
for (roww in 1:dim(height_diff_df)[1]) {
  fo = sum(df$Height_Diff == height_diff_df$height_bins[roww],na.rm = T)
  ta = sum(df$Height_Diff == (-height_diff_df$height_bins[roww]),na.rm = T)
  Height_Prob[roww] = fo/(fo + ta)
}

height_diff_df['Height_Prob'] = Height_Prob

height_diff_df %>% 
  ggplot(aes(x = height_bins, y = Height_Prob)) +
  geom_point(size = 2, alpha = 0.7, col = "dodgerblue1")+
  geom_smooth(method = "lm", col = "red") + 
  xlab("Height Difference") + ylab("Winning Chances") + 
  xlim(c(0,25)) + ylim(c(0.5,0.7)) +
  theme(axis.title = element_text())

df %>% 
  group_by(Year) %>% 
  summarise(avg_winner = mean(winner_ht,na.rm = T),
            avg_loser = mean(loser_ht,na.rm = T)) %>% 
  mutate(avg_height = (avg_loser + avg_winner)/2) %>% 
  ggplot(aes(x = Year, y = avg_height)) + 
  geom_col(fill = "dodgerblue1") + 
  scale_y_continuous(limits=c(170,175),oob = rescale_none) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("Average Height") + xlab("Year") +
  labs(title = "Height Evolution") +
  theme(axis.title = element_text())


  
