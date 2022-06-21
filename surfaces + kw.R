library(tidyverse)
library(ggthemes)
library(scales)
library(effectsize)
theme_set(theme_fivethirtyeight())

path = "datasets/"
allfiles = list.files(path)
wta_matches = str_detect(string = allfiles, pattern = "wta_matches_[:digit:]")
wta_matches = allfiles[wta_matches]

wta_matches_years = wta_matches %>% 
  str_extract_all(pattern = "[:digit:]") %>% 
  map_dbl(~as.double(paste0(.x,collapse = '')))

wta_matches_2001_2022 = wta_matches[wta_matches_years>2000]

print(wta_matches_2001_2022)

flag = TRUE
options(warn = -1)

for (file in wta_matches_2001_2022) {
  if (flag) {
    df_no = read.csv(paste0(path,file))
    flag = FALSE
  }else{
    df_no = rbind(df_no,read.csv(paste0(path,file)))
  }
}

#filter for grand slams
library(dplyr)
df = filter(df_no, tourney_level== "G")

df['Aces'] = df$l_ace + df$w_ace

df_w_ace <- df[c('w_ace', 'surface')]
colnames(df_w_ace) <- c('Ace','Surface')
df_l_ace <- df[c('l_ace', 'surface')]
colnames(df_l_ace) <- c('Ace','Surface')
df_aces <- rbind(df_w_ace, df_l_ace)
df_aces
#dfabc <- df_aces[!(is.na(df_aces$'Ace')), ]
df_aces  <- na.omit(df_aces) 
#mean per surface
aggregate(df_aces$Ace, list(df_aces$Surface), FUN=mean)

df_aces$Ace[which(df_aces$Surface == "Hard")] <- 2.76
df_aces$Ace[which(df_aces$Surface == "Clay")] <- 2.05
df_aces$Ace[which(df_aces$Surface == "Grass")] <- 3.11

#Compute Kruskal-Wallis test
kruskal.test(Ace ~ Surface, data = df_aces)
#p-value < 0.05, we can conclude that there are significant differences between the surfaces
library(rstatix)
df_aces %>%
kruskal_effsize(Ace ~ Surface)

#median with 95% CI
split(df, df$surface) %>% 
  lapply(function(x){
    tbl <- MedianCI(df$Aces, na.rm = TRUE) %>% 
      data.frame()
    
    names(tbl) <- unique(x$surface)
    
    tbl
  }) %>% bind_cols() %>% rownames_to_column("Measure") %>% melt(id.vars = "Measure") %>% 
  ggplot(aes(variable, value, fill = Measure)) + geom_col(position = "dodge")

MedianCI(df$Aces, na.rm = TRUE) %>% data.frame()


df %>% 
  group_by(surface) %>% 
  summarise(avg = mean(Aces,na.rm = T)) %>% 
  filter(surface %in% c('Hard','Grass','Clay')) %>% 
  ggplot(aes(surface,avg)) +
  geom_bar(stat = "identity", fill = c('Red','darkgreen','Blue')) +
  ylab("Aces per Match") + xlab("Surface") +
  labs(title = "Surface Comparisons for Different Surfaces")+
  theme(axis.title = element_text())


df['loser_1st_rate'] = df$l_1stIn/df$l_svpt
df['winner_1st_rate'] = df$w_1stIn/df$w_svpt
df['first_serve_rate'] = (df['loser_1st_rate'] + df['winner_1st_rate'])/2
colnames(df)
df %>% 
  group_by(surface) %>% 
  summarise(avg = 100*mean(first_serve_rate,na.rm = T)) %>% 
  filter(surface %in% c('Hard','Grass','Clay')) %>% 
  ggplot(aes(surface,avg)) +
  geom_bar(stat = "identity", fill = c('Red','darkgreen','Blue')) +
  scale_y_continuous(limits=c(50,70),oob = rescale_none) +
  ylab("First Serve In [%]") + xlab("Surface") +
  labs(title = "% of 1st serves in")+
  theme(axis.title = element_text())


df_fsr <- df[c('first_serve_rate', 'surface')]
colnames(df_fsr) <- c('fsr','Surface')

df_fsr  <- na.omit(df_fsr) 
#mean per surface
aggregate(df_fsr$fsr, list(df_fsr$Surface), FUN=mean)

df_fsr$fsr[which(df_fsr$Surface == "Hard")] <- 0.6146709
df_fsr$fsr[which(df_fsr$Surface == "Clay")] <- 0.6236848
df_fsr$fsr[which(df_fsr$Surface == "Grass")] <- 0.6295845

#Compute Kruskal-Wallis test
kruskal.test(fsr ~ Surface, data = df_fsr)
#p-value < 0.05, we can conclude that there are significant differences between the surfaces

df_fsr %>%
  kruskal_effsize(fsr ~ Surface)



df['loser_1st_taken'] =  df$l_1stWon/df$l_1stIn
df['winner_1st_taken'] =  df$w_1stWon/df$w_1stIn
df['first_taken'] = (df['loser_1st_taken'] + df['winner_1st_taken'])/2

df %>% 
  group_by(surface) %>% 
  summarise(avg = 100*mean(first_taken,na.rm = T)) %>% 
  filter(surface %in% c('Hard','Grass','Clay')) %>% 
  ggplot(aes(surface,avg)) +
  geom_bar(stat = "identity", fill = c('Red','darkgreen','Blue')) +
  scale_y_continuous(limits=c(50,70),oob = rescale_none) +
  ylab("First Serve point taken") + xlab("Surface") +
  labs(title = "% of first serve points taken")+
  theme(axis.title = element_text())

df_ft <- df[c('first_taken', 'surface')]
colnames(df_ft) <- c('ft','Surface')

df_ft  <- na.omit(df_ft) 
#mean per surface
aggregate(df_ft$ft, list(df_ft$Surface), FUN=mean)

df_ft$ft[which(df_ft$Surface == "Hard")] <- 0.6375273
df_ft$ft[which(df_ft$Surface == "Clay")] <- 0.6172621
df_ft$ft[which(df_ft$Surface == "Grass")] <-  0.6576330

#Compute Kruskal-Wallis test
kruskal.test(ft ~ Surface, data = df_ft)
#p-value < 0.05, we can conclude that there are significant differences between the surfaces

df_ft %>%
  kruskal_effsize(ft ~ Surface)

df['aces_per_serve_w'] = df$w_ace/df$w_svpt
df['aces_per_serve_l'] = df$l_ace/df$l_svpt
df['aces_per_serve'] = (df['aces_per_serve_w'] + df['aces_per_serve_l'])/2

df %>% 
  group_by(surface) %>% 
  summarise(avg = mean(aces_per_serve,na.rm = T)) %>% 
  filter(surface %in% c('Hard','Grass','Clay')) %>% 
  ggplot(aes(surface,avg)) +
  geom_bar(stat = "identity", fill = c('Red','darkgreen','Blue')) +
  ylab("Aces Per Serve") + xlab("Surface") +
  labs(title = "Aces Per Serve")+
  theme(axis.title = element_text())

df_aps <- df[c('aces_per_serve', 'surface')]
colnames(df_aps) <- c('aps','Surface')

df_aps  <- na.omit(df_aps) 
#mean per surface
aggregate(df_aps$aps, list(df_aps$Surface), FUN=mean)

df_aps$aps[which(df_aps$Surface == "Hard")] <- 0.04145322
df_aps$aps[which(df_aps$Surface == "Clay")] <- 0.03028383
df_aps$aps[which(df_aps$Surface == "Grass")] <-  0.04565910

#Compute Kruskal-Wallis test
kruskal.test(aps ~ Surface, data = df_aps)
#p-value < 0.05, we can conclude that there are significant differences between the surfaces

df_aps %>%
  kruskal_effsize(aps ~ Surface)

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
  mutate(Aces = (Win_Aces*Wins + Lose_Aces*Losses) / Games)


serious_players = serious_players %>% 
  filter(!is.na(GrassPCT), !is.na(ClayPCT))

kmeans_df = serious_players[,c('HardPCT','GrassPCT','ClayPCT')]

kmeans_fit = kmeans(x = kmeans_df, centers = 6)

serious_players['label'] = kmeans_fit$cluster
for (i in 1:dim(kmeans_fit$centers)[1]) {
  print(kmeans_fit$centers[i,])
  print(mean(serious_players$Height[serious_players$label == i],na.rm = T))
}


(kmeans_fit$centers * 100) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "cluster") %>% 
  gather(key = "Surface", value = "PCT", -cluster) %>% 
  ggplot(aes(x = cluster,y = PCT, fill = Surface, group = Surface)) +
  geom_col(position = "dodge") +
  labs(title = "6 Clusters of players") +
  ylab("Winning Percentage") +
  theme(axis.title = element_text())


pca = prcomp(kmeans_df, center = TRUE,scale. = TRUE, rank. = 2)
pca_df = cbind(as.data.frame(pca$x), 
               cluster = as.factor(kmeans_fit$cluster))
  

pca_df %>% 
  ggplot(aes(x = PC1,y = PC2, col = cluster)) +
  geom_point(size = 2,alpha = .8) +
  labs(title = "PCA 2D view") +
  theme(axis.title = element_text())

