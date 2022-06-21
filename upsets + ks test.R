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


df["Year"] = df$tourney_id %>% str_sub(start = 1,end = 4)
df["Sets"] = df$score %>% str_count(pattern = "-")
df['Rank_Diff'] =  df$loser_rank - df$winner_rank
df['Rank_Diff_Round'] = 10*round(df$Rank_Diff/10)
df['ind'] = 1:dim(df)[1]
df = df %>% column_to_rownames(var = "ind")

bins = seq(10,200,10)
diff_df__nrow = length(bins)
Prob = Grass_Prob = Clay_Prob = Hard_Prob = numeric(diff_df__nrow)

for (i in 1:diff_df__nrow) {
  Prob[i] = sum(df$Rank_Diff_Round == bins[i], na.rm = T) / sum(abs(df$Rank_Diff_Round) == bins[i], na.rm = T)
  Grass_Prob[i] = sum((df$Rank_Diff_Round == bins[i]) & (df$surface == "Grass"), na.rm = T) / 
    sum((abs(df$Rank_Diff_Round) == bins[i]) & (df$surface == "Grass"), na.rm = T)
  Clay_Prob[i] = sum((df$Rank_Diff_Round == bins[i]) & (df$surface == "Clay"), na.rm = T) / 
    sum((abs(df$Rank_Diff_Round) == bins[i]) & (df$surface == "Clay"), na.rm = T)
  Hard_Prob[i] = sum((df$Rank_Diff_Round == bins[i]) & (df$surface == "Hard"), na.rm = T) / 
    sum((abs(df$Rank_Diff_Round) == bins[i]) & (df$surface == "Hard"), na.rm = T)
}

diff_df = data.frame(bins = bins,
                     Prob = Prob,
                     Grass = Grass_Prob,
                     Clay = Clay_Prob,
                     Hard = Hard_Prob)


diff_df %>% 
  ggplot(aes(x = bins,Prob)) +
  geom_col(fill = "dodgerblue1") + 
  scale_y_continuous(limits=c(0.5,0.9),oob = rescale_none) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("Winning Probability") + xlab("Rank Difference") +
  labs(title = "Chances of upsets") +
  theme(axis.title = element_text())


diff_df %>% 
  gather(key = "surface", value = "prob", -bins,-Prob) %>% 
  ggplot(mapping = aes(x = bins,y = prob, col = surface,group=surface)) +
  geom_line(fill = "dodgerblue1", size = 1.3) + 
  scale_y_continuous(limits=c(0.5,0.9),oob = rescale_none) +
  scale_color_manual(values = c("red", "darkgreen","blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("Winning Probability") + xlab("Rank Difference") +
  labs(title = "Upsets on Different Surfaces") +
  theme(axis.title = element_text())

big_tour_df = df[df$draw_size == 128,]
bins = seq(10,200,10)
diff_df__nrow = length(bins)
Prob = Grass_Prob = Clay_Prob = Hard_Prob = numeric(diff_df__nrow)

for (i in 1:diff_df__nrow) {
  Prob[i] = sum(big_tour_df$Rank_Diff_Round == bins[i], na.rm = T) / sum(abs(big_tour_df$Rank_Diff_Round) == bins[i], na.rm = T)
  Grass_Prob[i] = sum((big_tour_df$Rank_Diff_Round == bins[i]) & (big_tour_df$surface == "Grass"), na.rm = T) / 
    sum((abs(big_tour_df$Rank_Diff_Round) == bins[i]) & (big_tour_df$surface == "Grass"), na.rm = T)
  Clay_Prob[i] = sum((big_tour_df$Rank_Diff_Round == bins[i]) & (big_tour_df$surface == "Clay"), na.rm = T) / 
    sum((abs(big_tour_df$Rank_Diff_Round) == bins[i]) & (big_tour_df$surface == "Clay"), na.rm = T)
  Hard_Prob[i] = sum((big_tour_df$Rank_Diff_Round == bins[i]) & (big_tour_df$surface == "Hard"), na.rm = T) / 
    sum((abs(big_tour_df$Rank_Diff_Round) == bins[i]) & (big_tour_df$surface == "Hard"), na.rm = T)
}

big_tour_df = data.frame(bins = bins,
                         Prob = Prob,
                         Grass = Grass_Prob,
                         Clay = Clay_Prob,
                         Hard = Hard_Prob,
                         type = "Big Tournaments")

small_tour_df = df[df$draw_size == 32,]
bins = seq(10,200,10)
diff_df__nrow = length(bins)
Prob = Grass_Prob = Clay_Prob = Hard_Prob = numeric(diff_df__nrow)

for (i in 1:diff_df__nrow) {
  Prob[i] = sum(small_tour_df$Rank_Diff_Round == bins[i], na.rm = T) / sum(abs(small_tour_df$Rank_Diff_Round) == bins[i], na.rm = T)
  Grass_Prob[i] = sum((small_tour_df$Rank_Diff_Round == bins[i]) & (small_tour_df$surface == "Grass"), na.rm = T) / 
    sum((abs(small_tour_df$Rank_Diff_Round) == bins[i]) & (small_tour_df$surface == "Grass"), na.rm = T)
  Clay_Prob[i] = sum((small_tour_df$Rank_Diff_Round == bins[i]) & (small_tour_df$surface == "Clay"), na.rm = T) / 
    sum((abs(small_tour_df$Rank_Diff_Round) == bins[i]) & (small_tour_df$surface == "Clay"), na.rm = T)
  Hard_Prob[i] = sum((small_tour_df$Rank_Diff_Round == bins[i]) & (small_tour_df$surface == "Hard"), na.rm = T) / 
    sum((abs(small_tour_df$Rank_Diff_Round) == bins[i]) & (small_tour_df$surface == "Hard"), na.rm = T)
}

small_tour_df = data.frame(bins = bins,
                           Prob = Prob,
                           Grass = Grass_Prob,
                           Clay = Clay_Prob,
                           Hard = Hard_Prob,
                           type = "Small Tournaments")

rbind(big_tour_df,small_tour_df) %>% 
  ggplot(aes(x = bins,Prob,fill = type)) +
  geom_col(position = "dodge2") + 
  scale_y_continuous(limits=c(0.5,0.9),oob = rescale_none) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("Winning Probability") + xlab("Rank Difference") +
  labs(title = "Upsets: Big vs Small Tournaments") +
  theme(axis.title = element_text())

who_won = Vectorize(function(score,set_num){
  try({
    sets = str_split(score,pattern = " ",n = 3) %>% unlist()
    set_score = sets[set_num]
    set_score = str_split(set_score,pattern = "-") %>% unlist()
    w = as.double(set_score[1]); l = as.double(set_score[2])
    if (!is.na(w) & !is.na(l)) {
      if (w<l) {
        return(0)
      }else{
        return(1)
      }
    }
  },silent = F)
  return(-1)
})

df['1st_set'] = who_won(df$score,1)
df['2nd_set'] = who_won(df$score,2)
df['3rd_set'] = who_won(df$score,3)


winning_per_set = function(Rank_diff, dff, set_num){
  positive_diff_w = positive_diff_l = negative_diff_w = negative_diff_l =  numeric(0)
  for (rk in Rank_diff) {
    positive_diff_w = append(positive_diff_w,sum(dff["Rank_Diff_Round"] == rk & dff[set_num] == 1,na.rm = T))
    positive_diff_l = append(positive_diff_l,sum(dff["Rank_Diff_Round"] == rk & dff[set_num] == 0,na.rm = T))
    
    negative_diff_w = append(negative_diff_w,sum(dff["Rank_Diff_Round"] == -rk & dff[set_num] == 1,na.rm = T))
    negative_diff_l = append(negative_diff_l,sum(dff["Rank_Diff_Round"] == -rk & dff[set_num] == 0,na.rm = T))
  }
  w = positive_diff_w + negative_diff_l
  l = positive_diff_l + negative_diff_w
  return(w/(l + w))
}

bins = seq(10,200,10)
diff_df = data.frame(bins = bins)
diff_df["Set_1"] = winning_per_set(Rank_diff = bins, dff = df, set_num = "1st_set")
diff_df["Set_2"] = winning_per_set(Rank_diff = bins, dff = df, set_num = "2nd_set")
diff_df["Set_3"] = winning_per_set(Rank_diff = bins, dff = df, set_num = "3rd_set")


diff_df %>% 
  gather(key = "Set",value = "prob",-bins) %>% 
  ggplot(mapping = aes(x = bins,y = prob, col = Set,group=Set)) +
  geom_line(size = 1.3) + 
  scale_color_manual(values = c("red", "darkgreen","blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("Winning Probability") + xlab("Rank Difference") +
  labs(title = "Upsets in Different Sets") +
  theme(axis.title = element_text())

second_set = df[df$Sets == 2,]
bins = seq(10,200,10)
diff_df__nrow = length(bins)
Prob = Grass_Prob = Clay_Prob = Hard_Prob = numeric(diff_df__nrow)

for (i in 1:diff_df__nrow) {
  Prob[i] = sum(second_set$Rank_Diff_Round == bins[i], na.rm = T) / sum(abs(second_set$Rank_Diff_Round) == bins[i], na.rm = T)
  Grass_Prob[i] = sum((second_set$Rank_Diff_Round == bins[i]) & (second_set$surface == "Grass"), na.rm = T) / 
    sum((abs(second_set$Rank_Diff_Round) == bins[i]) & (second_set$surface == "Grass"), na.rm = T)
  Clay_Prob[i] = sum((second_set$Rank_Diff_Round == bins[i]) & (second_set$surface == "Clay"), na.rm = T) / 
    sum((abs(second_set$Rank_Diff_Round) == bins[i]) & (second_set$surface == "Clay"), na.rm = T)
  Hard_Prob[i] = sum((second_set$Rank_Diff_Round == bins[i]) & (second_set$surface == "Hard"), na.rm = T) / 
    sum((abs(second_set$Rank_Diff_Round) == bins[i]) & (second_set$surface == "Hard"), na.rm = T)
}

second_set = data.frame(bins = bins,
                         Prob = Prob,
                         Grass = Grass_Prob,
                         Clay = Clay_Prob,
                         Hard = Hard_Prob,
                         type = "Set2")

last_set = df[df$Sets == 3,]
bins = seq(10,200,10)
diff_df__nrow = length(bins)
Prob = Grass_Prob = Clay_Prob = Hard_Prob = numeric(diff_df__nrow)

for (i in 1:diff_df__nrow) {
  Prob[i] = sum(last_set$Rank_Diff_Round == bins[i], na.rm = T) / sum(abs(last_set$Rank_Diff_Round) == bins[i], na.rm = T)
  Grass_Prob[i] = sum((last_set$Rank_Diff_Round == bins[i]) & (last_set$surface == "Grass"), na.rm = T) / 
    sum((abs(last_set$Rank_Diff_Round) == bins[i]) & (last_set$surface == "Grass"), na.rm = T)
  Clay_Prob[i] = sum((last_set$Rank_Diff_Round == bins[i]) & (last_set$surface == "Clay"), na.rm = T) / 
    sum((abs(last_set$Rank_Diff_Round) == bins[i]) & (last_set$surface == "Clay"), na.rm = T)
  Hard_Prob[i] = sum((last_set$Rank_Diff_Round == bins[i]) & (last_set$surface == "Hard"), na.rm = T) / 
    sum((abs(last_set$Rank_Diff_Round) == bins[i]) & (last_set$surface == "Hard"), na.rm = T)
}

last_set = data.frame(bins = bins,
                           Prob = Prob,
                           Grass = Grass_Prob,
                           Clay = Clay_Prob,
                           Hard = Hard_Prob,
                           type = "Set3")

rbind(last_set, second_set) %>% 
  ggplot(aes(x = bins,y = Prob, fill = type)) +
  geom_col(position = "dodge2") + 
  scale_y_continuous(limits=c(0.5,0.9),oob = rescale_none) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("Winning Probability") + xlab("Rank Difference") +
  theme(axis.title = element_text())

df['Age_Diff'] = df$winner_age - df$loser_age

ggplot() +
  geom_density(data = df, mapping = aes(x = Age_Diff,col = "All Matches"),alpha = .4, size = 1.5) +
  geom_density(data = df[df$Sets==3,], mapping = aes(x = Age_Diff,col = "3rd Set"),alpha = .4, size = 1.5) +
  xlim(c(-15,15)) + 
  xlab("Age Difference") + ylab("") +
  labs(title = "Age advantage in the last set")+
  theme(axis.title = element_text())



ggplot() +
  geom_density(data = df, mapping = aes(x = Age_Diff,col = "All Matches"),alpha = .4, size = 1.5) +
  geom_density(data = df[df$round=="F",], mapping = aes(x = Age_Diff,col = "Finals"),alpha = .4, size = 1.5) +
  xlim(c(-15,15)) + 
  xlab("Age Difference") + ylab("") +
  labs(title = "Age advantage in the Finals")+
  theme(axis.title = element_text())


## Kolmogorov Smirmanov tests

third_set <- df$Age_Diff[df$Sets==3]
all_matched <- df$Age_Diff
finals <-df$Age_Diff[df$round == "F"]


# ks.test
ks.test(third_set, all_matched)
ks.test(all_matched, finals)
