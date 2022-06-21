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


split_set = str_split(df$score,pattern = " ",n = 3)
df['Set_1'] = split_set %>% map_chr(~.x[1])
df['Set_2'] = split_set %>% map_chr(~.x[2])
df['Set_3'] = split_set %>% map_chr(~.x[3])


comeback = 0
for (row in 1:dim(df)[1]) {
  if (!is.na(df[row,"Set_2"])) {
    if (str_count(df[row,"Set_2"],"R") == 0) {
      if (!is.na(df[row,"Set_3"])) {
        if ((str_count(df[row,"Set_3"],"R") == 0) &
            (str_count(df[row,"Set_3"],"u") == 0) &
            (df[row,"Set_3"] != '6-0 6-1') &
            (str_count(df[row,"Set_3"],"D") == 0)
        ) {
          set_score_Set_2 = str_replace(string = df[row,"Set_2"],pattern = "\\(\\d+\\)",replacement = " ")
          set_score_Set_3 = str_replace(string = df[row,"Set_3"],pattern = "\\(\\d+\\)",replacement = " ")
          Set_2 = str_split(set_score_Set_2,pattern = "-") %>% unlist()
          Set_2 = as.double(Set_2[1]) - as.double(Set_2[2])
          Set_3 = str_split(set_score_Set_3,pattern = "-") %>% unlist()
          Set_3 = as.double(Set_3[1]) - as.double(Set_3[2])
          if (!is.na(Set_3) & !is.na(Set_2)) {
            if (Set_3*Set_2 > 0) {
              comeback = comeback + 1
            }
          }
        }
      }
    }
  }
}

paste0('Comeback % = ', (100*comeback/(dim(df)[1])))


unique_surfaces = unique(df$surface)
y = numeric(0); x = character(0)

for (surf in unique_surfaces) {
  if (!is.na(surf)) {
    if (surf!="") {
      df_ = df %>% 
        filter(surface == surf)
      
      comeback = 0
      for (row in 1:dim(df_)[1]) {
        if (!is.na(df_[row,"Set_2"])) {
          if (str_count(df_[row,"Set_2"],"R") == 0) {
            if (!is.na(df_[row,"Set_3"])) {
              if ((str_count(df_[row,"Set_3"],"R") == 0) &
                  (str_count(df_[row,"Set_3"],"u") == 0) &
                  (df_[row,"Set_3"] != '6-0 6-1') &
                  (str_count(df_[row,"Set_3"],"D") == 0)
              ) {
                set_score_Set_2 = str_replace(string = df_[row,"Set_2"],pattern = "\\(\\d+\\)",replacement = " ")
                set_score_Set_3 = str_replace(string = df_[row,"Set_3"],pattern = "\\(\\d+\\)",replacement = " ")
                Set_2 = str_split(set_score_Set_2,pattern = "-") %>% unlist()
                Set_2 = as.double(Set_2[1]) - as.double(Set_2[2])
                Set_3 = str_split(set_score_Set_3,pattern = "-") %>% unlist()
                Set_3 = as.double(Set_3[1]) - as.double(Set_3[2])
                if (!is.na(Set_3) & !is.na(Set_2)) {
                  if (Set_3*Set_2 > 0) {
                    comeback = comeback + 1
                  }
                }
              }
            }
          }
        }
      }
      x = append(x,surf)
      y = append(y,(100*comeback/(dim(df_)[1])))
    }
  }
}

Comeback_bysurface = data.frame(surface = x,Comeback = y)

Comeback_bysurface %>% 
  ggplot(aes(surface,Comeback)) +
  geom_bar(stat = "identity", fill = c('Red','Green','Blue')) +
  ylab("Comeback Percentage") + xlab("Surface") +
  theme_classic()

avg_wins = df %>% 
  group_by(winner_name) %>% 
  count() %>% 
 filter(!is.na(winner_name), winner_name != "")
avg_wins = mean(avg_wins$n)

print(paste0("Avg_wins per player = ", avg_wins))


player_group = unique(df$winner_name)
no_of_wins = numeric(0); comeback_by_name = numeric(0); x = character(0)

for (nm in player_group) {
  if (!is.na(nm)) {
    if (nm!="") {
      df_ = df %>%
        filter(winner_name == nm)
      if (dim(df_)[1] > avg_wins) {
        comeback = 0
        for (row in 1:dim(df_)[1]) {
          if (!is.na(df_[row,"Set_2"])) {
            if (str_count(df_[row,"Set_2"],"R") == 0) {
              if (!is.na(df_[row,"Set_3"])) {
                if ((str_count(df_[row,"Set_3"],"R") == 0) &
                    (str_count(df_[row,"Set_3"],"u") == 0) &
                    (df_[row,"Set_3"] != '6-0 6-1') &
                    (str_count(df_[row,"Set_3"],"D") == 0)
                ) {
                  set_score_Set_2 = str_replace(string = df_[row,"Set_2"],pattern = "\\(\\d+\\)",replacement = " ")
                  set_score_Set_3 = str_replace(string = df_[row,"Set_3"],pattern = "\\(\\d+\\)",replacement = " ")
                  Set_2 = str_split(set_score_Set_2,pattern = "-") %>% unlist()
                  Set_2 = as.double(Set_2[1]) - as.double(Set_2[2])
                  Set_3 = str_split(set_score_Set_3,pattern = "-") %>% unlist()
                  Set_3 = as.double(Set_3[1]) - as.double(Set_3[2])
                  if (!is.na(Set_3) & !is.na(Set_2)) {
                    if (Set_3*Set_2 > 0) {
                      comeback = comeback + 1
                    }
                  }
                }
              }
            }
          }
        }
        x = append(x,nm)
        comeback_by_name = append(comeback_by_name,(100*comeback/(dim(df_)[1])))
        no_of_wins = append(no_of_wins,dim(df_)[1])
      }
    }
  }
}

#comeback queens
player_comback_nwin = data.frame(winner_name = x,
                                 no_of_wins = no_of_wins,
                                 comeback = comeback_by_name) %>% 
  arrange(desc(comeback))
 

player_comback_nwin %>% 
  ggplot(aes(x = comeback)) +
 geom_histogram(fill = "blue") +
 ylab("Occurences") + 
 xlab("Comeback %") +
 theme_classic()


player_comback_nwin %>% 
  ggplot(aes(y = comeback, x = no_of_wins)) +
  geom_point(col = "blue", size = 2, alpha = .4) +
  ylab("Comeback %") + 
  xlab("# wins") +
  theme_classic()

