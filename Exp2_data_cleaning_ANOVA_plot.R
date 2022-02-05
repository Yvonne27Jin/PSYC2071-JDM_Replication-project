### PSYC2071 Replication - Stimulated data cleaning, Experiment 2
### Author: Yvonne JIN
### Date: 11/13/2021



library(haven)
df <- read_sav("Newman et al. (2011) Experiments 1&2 R&E_NOCHECKS_November 13, 2021_02.05.sav")
View(df)


library(dplyr)
library(tidyverse)


#slicing dataframe into conditions

# Q47L individual sensitivity

# positive celebrity - Q37 - Q41
posi_celeb <- df %>% filter(is.na(Q37.2)==FALSE) %>%
  select(starts_with("Q37."),
         starts_with("Q38."),
         starts_with("Q39."),
         starts_with("Q40."),
         starts_with("Q41."),
         starts_with("Q47."),
         starts_with("Q49.")) %>%
  mutate(valence = "positive",
         manipulation_check1 = Q37.2,
         manipulation_check2 = Q37.3,
         pre_willing_buy = Q37.4,
         pre_pleasant = Q37.5,
         pre_market_value = Q37.6,
         pre_impressed = Q37.7,
         sensitivity = (Q47.2+Q47.3+Q47.4)/3)
  
# negative celebrity - Q42 - Q46
nega_celeb <- df %>% filter(is.na(Q42.2)==FALSE) %>%
  select(starts_with("Q42."),
         starts_with("Q43."),
         starts_with("Q44."),
         starts_with("Q45."),
         starts_with("Q46."),
         starts_with("Q47."),
         starts_with("Q49.")) %>%
  mutate(valence = "negative",
         manipulation_check1 = Q42.2,
         manipulation_check2 = Q42.3,
         pre_willing_buy = Q42.4,
         pre_pleasant = Q42.5,
         pre_market_value = Q42.6,
         pre_impressed = Q42.7,
         sensitivity = (Q47.2+Q47.3+Q47.4)/3)

# positive celebrity - Q37 - Q41
# Q38: contagion highlighted
posi_contagion_h <- posi_celeb %>% filter(is.na(Q38.1)==FALSE) %>%
  mutate(manipulation = "Contagion",
         direction = "Highlighted",
         post_willing_buy = Q38.1,
         post_pleasant = Q38.2,
         post_market_value = Q38.3,
         post_impressed = Q38.4)

# Q39: contagion decreased
posi_contagion_d <- posi_celeb %>% filter(is.na(Q39.1)==FALSE) %>%
  mutate(manipulation = "Contagion",
         direction = "Decreased",
         post_willing_buy = Q39.1,
         post_pleasant = Q39.2,
         post_market_value = Q39.3,
         post_impressed = Q39.4)

# Q40: market value highlighted
posi_value_h <- posi_celeb %>% filter(is.na(Q40.1)==FALSE) %>%
  mutate(manipulation = "Market Value",
         direction = "Highlighted",
         post_willing_buy = Q40.1,
         post_pleasant = Q40.2,
         post_market_value = Q40.3,
         post_impressed = Q40.4)

# Q41: market value decreased
posi_value_d <- posi_celeb %>% filter(is.na(Q41.1)==FALSE) %>%
  mutate(manipulation = "Market Value",
         direction = "Decreased",
         post_willing_buy = Q41.1,
         post_pleasant = Q41.2,
         post_market_value = Q41.3,
         post_impressed = Q41.4)

####-----------------negative celebrity---------

# Q43: contagion highlighted
nega_congation_h <- nega_celeb %>% filter(is.na(Q43.1)==FALSE) %>%
  mutate(manipulation = "Contagion",
         direction = "Highlighted",
         post_willing_buy = Q43.1,
         post_pleasant = Q43.2,
         post_market_value = Q43.3,
         post_impressed = Q43.4)
              

# Q44: contagion decreased
nega_congation_d <- nega_celeb %>% filter(is.na(Q44.1)==FALSE) %>%
  mutate(manipulation = "Contagion",
         direction = "Decreased",
         post_willing_buy = Q44.1,
         post_pleasant = Q44.2,
         post_market_value = Q44.3,
         post_impressed = Q44.4)

# Q45: market value highlighted
nega_value_h <- nega_celeb %>% filter(is.na(Q45.1)==FALSE) %>%
  mutate(manipulation = "Market Value",
         direction = "Highlighted",
         post_willing_buy = Q45.1,
         post_pleasant = Q45.2,
         post_market_value = Q45.3,
         post_impressed = Q45.4)

# Q46: market value decreased
nega_value_d <- nega_celeb %>% filter(is.na(Q46.1)==FALSE) %>%
  mutate(manipulation = "Market Value",
         direction = "Decreased",
         post_willing_buy = Q46.1,
         post_pleasant = Q46.2,
         post_market_value = Q46.3,
         post_impressed = Q46.4)


# append into 1 dataset
cleaned_df <- posi_contagion_h[27:46] %>% 
  rbind(posi_contagion_d[27:46]) %>%
  rbind(posi_value_h[27:46]) %>%
  rbind(posi_value_d[27:46]) %>%
  rbind(nega_congation_h[27:46]) %>%
  rbind(nega_congation_d[27:46]) %>%
  rbind(nega_value_h[27:46]) %>% 
  rbind(nega_value_d[27:46])

# recode variables: change between pre- and post- manipulation

colnames(cleaned_df)

cleaned_df <- cleaned_df %>% mutate(change_willing_buy = post_willing_buy - pre_willing_buy,
                                    change_pleasant = post_pleasant - pre_pleasant,
                                    change_market_value = post_market_value - pre_market_value,
                                    change_impressed = post_impressed - pre_impressed,
                                    change_extension_DV = (change_market_value + change_impressed)/2)

# export 
#write.csv(cleaned_df,"Exp2_cleaned_data.csv",fileEncoding = "UTF-8")

# export positive and negative celebrity seperately for analysis 
#posi_cleaned <- cleaned_df %>% filter(valence == "positive")
#write.csv(posi_cleaned,"Exp2_posi_cleaned.csv",fileEncoding = "UTF-8")

#nega_cleaned <- cleaned_df %>% filter(valence == "negative")
#write.csv(nega_cleaned,"Exp2_nega_cleaned.csv",fileEncoding = "UTF-8")


#summary(posi_cleaned)


#############################
# run ANOVA using JAMOVI code
library(jmv)

# DV1: change of purchase intention
jmv::ANOVA(
  formula = change_willing_buy ~ valence + manipulation + direction + valence:manipulation + valence:direction + manipulation:direction + valence:manipulation:direction,
  data = cleaned_df,
  effectSize = "eta",
  modelTest = TRUE,
  homo = TRUE,
  postHoc = ~ valence + manipulation:direction,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ valence:manipulation:direction,
  emmTables = TRUE)

# DV2: change of pleasantness of wearing the item
jmv::ANOVA(
  formula = change_pleasant ~ valence + manipulation + direction + valence:manipulation + valence:direction + manipulation:direction + valence:manipulation:direction,
  data = cleaned_df,
  effectSize = "eta",
  modelTest = TRUE,
  homo = TRUE,
  postHoc = ~ valence + manipulation:direction,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ valence:manipulation:direction,
  emmTables = TRUE)

# DV3: extention DVs (market value DV from Exp1)

jmv::ANOVA(
  formula = change_extension_DV ~ valence + manipulation + direction + valence:manipulation + valence:direction + manipulation:direction + valence:manipulation:direction,
  data = cleaned_df,
  effectSize = "eta",
  modelTest = TRUE,
  homo = TRUE,
  postHoc = ~ valence + manipulation:direction,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ valence:manipulation:direction,
  emmTables = TRUE)


## ggstatsplot for visualization

#try lapply?
jjstatsplot::jjbetweenstats(
  data = cleaned_df,
  #dep = vars(change_willing_buy, change_pleasant, change_extension_DV),
  group = manipulation,
  grvar = direction,
  pairwisedisplay = "everything",
  originaltheme = TRUE)