### PSYC2071 Replication - Stimulated data cleaning, Experiment 1
### Author: Yvonne JIN
### Date: 11/13/2021



library(haven)
df <- read_sav("Newman et al. (2011) Experiments 1&2 R&E_NOCHECKS_November 13, 2021_02.05.sav")
View(df)
summary(df)


library(dplyr)
library(tidyverse)

#slicing dataframe into conditions
# positive celebrity - Q1- Q6
posi_celeb <- df %>% filter(is.na(Q1.2)==FALSE) %>%
  select(starts_with("Q1."),
         starts_with("Q2."),
         starts_with("Q3."),
         starts_with("Q4."),
         starts_with("Q5."),
         starts_with("Q6."),
         starts_with("Q49.")) #demographic data
# positive non-celebrity - Q7- Q12
posi_nonceleb <- df %>% filter(is.na(Q7.2)==FALSE) %>%
  select(starts_with("Q7."),
         starts_with("Q8."),
         starts_with("Q9."),
         starts_with("Q10."),
         starts_with("Q11."),
         starts_with("Q12."),
         starts_with("Q49."))
# negative celebrity - Q13-Q18
nega_celeb <- df %>% filter(is.na(Q13.2)==FALSE) %>%
  select(starts_with("Q13."),
         starts_with("Q14."),
         starts_with("Q15."),
         starts_with("Q16."),
         starts_with("Q17."),
         starts_with("Q18."),
         starts_with("Q49."))
# negative non-celebrity - Q19 - Q24
nega_nonceleb <- df %>% filter(is.na(Q19.2)==FALSE) %>%
  select(starts_with("Q19."),
         starts_with("Q20."),
         starts_with("Q21."),
         starts_with("Q22."),
         starts_with("Q23."),
         starts_with("Q24."),
         starts_with("Q49."))
# mixed valence celebrity - Q25 - Q30
mixed_celeb <- df %>% filter(is.na(Q25.2)==FALSE) %>%
  select(starts_with("Q25."),
         starts_with("Q26."),
         starts_with("Q27."),
         starts_with("Q28."),
         starts_with("Q29."),
         starts_with("Q30."),
         starts_with("Q49."))
# mixed valence non celebrity - Q31 - Q36
mixed_nonceleb <- df %>% filter(is.na(Q31.2)==FALSE) %>%
  select(starts_with("Q31."),
         starts_with("Q32."),
         starts_with("Q33."),
         starts_with("Q34."),
         starts_with("Q35."),
         starts_with("Q36."),
         starts_with("Q49."))


#take average score across three trials for each participant
# (automatically changed variable type of calculated index)

#positive celebrity - Q1-Q6
posi_celeb <- mutate(posi_celeb, 
                     con = "posi_celeb",
                     fame = "celeb",
                     valence = "positive",
                     manipulation_check = (Q1.2+ Q3.2+ Q5.2)/3,
                     person_liking = ( Q1.3+ Q3.3+ Q5.3)/3,
                     person_meet = ( Q1.4+ Q3.4+ Q5.4)/3,
                     person_hug = ( Q1.5+ Q3.5+ Q5.5)/3,
                     item_own = ( Q2.2+ Q4.2+ Q6.2)/3,
                     item_purchase = ( Q2.3+ Q4.3+ Q6.3)/3,
                     item_keep = ( Q2.4+ Q4.4+ Q6.4)/3,
                     item_hold_in_hand = ( Q2.5+ Q4.5+ Q6.5)/3,
                     item_market_value = (Q2.6+ Q4.6+ Q6.6)/3,
                     item_impressed = (Q2.7+ Q4.7+ Q6.7)/3,
                     item_historial_value = (Q2.8+ Q4.8+ Q6.8)/3) 

# positive non-celebrity - Q7- Q12
posi_nonceleb <- mutate(posi_nonceleb, 
                        con = "posi_nonceleb",
                        fame = "nonceleb",
                        valence = "positive",
                     manipulation_check = (Q7.2+ Q9.2+ Q11.2)/3,
                     person_liking = ( Q7.3+ Q9.3+ Q11.3)/3,
                     person_meet = ( Q7.4+ Q9.4+ Q11.4)/3,
                     person_hug = ( Q7.5+ Q9.5+ Q11.5)/3,
                     item_own = ( Q8.2+ Q10.2+ Q12.2)/3,
                     item_purchase = ( Q8.3+ Q10.3+ Q12.3)/3,
                     item_keep = ( Q8.4+ Q10.4+ Q12.4)/3,
                     item_hold_in_hand = ( Q8.5+ Q10.5+ Q12.5)/3,
                     item_market_value = (Q8.6+ Q10.6+ Q12.6)/3,
                     item_impressed = (Q8.7+ Q10.7+ Q12.7)/3,
                     item_historial_value = (Q8.8+ Q10.8+ Q12.8)/3)

# negative celebrity - Q13-Q18
nega_celeb <- mutate(nega_celeb, 
                     con = "nega_celeb",
                     fame = "celeb",
                     valence = "negative",
                        manipulation_check = (Q13.2+ Q15.2+ Q17.2)/3,
                        person_liking = ( Q13.3+ Q15.3+ Q17.3)/3,
                        person_meet = ( Q13.4+ Q15.4+ Q17.4)/3,
                        person_hug = ( Q13.5+ Q15.5+ Q17.5)/3,
                        item_own = ( Q14.2+ Q16.2+ Q18.2)/3,
                        item_purchase = ( Q14.3+ Q16.3+ Q18.3)/3,
                        item_keep = ( Q14.4+ Q16.4+ Q18.4)/3,
                        item_hold_in_hand = ( Q14.5+ Q16.5+ Q18.5)/3,
                        item_market_value = (Q14.6+ Q16.6+ Q18.6)/3,
                        item_impressed = (Q14.7+ Q16.7+ Q18.7)/3,
                        item_historial_value = (Q14.8+ Q16.8+ Q18.8)/3)

# negative non-celebrity - Q19 - Q24
nega_nonceleb <- mutate(nega_nonceleb, 
                        con = "nega_nonceleb",
                        fame = "nonceleb",
                        valence = "negative",
                     manipulation_check = (Q19.2+ Q21.2+ Q23.2)/3,
                     person_liking = ( Q19.3+ Q21.3+ Q23.3)/3,
                     person_meet = ( Q19.4+ Q21.4+ Q23.4)/3,
                     person_hug = ( Q19.5+ Q21.5+ Q23.5)/3,
                     item_own = ( Q20.2+ Q22.2+ Q24.2)/3,
                     item_purchase = ( Q20.3+ Q22.3+ Q24.3)/3,
                     item_keep = ( Q20.4+ Q22.4+ Q24.4)/3,
                     item_hold_in_hand = ( Q20.5+ Q22.5+ Q24.5)/3,
                     item_market_value = (Q20.6+ Q22.6+ Q24.6)/3,
                     item_impressed = (Q20.7+ Q22.7+ Q24.7)/3,
                     item_historial_value = (Q20.8+ Q22.8+ Q24.8)/3)

# mixed valence celebrity - Q25 - Q30
mixed_celeb <- mutate(mixed_celeb, 
                      con = "mixed_celeb",
                      fame = "celeb",
                      valence = "mixed",
                     manipulation_check = (Q25.2+ Q27.2+ Q29.2)/3,
                     person_liking = ( Q25.3+ Q27.3+ Q29.3)/3,
                     person_meet = ( Q25.4+ Q27.4+ Q29.4)/3,
                     person_hug = ( Q25.5+ Q27.5+ Q29.5)/3,
                     item_own = ( Q26.2+ Q28.2+ Q30.2)/3,
                     item_purchase = ( Q26.3+ Q28.3+ Q30.3)/3,
                     item_keep = ( Q26.4+ Q28.4+ Q30.4)/3,
                     item_hold_in_hand = ( Q26.5+ Q28.5+ Q30.5)/3,
                     item_market_value = (Q26.6+ Q28.6+ Q30.6)/3,
                     item_impressed = (Q26.7+ Q28.7+ Q30.7)/3,
                     item_historial_value = (Q26.8+ Q28.8+ Q30.8)/3)

# mixed valence non celebrity - Q31 - Q36
mixed_nonceleb <- mutate(mixed_nonceleb, 
                        con = "mixed_nonceleb", 
                        fame = "nonceleb",
                        valence = "mixed",
                     manipulation_check = (Q31.2+ Q33.2+ Q35.2)/3,
                     person_liking = ( Q31.3+ Q33.3+ Q35.3)/3,
                     person_meet = ( Q31.4+ Q33.4+ Q35.4)/3,
                     person_hug = ( Q31.5+ Q33.5+ Q35.5)/3,
                     item_own = ( Q32.2+ Q34.2+ Q36.2)/3,
                     item_purchase = ( Q32.3+ Q34.3+ Q36.3)/3,
                     item_keep = ( Q32.4+ Q34.4+ Q36.4)/3,
                     item_hold_in_hand = ( Q32.5+ Q34.5+ Q36.5)/3,
                     item_market_value = (Q32.6+ Q34.6+ Q36.6)/3,
                     item_impressed = (Q32.7+ Q34.7+ Q36.7)/3,
                     item_historial_value = (Q32.8+ Q34.8+ Q36.8)/3)

# append 6 conditions into 1 cleaned dataset
# take only the averaged variables
cleaned_df <- posi_celeb[37:56] %>% 
  rbind(posi_nonceleb[37:56]) %>%
  rbind(nega_celeb[37:56]) %>%
  rbind(nega_nonceleb[37:56]) %>%
  rbind(mixed_celeb[37:56]) %>%
  rbind(mixed_nonceleb[37:56])

# regroup questions into DVs
colnames(cleaned_df)
cleaned_df <- cleaned_df %>% mutate(Object_valuation = (item_own + item_purchase + item_keep)/3,
                                        Contagion = (person_hug + person_liking)/2,
                                        Market_value = (item_market_value + item_impressed)/2,
                                        Historical_sig = item_historial_value,
                                        Liking = person_liking,
                                        Extension_contagion = person_meet)


# export data

#write.csv(cleaned_df,"Exp1_cleaned_data.csv",fileEncoding = "UTF-8")
 
###############################
# run ANOVA using JAMOVI code

install.packages("jmv")

library(jmv)

# first DV: Item value
# two-way ANOVA with hypothesis check of homogeneity, post hoc analysis, table and graph of means
jmv::ANOVA(
  formula = Object_valuation ~ fame + valence + fame:valence,
  data = cleaned_df,
  effectSize = "eta",
  homo = TRUE,
  postHoc = ~ fame + valence + fame:valence,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ fame + valence + fame:valence,
  emmTables = TRUE)



# DV : contagion

jmv::ANOVA(
  formula = Contagion ~ fame + valence + fame:valence,
  data = cleaned_df,
  effectSize = "eta",
  homo = TRUE,
  postHoc = ~ fame + valence + fame:valence,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ fame + valence + fame:valence,
  emmTables = TRUE)

# Extension DV: contagion

jmv::ANOVA(
  formula = Extension_contagion ~ fame + valence + fame:valence,
  data = cleaned_df,
  effectSize = "eta",
  homo = TRUE,
  postHoc = ~ fame + valence + fame:valence,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ fame + valence + fame:valence,
  emmTables = TRUE)


# DV: market value

jmv::ANOVA(
  formula = Market_value ~ fame + valence + fame:valence,
  data = cleaned_df,
  effectSize = "eta",
  homo = TRUE,
  postHoc = ~ fame + valence + fame:valence,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ fame + valence + fame:valence,
  emmTables = TRUE)


# DV: historical value
jmv::ANOVA(
  formula = Historical_sig ~ fame + valence + fame:valence,
  data = cleaned_df,
  effectSize = "eta",
  homo = TRUE,
  postHoc = ~ fame + valence + fame:valence,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ fame + valence + fame:valence,
  emmTables = TRUE)

# DV: liking

jmv::ANOVA(
  formula = Liking ~ fame + valence + fame:valence,
  data = cleaned_df,
  effectSize = "eta",
  homo = TRUE,
  postHoc = ~ fame + valence + fame:valence,
  postHocES = "d",
  postHocEsCi = TRUE,
  emMeans = ~ fame + valence + fame:valence,
  emmTables = TRUE)


## ggstatplot for visualization

install.packages("ggstatsplot")
library(ggstatsplot)

ggstatsplot::ggbetweenstats(
  data = cleaned_df,
  y = Object_valuation,
  x = con,
  pairwisedisplay = "everything",
  padjustmethod = "bonferroni",
  originaltheme = TRUE)

#try lapply?
ggstatsplot::ggbetweenstats(
  data = cleaned_df,
  #  y = c(Object_valuation, Contagion, Market_value, Historical_sig, Liking, Extension_contagion),
  x = con,
  pairwisedisplay = "everything",
  padjustmethod = "bonferroni",
  originaltheme = TRUE)


