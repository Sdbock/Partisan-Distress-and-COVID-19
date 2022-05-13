### Author: Sean Bock
### Date: 4/28/21 **updated 5/13/22 
### Project: COVID and distress
### Note: This script loads cleaned data and runs models and creates figures


pacman::p_load(tidyverse, magrittr, gssr, ggthemes, broom, here, TAM)

# Loading data ------------------------------------------------------------

norc <- read_rds("data/norc_clean.rds")
gss <- read_rds("data/gss_clean.rds")


# Prepping data for figures ----

##transforming into long format for plotting

norc_long <-
  pivot_longer(norc,cols=c(starts_with("dep_")), names_to="outcomes")

## calulating means, SEs, and CIs across outcomes by yearXparty

means_norc <-
  norc_long %>%
  filter(party3 != "missing") %>%
  group_by(year,party3,outcomes) %>%
  summarize(mean = weighted.mean(value, na.rm = TRUE, w = weight),
            sd = weighted_sd(value, w = weight),
            count = n()) %>%
  mutate(se = sd / sqrt(count),
         lbound = mean - 1.96 * se,
         ubound = mean + 1.96 * se) 

## Now GSS

gss_long <-
  pivot_longer(gss,cols=c(starts_with("dep_")),names_to="outcomes")

means_gss <- 
  gss_long %>%
  filter(party3 != "missing",
         outcomes == "dep_happy" |
         year == 2018) %>%
  group_by(year, party3, outcomes) %>%
  summarize(mean = weighted.mean(value, na.rm = TRUE, w = weight),
            sd = weighted_sd(value, w = weight),
            count = n()) %>%
  mutate(se = sd / sqrt(count),
         lbound = mean - 1.96 * se,
         ubound = mean + 1.96 * se) 

##combinging gss and norc
means <- bind_rows(means_gss,means_norc)


# labeling outcomes
means$outcomes[means$outcomes=="dep_emoprobs"]<-"Emotional problems"
means$outcomes[means$outcomes=="dep_fatigue"]<-"Fatigue"
means$outcomes[means$outcomes=="dep_happy"]<-"General unhappiness"
means$outcomes[means$outcomes=="dep_mental"]<-"Mental health"
means$outcomes[means$outcomes=="dep_lonely"]<-"Loneliness scale"
means$outcomes[means$outcomes=="dep_quallife"]<-"Quality of life"




means_norc$outcomes[means_norc$outcomes=="dep_emoprobs"] <- "Emotional problems"
means_norc$outcomes[means_norc$outcomes=="dep_fatigue"] <- "Fatigue"
means_norc$outcomes[means_norc$outcomes=="dep_happy"] <- "General unhappiness"
means_norc$outcomes[means_norc$outcomes=="dep_mental"] <- "Mental health"
means_norc$outcomes[means_norc$outcomes=="dep_lonely"] <- "Loneliness scale"
means_norc$outcomes[means_norc$outcomes=="dep_quallife"] <- "Quality of life"
means_norc$outcomes[means_norc$outcomes=="dep_feel"] <- "Negative feelings scale"
means_norc$outcomes[means_norc$outcomes=="dep_stress"] <- "Stress scale"
means_norc$outcomes[means_norc$outcomes=="dep_covidreact"] <- "Covid negative reaction scale"

# Figures ------

## Fig 1: 2020 only figure ----


plot_2020 <- 
  means_norc %>%
  ggplot(aes(y = mean,x = year,color = party3)) + 
  theme_clean(base_size=15)+
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = lbound, ymax = ubound, fill = party3), alpha = .2, color = FALSE, show.legend = FALSE) +
  scale_color_manual(values=c("#016a8e","#a8a6a7","#b1283a")) +
  scale_fill_manual(values=c("#016a8e","#a8a6a7","#b1283a")) +
  facet_wrap(~ outcomes,scales="free_y",labeller=label_wrap_gen()) +
  labs(x = "Waves of 2020 COVID Response Tracking Study",
       y = expression("Greater distress" %->% ""),
       color = "Party ID",
       caption= "Note: Points indicate mean response by group with 95% confidence bands. Higher values = greater distress.") +
  scale_x_continuous(breaks=c(2020.2,2020.4,2020.6),labels=c("May","June", "July")) +
  theme(legend.position= "bottom",
        plot.background=element_rect(color = "white"),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 12, hjust = 0))

ggsave("Fig1.tif",device="tiff",plot_2020,height=8.5,width=11,path="figures/")

## Fig 3: Over-time distress -----

## plot including 2018 for select outcomes in top panel. Longer trends for 
## general unhappiness on bottom panel.
### top panel: trends with 2018 GSS data ----
plot_overtime <- 
  means %>%
  filter(year > 2016,
         !outcomes %in% c("dep_covidreact", "dep_feel", "dep_stress")) %>%
  ggplot(aes(y = mean, x = year, color = party3)) + 
  ggthemes::theme_clean(base_size = 15) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = lbound, ymax = ubound, fill = party3), alpha = .2, color = FALSE) +
  scale_color_manual(values=c("#016a8e","#a8a6a7","#b1283a")) +
  scale_fill_manual(values=c("#016a8e","#a8a6a7","#b1283a")) +
  facet_wrap(~ outcomes, scales="free_y", labeller=label_wrap_gen()) +
  labs(x = NULL,
       y = expression("Greater distress" %->% ""),
       color = "Party ID") +
  theme(legend.position="") +
  geom_vline(xintercept=2020.19,linetype="dashed") +
  theme(plot.background=element_rect(color="white"))

### bottom panel: Longer trends with happiness outcomes. GAM used to fit
### nonlinear trends in unhappiness by party ---
### 

library(tidymv)

## need combined wide data for modeling
parties_happy <-
  gss %>%
  bind_rows(norc) %>%
  select(party3, dep_happy, year, weight)

## creating new data frame for predictions
newdata = expand_grid(party3 = c("Democrat","Independent","Republican"), year = seq(1972,2020.6,.4))

## fitting gam
gam_fits <-
  parties_happy %>%
  filter(!is.na(party3)) %>%
  mgcv::gam(dep_happy ~ party3 + s(year, bs = "cs", by = party3),
            data = .,
            gamma = 4,
            weights = weight,
            methods = "REML"
  ) %>%
  augment(newdata = newdata)

plot_overtimehappy <-
  gam_fits %>%
  ggplot(aes(x = year, y = .fitted, color = party3, fill = party3)) +
  theme_clean(base_size=15) +
  geom_point(data = means %>% 
               filter(outcomes == "General unhappiness"), aes(x = year, y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = .fitted - 1.96 * .se.fit, ymax = .fitted + 1.96 * .se.fit), 
              alpha = .30, 
              show.legend = FALSE, 
              color = FALSE) +
  scale_color_manual(values = c("#016a8e","#a8a6a7","#b1283a")) +
  scale_fill_manual(values = c("#016a8e","#a8a6a7","#b1283a")) +
  guides(
    fill = "none"
  ) +
  theme(legend.position="bottom") +
  geom_vline(xintercept=2020.19,linetype="dashed") +
  labs(color = "Party ID",
       y = expression("Greater distress" %->% ""),
       x = NULL,
       title = "GAM smoothed trends in unhappiness by party, 1972-2020"
  ) +
  theme(plot.background=element_rect(color="white"),
        plot.title = element_text(face = "plain"))

# Combining panels
library(patchwork)

plot3 <- 
plot_overtime / plot_overtimehappy +
  plot_annotation(
    caption =
      str_wrap("Note: Points indicate mean response by group in each year. Higher values = greater distress. Dashed line indicates beginning of pandemic. Confidence bands demonstrate uncertainty at the 95% level. Smoothed trends (bottom panel) are predicted from a Generalized Additive Model (GAM) with restricted maximum likelihood estimation.", width = 90),
    theme = 
      theme(plot.caption = element_text(hjust = 0, size =12))
  ) 
  
ggsave("Fig3.tif", device = "tiff", plot3, height=11, width=8.5, path="figures/")



## Fig 4: Over-time trends by sample ----
 
### Top panel: party gap by race -----

#### party gap overall ----

# calculating gap between Dems and Reps with ttest for differences
party_gap <-
  means %>%
  select(year, party3, outcomes, mean, sd, count) %>%
  filter(party3 != "Independent") %>%
  pivot_wider(names_from = party3, values_from = c(mean,sd,count)) %>%
  mutate(mean_gap = mean_Democrat - mean_Republican,
         se = sqrt((sd_Democrat^2/count_Democrat) + (sd_Republican^2/count_Republican)),
         lbound = mean_gap - 1.96 * se,
         ubound = mean_gap + 1.96 * se
  ) %>%
  rename(
    "gap" = mean_gap
  ) %>%
  select(year, outcomes, gap, lbound, ubound) %>%
  mutate(sample = "Full")


#### party gap for non-Hispanic whites only ------

means_norc_race <- 
  norc_long %>%
  filter(party3 != "missing") %>%
  group_by(year,party3,con_race,outcomes) %>% ###using non-hispanic whites here
  summarize(mean = weighted.mean(value, na.rm = TRUE, w = weight),
            sd = weighted_sd(value, w = weight),
            count = n()) %>%
  mutate(se = sd / sqrt(count),
         lbound = mean - 1.96 * se,
         ubound = mean + 1.96 * se) 


gss_long <-
  gss_long %>%
  mutate(nonhispanic = if_else(hispanic == 1,1,0)) ## "hispanic" variable in GSS indicates type of hispanic ID if > 1

gss_long_r <- # recoding race variable in gss to match 2020 data
  gss_long %>% 
  mutate(con_race = case_when(
    con_race3 == "White" & nonhispanic==1 ~ "White",
    con_race3 == "White" & nonhispanic==0 ~ "Hispanic",
    con_race3 == "Black" ~ "Black",
    con_race3 == "Other" ~ "Other"
  ))



means_gss_white <- 
  gss_long_r %>%
  filter(party3 != "missing",
         year == 2018,
         con_race == "White") %>%
  group_by(year,party3,con_race,outcomes) %>% 
  summarize(mean = weighted.mean(value, na.rm = TRUE, w = weight),
            sd = weighted_sd(value, w = weight),
            count = n()) %>%
  mutate(se = sd / sqrt(count),
         lbound = mean - 1.96 * se,
         ubound = mean + 1.96 * se) 


means_norc_white <-
  means_norc_race %>%
  filter(con_race == "White")

means_white <- bind_rows(means_gss_white,means_norc_white)

means_white$outcomes[means_white$outcomes=="dep_emoprobs"] <- "Emotional problems"
means_white$outcomes[means_white$outcomes=="dep_fatigue"] <- "Fatigue"
means_white$outcomes[means_white$outcomes=="dep_happy"] <- "General unhappiness"
means_white$outcomes[means_white$outcomes=="dep_mental"] <- "Mental health"
means_white$outcomes[means_white$outcomes=="dep_lonely"] <- "Loneliness scale"
means_white$outcomes[means_white$outcomes=="dep_quallife"] <- "Quality of life"


### party gap non-hipsanic whites only -----

party_gap_white <- 
  means_white %>%
  ungroup() %>%
  select(year, party3, outcomes, mean, sd, count) %>%
  filter(party3 != "Independent") %>%
  group_by(outcomes, year) %>%
  pivot_wider(names_from = party3, values_from = c(mean, sd, count)) %>%
  mutate(mean_gap = mean_Democrat - mean_Republican,
         se = sqrt((sd_Democrat^2/count_Democrat) + (sd_Republican^2/count_Republican)),
         lbound = mean_gap - 1.96 * se,
         ubound = mean_gap + 1.96 * se
  ) %>%
  rename(
    "gap" = mean_gap
  ) %>%
  select(year, outcomes, gap, lbound, ubound) %>%
  mutate(sample = "Non-Hispanic whites only")


# Combining party gap estimates

party_gap_combined <- 
  bind_rows(party_gap,party_gap_white)

### party gaps plot ----
plot_gaps <- 
  party_gap_combined %>%
  filter(
    outcomes != "dep_feel",
    outcomes != "dep_stress",
    outcomes != "dep_covidreact",
    year > 2016) %>%
  ggplot(aes(y = gap, x = year, color = sample, linetype = sample, fill = sample)) + 
  theme_clean(base_size = 15)+
  geom_point() + 
  geom_line(size = 1.2) +
  guides(fill = "none",
         linetype = "none") +
  scale_color_wsj(palette = "rgby") +
  scale_fill_wsj(palette = "rgby") +
  facet_wrap(~outcomes,scales="free_y",labeller=label_wrap_gen()) +
  labs(color = "Sample",
       x = NULL,
       y = "Democrats - Republicans") +
  theme(legend.position="bottom")+
  geom_vline(xintercept=2020.19,linetype="dashed")+
  theme(plot.background=element_rect(color="white"))

### Bottom panel: Trends in Dems happiness by race

dems_race_gss <- 
  gss %>% 
  mutate(nonhispanic = if_else(hispanic == 1,1,0)) %>%
  mutate(con_race = case_when(
    con_race3 == "White" & nonhispanic==1 ~ "White",
    con_race3 == "White" & nonhispanic==0 ~ "Hispanic",
    con_race3 == "Black" ~ "Black",
    con_race3 == "Other" ~ "Other"
  ),
  con_race_split = case_when(  ## new variable that splits ethnicity for whites at 2000
    con_race == "White" ~ "White",
    con_race3 == "White" & year < 2000 ~ "White",
    con_race == "Black" ~ "Black",
    con_race == "Other" ~ 'Other'
  )) %>%
  filter(party3 == "Democrat") %>%
  select(year, dep_happy, con_race_split, weight) %>%
  rename(con_race = con_race_split ) ## renaming to match NORC race variable



dems_race_norc <-
  norc %>%
  filter(party3 == "Democrat") %>%
  select(year, dep_happy, con_race, weight) 

dems_race <- bind_rows(dems_race_gss, dems_race_norc) %>%
  mutate(con_race = factor(con_race))

## getting means for plot 


dems_race_means <-
  dems_race %>%
  group_by(year, con_race) %>%
  summarize(mean = weighted.mean(dep_happy, weight = weight, na.rm = TRUE))

# fitting GAM models

newdata_race = expand_grid(con_race = c("White","Black"), year = seq(1972,2020.6,.4))
newdata_race2 = expand_grid(con_race = c("White","Black"), year = seq(2000,2020.6,.4))



gam_dems_race <-
  dems_race %>%
  filter(con_race %in% c("White", "Black")) %>%
  mgcv::gam(dep_happy ~ con_race + s(year, bs = "cs", by = con_race),
            data = .,
            gamma = 1.5,
            weights = weight,
            methods = "REML"
  ) %>%
  augment(newdata = newdata_race)



gam_dems_race_pre20 <-
  dems_race %>%
  filter(con_race %in% c("White", "Black"),
         year < 2020) %>%
  mgcv::gam(dep_happy ~ con_race + s(year, bs = "cs", by = con_race),
            data = .,
            gamma = 1.5,
            weights = weight,
            methods = "REML"
  ) %>%
  augment(newdata = newdata_race)


# plotting GAM model

plot_dems_race <- 
  gam_dems_race %>%
  ggplot(aes(y=.fitted, x=year, color = con_race, fill = con_race)) + 
  theme_clean(base_size=15)+
  geom_point(data = dems_race_means %>% 
               filter(con_race %in% c("White","Black")),
             aes(x = year, y = mean, alpha = .4),
             show.legend = FALSE) + 
  guides(fill = "none") +
  geom_line() +
  geom_ribbon(aes(ymin = .fitted - 1.96 * .se.fit, ymax = .fitted + 1.96 * .se.fit), 
              alpha = .30, 
              show.legend = FALSE, 
              color = FALSE) +
  geom_line(data = gam_dems_race_pre20, 
            linetype = "dotted",
            size = 1.5,
            show.legend = FALSE) +
  scale_color_manual(values=c("orange","dark green")) +
  scale_fill_manual(values=c("orange","dark green")) +
  xlab("") + 
  ylab(expression("Greater unhappiness" %->% ""))+
  labs(color="Race",
       linetype="Race") +
  theme(legend.position="bottom") +
  geom_vline(xintercept=2020.19,linetype="dashed") +
  ggrepel::geom_text_repel(data = gam_dems_race_pre20 %>% filter(con_race == "Black", year > 2020),
                           aes(x = year, y = .fitted), 
                           label = str_wrap("Dotted trend lines indicate forecasted unhappiness without 2020 data",width = 20),
                           color = "black",
                           inherit.aes = FALSE,
                           nudge_x = 8,
                           min.segment.length = 0,
                           size = 6) +
  ggtitle("Democratic trends in unhappiness by race, 1974-2020") +
  labs(caption = str_wrap("Note: Higher values = greater distress. Dashed line indicates beginning of pandemic. 95% confidence bands demonstrate uncertainty in locally weighted smoothed trends (bottom).",100))+
  theme(plot.background=element_rect(color="white"))

### Combining panels -----

plot4 <-
  plot_gaps / plot_dems_race +
  plot_annotation(
    caption =
      str_wrap("Note: Because the GSS did not ask respondents about ethnicity until 2000, 'white' include both Hispanic and non-Hispanic whites before 2000. Higher values = greater distress. Dashed line indicates beginning of pandemic. Confidence bands demonstrate uncertainty at the 95% level. Smoothed trends (bottom panel) are predicted from a Generalized Additive Model (GAM) with restricted maximum likelihood estimation.", width = 90),
    theme = 
      theme(plot.caption = element_text(hjust = 0, size = 12))
  ) 

ggsave("Fig4.tif",device="tiff",plot4,height=11,width=8.5,path="figures/")

## Fig 2: OLS coefficient plots ------

# WARNING: ugly code below. This could all be done much more efficiently, but 
# leaving brute force solution for now.


### Wave 1 ----

model_baseline_w1 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress))~ party3,
                        data = norc %>% filter(year==2020.2))
model_baseline_w1 <- tidy(model_baseline_w1) %>%
  mutate(model="Baseline")


model_controls_w1 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress))~ party3 + 
                         con_educ + con_income + con_employed + con_married + con_race + con_region + con_age,
                       data = norc %>% filter(year==2020.2))
model_controls_w1 <- tidy(model_controls_w1) %>%
  mutate(model="Baseline + Soc dem controls")


model_controls_covid_w1 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress)) ~
                                party3 + 
                                con_educ + con_income + con_employed + con_married + con_race + con_region + con_age +
                                con_info + con_hotpot + con_exposed + con_familydiag + con_econimpact + con_familyimpact,
                              data = norc %>% filter(year==2020.2))

model_controls_covid_w1 <- tidy(model_controls_covid_w1) %>%
  mutate(model = "Baseline + Soc dem controls + COVID impact")

models_w1 <- bind_rows(model_baseline_w1,model_controls_w1,model_controls_covid_w1) %>%
  mutate(wave="Wave 1")

### Wave 2 ----
model_baseline_w2 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress))~ party3,
                        data = norc %>% filter(year==2020.4))
model_baseline_w2 <- tidy(model_baseline_w2) %>%
  mutate(model="Baseline")


model_controls_w2 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress))~ party3 + 
                          con_educ + con_income + con_employed + con_married + con_race + con_region + con_age,
                        data = norc %>% filter(year==2020.4))
model_controls_w2 <- tidy(model_controls_w2)%>%
  mutate(model="Baseline + Soc dem controls")


model_controls_covid_w2 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress)) ~
                                party3 + 
                                con_educ + con_income + con_employed + con_married + con_race + con_region + con_age +
                                con_info + con_hotpot + con_exposed + con_familydiag + con_econimpact + con_familyimpact,
                              data = norc %>% filter(year==2020.4))

model_controls_covid_w2 <- tidy(model_controls_covid_w2) %>%
  mutate(model="Baseline + Soc dem controls + COVID impact")

models_w2 <- bind_rows(model_baseline_w2,model_controls_w2,model_controls_covid_w2)%>%
  mutate(wave="Wave 2")

### Wave 3 -----

model_baseline_w3 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress))~ party3,
                        data = norc %>% filter(year==2020.6))
model_baseline_w3 <- tidy(model_baseline_w3) %>%
  mutate(model="Baseline")


model_controls_w3 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress))~ party3 + 
                          con_educ + con_income + con_employed + con_married + con_race + con_region + con_age,
                        data = norc %>% filter(year==2020.6))
model_controls_w3 <- tidy(model_controls_w3)%>%
  mutate(model = "Baseline + Soc dem controls")


model_controls_covid_w3 <- lm(cbind(scale(dep_emoprobs),scale(dep_fatigue),scale(dep_happy),scale(dep_mental),scale(dep_lonely),scale(dep_quallife),scale(dep_feel),scale(dep_stress)) ~
                                party3 + 
                                con_educ + con_income + con_employed + con_married + con_race + con_region + con_age +
                                con_info + con_hotpot + con_exposed + con_familydiag + con_econimpact + con_familyimpact,
                              data=norc %>% filter(year==2020.6))

model_controls_covid_w3 <- tidy(model_controls_covid_w3) %>%
  mutate(model="Baseline + Soc dem controls + COVID impact")

models_w3 <- bind_rows(model_baseline_w3,model_controls_w3,model_controls_covid_w3)%>%
  mutate(wave="Wave 3")

## combining all estimates
models <- bind_rows(models_w1,models_w2,models_w3)

### Labeling vars ------

models$response[models$response=="scale(dep_emoprobs)"]<-"Emotional problems"
models$response[models$response=="scale(dep_fatigue)"]<-"Fatigue"
models$response[models$response=="scale(dep_happy)"]<-"General unhappiness"
models$response[models$response=="scale(dep_mental)"]<-"Mental Health"
models$response[models$response=="scale(dep_lonely)"]<-"Loneliness scale"
models$response[models$response=="scale(dep_quallife)"]<-"Quality of life"
models$response[models$response=="scale(dep_feel)"]<-"Negative feelings scale"
models$response[models$response=="scale(dep_stress)"]<-"Stress scale"



models$response[models$response=="dep_emoprobs"]<-"Emotional problems"
models$response[models$response=="dep_fatigue"]<-"Fatigue"
models$response[models$response=="dep_happy"]<-"General unhappiness"
models$response[models$response=="dep_mental"]<-"Mental Health"
models$response[models$response=="dep_lonely"]<-"Loneliness scale"
models$response[models$response=="dep_quallife"]<-"Quality of life"
models$response[models$response=="dep_feel"]<-"Negative feelings scale"
models$response[models$response=="dep_stress"]<-"Stress scale"

models$term[models$term=="party3Independent"]<-"Independent"
models$term[models$term=="party3Republican"]<-"Republican"
models$term[models$term=="con_educSome college"]<-"Some college"
models$term[models$term=="con_educBA or more"]<-"BA or more"
models$term[models$term=="con_income$50,000 to under $100,00"]<-"$50,000 to under $100,00"
models$term[models$term=="con_income$100,000 or more" ]<-"$100,000 or more"
models$term[models$term=="con_employedEmployed"  ]<-"Employed"
models$term[models$term=="con_marriedMarried/co-habitating"  ]<-"Married/co-habitating"
models$term[models$term=="con_raceAfrican American"]<-"African American"
models$term[models$term=="con_raceHispanic"]<-"Hispanic"
models$term[models$term=="con_raceOther"]<-"Other"
models$term[models$term=="con_regionMidwest"]<-"Midwest"
models$term[models$term=="con_regionSouth"]<-"South"
models$term[models$term=="con_regionWest"]<-"West"
models$term[models$term=="con_age30-39"]<-"30-39"
models$term[models$term=="con_age40-59"]<-"40-59"
models$term[models$term== "con_age60-64"]<-"60-64"
models$term[models$term== "con_age65 or older"]<-"65 or older"
models$term[models$term== "con_infoRarely"]<-"COVID-19 info: Rarely"
models$term[models$term== "con_infoOccasionally"]<-"COVID-19 info: Occasionally"
models$term[models$term== "con_infoOften"]<-"COVID-19 info: Often"
models$term[models$term== "con_infoMost of the time"]<-"COVID-19 info: Most of the time"
models$term[models$term== "con_hotpot"]<-"Live in hotspot"
models$term[models$term== "con_exposedYes"]<-"Exposed to COVID-19"
models$term[models$term== "con_familydiagYes"]<-"Family member diagnosed"
models$term[models$term== "con_econimpact"]<-"Impacted economically"
models$term[models$term== "con_familyimpact"]<-"Family impact scale"


plot_coefs <- 
  models %>%
  filter(term== "Republican")%>%
  mutate(order=row_number())%>%
  ggplot(aes(x=term,y=estimate,color=reorder(model,-order))) + 
  theme_clean(base_size = 15) +
  geom_hline(yintercept = 0,linetype="dashed")+
  geom_point(position = position_dodge(width = .7), size = .5) +
  geom_pointrange(aes(ymin = estimate - 1.96 * std.error, ymax=estimate + 1.96 * std.error), position = position_dodge(width = .7))+  
  coord_flip()+ 
  scale_color_wsj(palette="rgby")+
  facet_grid(~response ~wave ,
             scales="free_x",labeller = labeller(response = label_wrap_gen(width = 1)))+
  labs(caption=str_wrap("Note: Data from NORC COVID Response Tracking Study.
                        Positive estimates indicate more distress for Republicans 
                        compared to Democrats. Outcomes are standardized in order 
                        to compare association sizes between estimates.",79),
       color="Model",
       x = NULL,
       y = "OLS coefficients for Republicans (vs. Democrats)")+
  guides(color = guide_legend(reverse=TRUE))+
  theme(legend.position="bottom",
        plot.background=element_rect(color="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10),
        legend.spacing = unit(1, "cm"))


ggsave("Fig2.tif",device="tiff",plot_coefs,height=11,width=8.5,path="figures/")

