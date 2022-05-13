### Author: Sean Bock
### Date: 4/21/21
### Project: COVID and distress
### Note: This script cleans and prepares data for analyses


pacman::p_load(tidyverse, magrittr, dplyr, haven, gssr, forcats, ggthemes, broom, here)

# Loading data ------------------------------------------------------------

data_full <- read_dta(here("data/cumulative/nsf_longitudinal.dta"))

##loading in data as .dta files because it's easier to handle numeric outcomes rather than strings
data1 <- read_dta(here("data/wave1/norc_wave1.dta"))
data2 <- read_dta(here("data/wave2/norc_wave2.dta"))
data3 <- read_dta(here("data/wave3/norc_wave3.dta"))

names(data1) <- tolower(names(data1))
names(data2) <- tolower(names(data2))
names(data3) <- tolower(names(data3))

# adding numeric year with equally spaced month values

data1_r <- data1 %>% mutate(year = 2020.2) %>% select(caseid,weight,p_partyi,year,feela:feelj,stress1:stress4,happy,emoprobs,fatigue,hlthmntl,lonely1,lonely2,lonely3,quallife,edu_r3,inc_r3,employed,married,raceth,region4,age_grou,starts_with("q25_"),starts_with("q20_"),starts_with("q21"),econi_17,starts_with("react"),info,hotspot)
data2_r <- data2 %>% mutate(year = 2020.4) %>% select(caseid,weight,p_partyi,year,feela:feelj,stress1:stress4,happy,emoprobs,fatigue,hlthmntl,lonely1,lonely2,lonely3,quallife,edu_r3,inc_r3,employed,married,raceth,region4,age_grou,starts_with("q25_"),starts_with("q20_"),starts_with("q21"),econimpa,starts_with("react"),info,hotspot)
data3_r <- data3 %>% mutate(year = 2020.6) %>% select(caseid,weight,p_partyi,year,feela:feelj,stress1:stress4,happy,emoprobs,fatigue,hlthmntl,lonely1,lonely2,lonely3,quallife,edu_r3,inc_r3,employed,married,raceth,region4,age_grou,starts_with("q25_"),starts_with("q20_"),starts_with("q21"),econimpa,starts_with("react"),info,hotspot)


###Some weird coding error with the econimc impact measures in wave 1. recoding to match waves 2 and 3
data1_r %<>% rename(
  econimpa = econi_17,
)

# binding waves together
data_r <- bind_rows(data1_r,data2_r,data3_r)

# cleaning NORC data ------------------------------------------------------

## cleaning key variables and outcomes ----
## 
data_r$partyid <- factor(data_r$p_partyi,levels=c(1,2,3,4,5,6,7),labels=c("Strong Democrat","Democrat","Leans Democrat","Independent","Leans Republican","Republican","Strong Republican"))

data_r$party3 <- fct_collapse(data_r$partyid,
                           Republican = c("Strong Republican", "Republican", "Leans Republican"),
                           Independent = c("Independent"),
                           Democrat = c("Strong Democrat", "Democrat","Leans Democrat"))


data_r  %<>% ##transforming all outcomes to numeric
  as_tibble() %>%
  mutate(across(c(feela:feelj,stress1:stress4,happy,emoprobs,fatigue,hlthmntl,lonely1,lonely2,lonely3,quallife,starts_with("react")), as.numeric))

data_r  %<>%    ##marking anything above value of 5 as missing
  mutate(across(c(feela:feelj,stress1:stress4,happy,emoprobs,fatigue,hlthmntl,lonely1,lonely2,lonely3,quallife,starts_with("react")),~ ifelse(. > 5,NA,.)))

### recoding feel and stress variables to be in consistent order: 1 indicates distress

data_r %<>%  ##correcting order variables, transforming to 1=distress 0=not
  mutate(across(c(feela,feelc,feele,feeli),~ (. - 1)))

data_r %<>%  ##correcting order variables, transforming to 1=distress 0=not
  mutate(across(c(feelb,feeld,feelf,feelg,feelh,feelj), ~ ((. - 2) * -1)))

data_r %<>%  ##correct order variables, transforming to 5 = most distressed
  mutate(across(c(stress2,stress3), ~ (6 - .)))


### covid reaction outcomes 
data_r %<>%  ##correcting order variables, transforming to 1=distress 0=not
  mutate(across(c(react1a,react1c,react1d,react1e,react1f,react1g,react1h,react1l,react1j,react1k,react1l,react1m,react1n,react1o),~ ((. - 2)*-1)))



##creating composite measures
data_r %<>% rowwise() %>% mutate(covidreact = sum(react1a,react1c,react1d,react1e,react1f,react1g,react1h,react1l,react1j,react1k,react1l,react1m,react1n,react1o))
data_r %<>% rowwise() %>% mutate(feel_scale = sum(feela,feelb,feelc,feeld,feele,feelf,feelg,feelh,feeli,feelj))
data_r %<>% rowwise() %>% mutate(lonely_scale = sum(lonely1,lonely2,lonely3))
data_r %<>% rowwise() %>% mutate(stress_scale = sum(stress1,stress2,stress3,stress4))

## adding dep_ prefix to outcomes
data_r %<>%
  rename(
    dep_feel = feel_scale,
    dep_lonely = lonely_scale,
    dep_stress = stress_scale,
    dep_happy = happy,
    dep_emoprobs = emoprobs,
    dep_fatigue = fatigue,
    dep_mental = hlthmntl,
    dep_quallife = quallife,
    dep_covidreact = covidreact
  )

## cleaning control variables ----
data_r  %<>% 
  as_tibble() %>%  ##marking anything above value of 5 as missing
  mutate(across(c(edu_r3,inc_r3,employed,married,raceth,region4,age_grou,info,starts_with("react"),econimpa,starts_with("q21"),starts_with("econimpact")), ~ ifelse(. > 50,NA,.)))

data_r$edu_r3 <- factor(data_r$edu_r3,levels=c(1,2,3),labels=c("HS or less","Some college","BA or more"))
data_r$inc_r3 <- factor(data_r$inc_r3,levels=c(1,2,3),labels=c("Less than $50,000","$50,000 to under $100,00","$100,000 or more"))
data_r$employed <- factor(data_r$employed,levels=c(2,1),labels=c("Not employed","Employed"))
data_r$married <- factor(data_r$married,levels=c(2,1),labels=c("Not married","Married/co-habitating"))
data_r$raceth <- factor(data_r$raceth,levels=c(1,2,3,4),labels=c("White","Black","Hispanic","Other"))
data_r$race3 <- fct_other(data_r$raceth,keep=c("White","Black"))
data_r$region4 <- factor(data_r$region4,levels=c(1,2,3,4),labels=c("Northeast","Midwest","South","West"))
data_r$age_grou <- factor(data_r$age_grou,levels=c(1,2,3,4,5),labels=c("18-29","30-39","40-59","60-64","65 or older"))
data_r$hotspot <- factor(data_r$hotspot,levels=c(0,1),labels=c("Not a hotspot","Hotspot")) 
data_r$info <- factor(data_r$info,levels=c(1,2,3,4,5),labels=c("Never","Rarely","Occasionally","Often","Most of the time"))
data_r$exposed <- factor(data_r$q25_4,levels=c(1,0),labels=c("No","Yes")) #creating exposure variable based on final q25 option of "no,  not to my knowledge"
data_r$family_diag <- factor(data_r$q20_3,levels=c(1,0),labels=c("No","Yes")) #creating exposure variable based on final q25 option of "no,  not to my knowledge"
data_r$econ_impact <- factor(data_r$econimpa,levels=c(2,1),labels=c("No","Yes")) #creating exposure variable based on final q25 option of "no,  not to my knowledge"

# creating family impact scale
data_r %<>%  ##correcting order variables, transforming to 1=yes 0 = no
  mutate(across(c(q21a,q21b,q21c,q21d,q21e,q21f,q21g), ~ ((3 - .) - 1)))

data_r %<>% rowwise() %>% mutate(family_impact = sum(q21a,q21b,q21c,q21d,q21e,q21f,q21g))


##renaming controls to have "con_" prefix
data_r %<>%
  rename(con_educ = edu_r3,
         con_income = inc_r3,
         con_employed = employed,
         con_married = married,
         con_race = raceth,
         con_race3 = race3,
         con_region = region4,
         con_age = age_grou,
         con_hotpot = hotspot,
         con_exposed = exposed,
         con_info = info,
         con_familydiag = family_diag,
         con_familyimpact = family_impact,
         con_econimpact = econ_impact
         )

norc_clean <- data_r %>%
  select(caseid,weight,year,party3,starts_with("dep_"),starts_with("con_"))


#data_happy$happy <- factor(data_happy$happy,levels=c(1,2,3),labels=c("Very happy","Pretty happy","Not too happy"))

# happy_data<-data_happy%>%
#   filter(!is.na(happy),
#          !is.na(party3))%>%
#   group_by(party3,year,happy)%>%
#   summarize(n=n())%>%
#   mutate(prop=n/sum(n))
         

# GSS ---------------------------------------------------------------------


data(gss_all) ### Loads gss data

gss <- gss_all%>%
  select(wtss,year,partyid,hlthmntl,happy,emoprobs,quallife,fatigue,lonely1,lonely2,lonely3, race,hispanic)

gss$con_race3 <- factor(gss$race,levels=c(1,2,3),labels=c("White","Black","Other"))

## GSS outcomes -----
gss$happy <- factor(gss$happy,levels=c(1,2,3), labels=c("Very happy","Pretty happy","Not too happy"))

#gss %<>% mutate_at(vars(4:11),~ifelse(. == 8 | .== 9,NA,.)) #Apparenlty they're already coded correctly?

gss %<>% 
  as_tibble() %>% ##transforming all outcomes to numeric
  mutate(across(4:11, as.numeric))

gss %<>% 
  rowwise() %>% 
  mutate(lonely_scale = sum(c(lonely1,lonely2,lonely3),na.rm=TRUE))

gss$lonely_scale[gss$lonely_scale==0] <- NA

gss$partyid <- factor(gss$partyid,levels=c(0,1,2,3,4,5,6,7,8,9),labels=c("Strong Democrat","Democrat","Leans Democrat","Independent","Leans Republican","Republican","Strong Republican","Other party","Don't know","No answer"))

gss$party3 <- fct_collapse(gss$partyid,
                           missing= c("No answer", "Don't know","Other party"),
                           Republican = c("Strong Republican", "Republican", "Leans Republican"),
                           Independent = c("Independent"),
                           Democrat = c("Strong Democrat", "Democrat","Leans Democrat"))

gss$party3[gss$party3=="missing"] <- NA

gss %<>% rename(
    dep_lonely = lonely_scale,
    dep_happy = happy,
    dep_emoprobs = emoprobs,
    dep_fatigue = fatigue,
    dep_mental = hlthmntl,
    dep_quallife = quallife,
    weight = wtss
  )


gss_clean <- 
  gss %>%
  select(weight,year,party3,starts_with("dep_"),starts_with("con_"),hispanic)

#Saving cleaned data ----

write_rds(norc_clean,file="data/norc_clean.rds")
write_rds(gss_clean,file="data/gss_clean.rds")




