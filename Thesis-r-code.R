getwd()
setwd("/Users/trishakershaw/Documents/THESIS/R Files")
getwd()

Canada_dataset <- read.csv("CanadaReplicationData.csv")

install.packages("readr")
install.packages("AER")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("GGally") # nice correlation plots
install.packages("arsenal") # helpful for categorical variables summary stats
install.packages("car") # vif for MC
install.packages("lmtest") # has function bptest (heteroscedasticity) & coeftest
install.packages("tidyverse")
install.packages("extrafont")
library(extrafont)
library(AER)
library(stargazer)
library(ggplot2)
library(GGally)
library(lmtest)
library (arsenal)
library(car)
library("readr")
library("tidyverse")

## Aesthetic indicators and setting plot theme##

partiesilike <- c("Bloc Québécois","Conservative Party", "Green Party", 
                  "Liberal Party", "New Democratic Party", 
                  "People's Party")

party_colours <- c("Bloc Québécois" = "skyblue", 
                   "Conservative Party" = "blue", 
                   "Green Party" = "darkgreen", 
                   "Liberal Party" = "red", 
                   "New Democratic Party" = "darkorange", 
                   "People's Party" = "purple", 
                   "All" = "black")

prov_colours<-c(c("AB" = "red", 
                  "BC" = "orange", 
                  "ON" = "blue", 
                  "QC" = "darkgreen", 
                  "SK" = "purple"))

font_import() #only once
loadfonts()
fonts()

theme_set(theme_minimal() +
            theme(text = element_text(family = "Lato", size = 12)))


#specify certain variables for analysis
thesis_unclean <- Canada_dataset [,c ("responseid","wave", "prov_clean","ideology",
                                      "genderBIN", "age3", "frenchBIN", "bachelorsBIN", "edu5",
                                      "income6","income.NUM","trust.FED","wave1_party",
                                      "wave3_party","wave4_party","support_cp","oppose_cp",
                                      "strongoppose_cp")]

#took all na's out of support_cp, oppose_cp, and strongoppose_cp
thesis<-thesis_unclean[!is.na(thesis_unclean$support_cp),]
thesis <- thesis[!is.na(thesis$oppose_cp),]
thesis <- thesis[!is.na(thesis$strongoppose_cp),]

#First lets ensure consistency of Party Names and Full Names for clarity

thesis<-thesis |> mutate(wave1_party=case_when(wave1_party=="ndp"~"New Democratic Party", 
                                               .default = wave1_party))|>
  mutate(wave3_party=case_when(wave3_party=="ndp"~"New Democratic Party",
                               .default = wave3_party)) |>
  mutate(wave4_party=case_when(wave4_party=="Conservatives"~"Conservative Party",
                                       wave4_party=="Liberals"~"Liberal Party", 
                               wave4_party=="ndp"~"New Democratic Party",
                                       wave4_party=="People's Party of Canada"~"People's Party", 
                                       .default = wave4_party)) 

#Lets create a new variable, 'cp_attitude' which turns support, oppose, and strongly oppose into a 4-point scale
#if they selected 0 for all three, we made them a 0 (they seem indifferent) 
#if they selected 1 for both oppose and strong oppose, we made them -2 (they seem very opposed)

#Lets also turn edu5 and age3 into a numerical variable for future regression analyses

thesis<-thesis |> mutate(cp_attitude=case_when(support_cp=="1"~1,
                            support_cp=="0" & oppose_cp=="0" & strongoppose_cp=="0"~0,   
                             oppose_cp=="1" & strongoppose_cp=="0"~-1,
                            oppose_cp=="1" & strongoppose_cp=="1"~-2,
                             oppose_cp=="0" & strongoppose_cp=="1"~-2)) |>
  mutate(edu_NUM=case_when(edu5=="Less than high school"~0, edu5=="High school"~1,
                           edu5=="Some college"~2, edu5=="College"~3, 
                           edu5=="Graduate or prof. degree"~4)) |>
  mutate(age_NUM=case_when(age3=="18-34"~1, age3=="35-54"~2,
                           age3=="55 and older"~3))

#Table 2: Create a chart of descriptive statistics of all the variables

stargazer(thesis, title = "Table 2: Descriptive Statistics", type = "html", out="Table2final.htm")

# Let's make a new categorical variable - party, assign each observation as party
# Party for waves 1 and 2, is what they put for wave1_party, wave3_party for wave 3, wave4_party for waves 4 and 5
# This is because respondents were not asked their parties in waves 2 and 5

thesis$party<-ifelse(thesis$wave=="wave1"| thesis$wave=="wave2", thesis$wave1_party,"NA")
thesis$party<-ifelse(thesis$wave=="wave3", thesis$wave3_party, thesis$party)
thesis$party<-ifelse(thesis$wave=="wave4" | thesis$wave=="wave5", thesis$wave4_party, thesis$party)

#check the correlations of the numeric variables of interest

Correlations_unclean<-thesis[,c ("ideology","genderBIN", "frenchBIN", "bachelorsBIN",
                                 "income.NUM", "edu_NUM", "age_NUM", "trust.FED",
                                 "cp_attitude")]
correlations<-na.omit(Correlations_unclean)

#Table 3: Correlations Chart
CorrChart <- cor(correlations)

stargazer(CorrChart, type = "html", out = "correlations.htm",
          title="Table 3: Correlations of Numeric Variables")

# Figure 1: Plotting the Trust in Government by Political Ideology #
# We see that the more right wing a person is, the less trust they have in the feds

trust<- thesis%>%group_by(ideology)%>%summarise(mean_trust = mean(trust.FED, na.rm = TRUE))

ggplot(trust, aes(x = ideology, y = mean_trust)) +
  geom_col(fill="darkcyan") +
  labs(x = "Political Ideology", y = "Trust in Government") +
  scale_x_continuous(labels = c("Far Left", "Left", "Center", "Right", "Far Right"), 
                     breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  theme_minimal()+
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                      "inches"))

# Figure 2: Plotting the Trust in Government by Parties #
trust2<- thesis%>%group_by(party)%>%summarise(mean_trust_party = mean(trust.FED, na.rm = TRUE))

trust2 <-filter(trust2, party %in% partiesilike) 

ggplot(trust2, aes(x = party, y = mean_trust_party, fill=party)) +
  geom_col() +
  geom_text(aes(label = round(mean_trust_party, 2)), vjust = -0.5, size = 3, 
            family = "Lato") +
  labs(x = "Political Party", y = "Trust in Government") +
  scale_fill_manual(values = party_colours, name = "Political Party") + 
  guides(fill = FALSE) +
  theme_minimal()+
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                       "inches"))

#Figure 3: Plotting ideology by political party

ideology <- thesis%>%group_by(party)%>%summarise(mean_ideology = mean(ideology,na.rm = TRUE))|> 
  filter(party %in% partiesilike)

ggplot(ideology, aes(x = party, y = mean_ideology, fill=party)) +
  geom_col() +
  geom_text(aes(label = round(mean_ideology, 2)), vjust = -0.5, size = 3,
            family = "Lato") +
  labs(x = "Political Party", y = "Political Ideology") +
  scale_fill_manual(values = party_colours, name = "Political Party") + 
  guides(fill = FALSE) +
  theme_minimal()+
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                       "inches"))
  
# Next, plot the change in Attitude of Carbon Pricing over the waves

attitude<- thesis%>%group_by(wave)%>%summarise(mean_attitude = mean(cp_attitude)) %>%
  mutate(party = "All")

attitude1<-thesis%>%group_by(party, wave)%>%summarise(mean_attitude = mean(cp_attitude))|>
  filter(party %in% partiesilike)

attitude <- rbind(attitude1, attitude)

#FIGURE 4: CP Attitude over the waves, by party and all

ggplot(attitude, aes(x = wave, y = mean_attitude, color = party)) +
  geom_point() +
  geom_line(aes(group=party)) +   
  labs(x = "Wave", y = "Carbon Pricing Attitude") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
  scale_color_manual(values = c(party_colours), name = "Political Party") +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                       "inches"))

## for the appendix
attitude_rearranged <- attitude %>%
  pivot_wider(names_from = wave, values_from = mean_attitude)

write_csv(attitude_rearranged, "AttitudeByPartyByWave.csv")

#Figure 5: Facet grid the parties to better see the changes over waves

ggplot(attitude1, aes(x=wave, y=mean_attitude, fill=party)) + 
  facet_wrap(party ~ .) +
  geom_col() +
  labs(x = "Wave", y = "Carbon Pricing Attitude") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"), 
                   breaks = c("wave1", "wave2", "wave3", "wave4", "wave5")) +
  scale_y_continuous(labels = c("-2", "-1", "0", "1"), 
                   breaks = c(-2, -1, 0, 1)) +
  scale_fill_manual(values = party_colours, name = "Political Party") +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                       "inches"))

# FIGURE 6: Find the mean of cp_attitude by wave and party in this consistent voter dataset

##create dataset w only consistent voters

consistent <- thesis |> group_by(responseid)|>
  filter(wave1_party==wave3_party & wave1_party==wave4_party) |> 
  pivot_longer(cols=c(wave1_party, wave3_party, wave4_party), values_to="party.con", 
               names_to = "party_wave") |>
  filter(party %in% c(partiesilike))

mean_consistent <- consistent %>% 
  group_by(wave, party.con) %>% 
  summarise(mean_att = mean(as.numeric(as.character(cp_attitude)), na.rm = TRUE))

ggplot(mean_consistent, aes(x=wave, y=mean_att, fill=party.con)) + 
  facet_wrap(party.con ~ .) +
  geom_col() +
  labs(x = "Wave", y = "Carbon Pricing Attitude") +
  scale_fill_manual(values = party_colours, name = "Political Party") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"), 
                   breaks = c("wave1", "wave2", "wave3", "wave4", "wave5")) +
  scale_y_continuous(labels = c("-2", "-1", "0", "1"), 
                     breaks = c(-2, -1, 0, 1)) +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                       "inches"))

## appendix

consistent_rearranged <- mean_consistent %>%
  pivot_wider(names_from = wave, values_from = mean_att)

write_csv(consistent_rearranged, "ConsistentByPartyByWave.csv")

## Figure 7: Map Attitude by Province ##
#Maybe there is a provincial element we are missing

attitude2<-thesis%>%group_by(prov_clean, wave)%>%summarise(mean_attitude = mean(cp_attitude))

ggplot(attitude2, aes(x = wave, y = mean_attitude, color = prov_clean)) +
  geom_point() +
  geom_line(aes(group = prov_clean)) +
  labs(x = "Wave", y = "Carbon Pricing Attitude") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"), 
                   breaks = c("wave1", "wave2", "wave3", "wave4", "wave5")) +
  scale_color_manual(values = prov_colours, name = "Provinces") +
  guides(guide_legend(override.aes = list(size = 2))) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))

## FIGURE 8: Look at difference between rebate and non-rebate provinces ##
#BC and QC don't have rebate = 0, ON and SK do have it = 1, AB is NA

rebate <- thesis |>
  mutate(rebateBIN = ifelse(prov_clean %in% c("BC", "QC"), 0, 
                         ifelse(prov_clean %in% c("ON", "SK"), 1, NA))) |>
  group_by(rebateBIN, wave) |>
  summarise(mean_cp_attitude = mean(cp_attitude, na.rm = TRUE))

rebate<-na.omit(rebate)

ggplot(rebate, aes(x = wave, y = mean_cp_attitude, color = factor(rebateBIN), group=rebateBIN)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = c(1.3), linetype = "dashed") + 
  geom_vline(xintercept = c(2.5), linetype = "solid") + 
  labs(x = "Wave", y = "Carbon Pricing Attitude", color = "Provinces") +
  scale_color_manual(values = c("red", "green"), labels = c("BC & QC (Non-Rebate)", 
                                                            "ON & SK (Rebate)")) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"), 
                   breaks = c("wave1", "wave2", "wave3", "wave4", "wave5")) +
  theme_minimal()+
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches")
  )

## Figure 9: Let's look at attitude by political ideology ##

attitude3<- thesis%>%group_by(ideology)%>%summarise(mean_attitude= mean(cp_attitude))

ggplot(attitude3, aes(x = ideology, y = mean_attitude)) +
  geom_col(fill="darkcyan") +
  labs(x = "Political Ideology", y = "Carbon Pricing Attitude") +
  scale_x_continuous(labels = c("Far Left", "Left", "Center", "Right", "Far Right"), 
                     breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                       "inches"))

### Time for some regressions ###

thesis_lm <- filter(thesis, party %in% partiesilike)

thesis_lm <- thesis_lm |>
  mutate(party=case_when(party=="Bloc Québécois"~0,party=="New Democratic Party"~1, 
                         party=="Green Party"~2, party=="Liberal Party"~3, 
                         party== "Conservative Party"~4, party=="People's Party"~5)) |>
  mutate(prov_clean=case_when(prov_clean=="AB" ~ 0, prov_clean=="BC" ~ 1, prov_clean=="ON" ~ 2, 
                              prov_clean=="SK" ~ 3, prov_clean=="QC" ~ 4))|>
  mutate(rebateBIN = ifelse(prov_clean %in% c(1,4), 0, 
                            ifelse(prov_clean %in% c(2, 3), 1, NA))) 

## Model for Attitude = ideology + trust + party + province + rebate
lm_ideology1<-lm(cp_attitude~ideology, data=thesis_lm)
lm_ideology2<-lm(cp_attitude~ideology+trust.FED, data=thesis_lm)
lm_ideology3<-lm(cp_attitude~ideology+trust.FED+party, data=thesis_lm)
lm_ideology4<-lm(cp_attitude~ideology+trust.FED+party+prov_clean, data=thesis_lm)
lm_ideology5<-lm(cp_attitude~ideology+trust.FED+party+prov_clean+rebateBIN, data=thesis_lm)

stargazer(lm_ideology2, lm_ideology3, lm_ideology4, lm_ideology5, 
          title="Linear Regression Models of Carbon Pricing Attitudes",
          type = "html", 
          out="ideology_regression.htm")

### APPENDIX ###

## distribution of party preferences 
party_prop <- thesis %>%
  filter(party %in% partiesilike) %>%
  group_by(wave, party) %>%
  summarise(num_voters = n()) %>%
  mutate(prop_voters = num_voters / sum(num_voters)) %>%
  mutate(percentage = num_voters /sum(num_voters) * 100)

write_csv(party_prop, "PartyDistribution.csv")

# Create the bar chart
ggplot(party_prop, aes(x = wave, y = percentage, fill = party)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Wave", y = "Percentage") +
  scale_fill_manual(values = party_colours, name = "Political Party") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"), 
                   breaks = c("wave1", "wave2", "wave3", "wave4", "wave5")) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))

## distribution of observations in ideology

ggplot(thesis, aes(x = ideology)) +
  geom_histogram(binwidth = 0.1, fill = "darkcyan", color = "black") +
  labs(x = "Political Ideology", y = "Frequency") +
  theme_minimal()+  
    theme(
      text = element_text(family = "Lato", size = 10),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))

ideology_table<- as.data.frame(table(thesis$ideology))
write_csv(ideology_table, "IdeologyDistribution.csv")

## appendix regressions ##

lm_gender <- lm(cp_attitude~genderBIN, data=thesis)
lm_french <-lm(cp_attitude~frenchBIN, data=thesis)
lm_bachelors<-lm(cp_attitude~bachelorsBIN, data=thesis)
lm_income<-lm(cp_attitude~income.NUM, data=thesis)
lm_age <-lm(cp_attitude~age_NUM, data = thesis)
lm_edu<-lm(cp_attitude~edu_NUM, data=thesis)

# income is not very practical,The model explains only a very small proportion of the variability in cp_attitude

# Create summary of all the models into 2 charts

stargazer(lm_gender, lm_french, lm_bachelors, 
          title = "Linear Regression Models - Dummy Variables", type = "html", out = "appendex_regressions.htm")

stargazer( lm_income, lm_edu, lm_age,
          title = "Linear Regression Models - Ordinal Variables", type = "html", out = "appendex_regressions2.htm")

## Stats Checks for Model 4 ##
plot(lm_ideology4)

#lets do VIF check 
vif<-vif(lm_ideology4)

par(mfrow = c(2, 2))

# Residuals vs Fitted plot
plot(lm_ideology4, which = 1)

# Normal Q-Q plot
plot(lm_ideology4, which = 2)

# Scale-Location (sqrt(|residuals|)) vs Fitted plot
plot(lm_ideology4, which = 3)

# Residuals vs Leverage plot
plot(lm_ideology4, which = 5)


