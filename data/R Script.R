##### The Social Identities of Biden and Trump Sympathizers ######
##### A Multilevel-Analysis of the Candidate Preference in the U.S. Presidential Election 2020 #####

setwd("") #set path of 'Code and Data' folder as working directory

####IMPORTING PACKAGES####
packages <- c("extrafont", "remotes", "tidyverse", "foreign", "lme4", 
              "sjstats", "psych", "lpSolve", 
              "misty", "lattice", "sjPlot", 
              "readxl", "survey", "gtsummary", "questionr",
              "Hmisc", "ggplot2", "usmap",
              "mapproj", "rgdal", "maptools", "rgeos",
              "rcompanion", "crosstable", "effects",
              "margins", "lmtest", "sjmisc", "interactions",
              "performance")

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

####IMPORTING data_master Dataset####

data_master <- read.csv("data_master.csv")

#####RECODING ALL RELEVANT VARIABLES#####
data_master <- data_master %>%
  mutate(biden_thermometer = V201151,
         biden_thermometer = na_if(biden_thermometer, -9),
         biden_thermometer = na_if(biden_thermometer, -4),
         biden_thermometer = na_if(biden_thermometer, 998),
         trump_thermometer = V201152,
         trump_thermometer = na_if(trump_thermometer, -9),
         white_identity = V202499x,
         white_identity = na_if(white_identity, -9),
         white_identity = na_if(white_identity, -7),
         white_identity = na_if(white_identity, -6),
         white_identity = na_if(white_identity, -5),
         white_identity = na_if(white_identity, -1),
         white_identity = case_when(white_identity == 1 ~ 5,
                                    white_identity == 2 ~ 4,
                                    white_identity == 3 ~ 3,
                                    white_identity == 4 ~ 2,
                                    white_identity == 5 ~ 1),
         american_identity = V202504,
         american_identity = na_if(american_identity, -9),
         american_identity = na_if(american_identity, -7),
         american_identity = na_if(american_identity, -6),
         american_identity = na_if(american_identity, -5),
         american_identity = case_when(american_identity == 1 ~ 5,
                                      american_identity == 2 ~ 4,
                                      american_identity == 3 ~ 3,
                                      american_identity == 4 ~ 2,
                                      american_identity == 5 ~ 1),
         religious_group = V201458x,
         religious_group = na_if(religious_group, -1),
         religious_group = case_when(religious_group == 9 ~ "Not Religious",
                                     religious_group == 2 ~ "Evangelical Chistians",
                                     religious_group == 1 ~ "Other Christians",
                                     religious_group >= 3 & religious_group <= 6 ~ "Other Christians",
                                     religious_group == 7 | religious_group == 8 ~ "Other Religion"),
         sociotropic_econ = V201325,
         sociotropic_econ = na_if(sociotropic_econ, -9),
         sociotropic_econ = na_if(sociotropic_econ, -8),
         sociotropic_econ = case_when(sociotropic_econ == 2 ~ "Stayed about the same",
                                      sociotropic_econ == 1 ~ "Gotten better",
                                      sociotropic_econ == 3 ~ "Gotten worse"),
         share_white = White.alone..not.Hispanic.or.Latino..percent,
         share_white = as.numeric(sub("%", "",share_white)),
         share_white = share_white/100,
         share_black = Black.or.African.American.alone..percent ,
         share_black = as.numeric(sub("%", "",share_black)),
         share_black = share_black/100,
         share_native = American.Indian.and.Alaska.Native.alone..percent,
         share_native = as.numeric(sub("%", "",share_native)),
         share_native = share_native/100,
         share_asian = Asian.alone..percent,
         share_asian = as.numeric(sub("%", "",share_asian)),
         share_asian = share_asian/100,
         share_latino = Hispanic.or.Latino..percent,
         share_latino = as.numeric(sub("%", "",share_latino)),
         share_latino = share_latino/100,
         share_hawaiian = Native.Hawaiian.and.Other.Pacific.Islander.alone..percent,
         share_hawaiian = as.numeric(sub("%", "",share_hawaiian)),
         share_hawaiian = ifelse(is.na(share_hawaiian), 0, share_hawaiian),
         share_hawaiian = share_hawaiian/100,
         share_twoormore = Two.or.More.Races..percent,
         share_twoormore = as.numeric(sub("%", "",share_twoormore)),
         share_twoormore = share_twoormore/100,
         population = Population..Census..April.1..2020,
         unemployment = as.numeric(...2)/100,
         partisan_lean_2020 = X2020,
         partisan_lean_2020 = case_when(partisan_lean_2020 == "D+1" ~ -1,
                                        partisan_lean_2020 == "D+10" ~ -10,
                                        partisan_lean_2020 == "D+11" ~ -11,
                                        partisan_lean_2020 == "D+12" ~ -12,
                                        partisan_lean_2020 == "D+13" ~ -13,
                                        partisan_lean_2020 == "D+15" ~ -15,
                                        partisan_lean_2020 == "D+22" ~ -22,
                                        partisan_lean_2020 == "D+23" ~ -23,
                                        partisan_lean_2020 == "D+24" ~ -24,
                                        partisan_lean_2020 == "D+3" ~ -3,
                                        partisan_lean_2020 == "D+36" ~ -36,
                                        partisan_lean_2020 == "D+6" ~ -6,
                                        partisan_lean_2020 == "D+7" ~ -7,
                                        partisan_lean_2020 == "D+8" ~ -8,
                                        partisan_lean_2020 == "R+0" ~ 0,
                                        partisan_lean_2020 == "R+1" ~ 1,
                                        partisan_lean_2020 == "R+13" ~ 13,
                                        partisan_lean_2020 == "R+15" ~ 15,
                                        partisan_lean_2020 == "R+16" ~ 16,
                                        partisan_lean_2020 == "R+17" ~ 17,
                                        partisan_lean_2020 == "R+18" ~ 18,
                                        partisan_lean_2020 == "R+19" ~ 19,
                                        partisan_lean_2020 == "R+21" ~ 21,
                                        partisan_lean_2020 == "R+24" ~ 24,
                                        partisan_lean_2020 == "R+25" ~ 25,
                                        partisan_lean_2020 == "R+26" ~ 26,
                                        partisan_lean_2020 == "R+27" ~ 27,
                                        partisan_lean_2020 == "R+28" ~ 28,
                                        partisan_lean_2020 == "R+3" ~ 3,
                                        partisan_lean_2020 == "R+30" ~ 30,
                                        partisan_lean_2020 == "R+33" ~ 33,
                                        partisan_lean_2020 == "R+34" ~ 34,
                                        partisan_lean_2020 == "R+4" ~ 4,
                                        partisan_lean_2020 == "R+45" ~ 45,
                                        partisan_lean_2020 == "R+6" ~ 6,
                                        partisan_lean_2020 == "R+8" ~ 8,
                                        partisan_lean_2020 == "R+9" ~ 9),
         dem_rep_lean = case_when(partisan_lean_2020 == 0 ~ "Neutral",
                                  partisan_lean_2020 > 0 ~ "Republican",
                                  partisan_lean_2020 < 0 ~  "Democratic"),
         education = V201510,
         education = na_if(education, -9),
         education = na_if(education, -8),
         education = na_if(education, 95),
         education = case_when(education <= 3 ~ "No College Degree",
                               education >= 4 ~ "College Degree"),
         gender = V201600,
         gender = na_if(gender, -9),
         gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female"))

#####CREATE DV favor01: Favoring Biden(1) or Trump(0)#####

data_master <- data_master %>%
  mutate(biden_trump_diff = biden_thermometer-trump_thermometer,
         favor01 = case_when(biden_trump_diff > 15 ~ 1,
                             biden_trump_diff < -15 ~ 0))

#####CREATE IVs#####

data_master <- data_master %>%
  mutate(whiteID = white_identity,
         natID = american_identity,
         relID = as.factor(religious_group),
         relID = relevel(relID, "Not Religious"),
         soc_econ = factor(sociotropic_econ),
         soc_econ = relevel(soc_econ, "Stayed about the same"),
         unemp = unemployment*100,
         dem_rep_lean = factor(dem_rep_lean),
         dem_rep_lean = relevel(dem_rep_lean, "Republican"),
         eth_div = (1 - (share_white^2 + share_black^2 + share_native^2 + share_asian^2 + share_latino^2 + share_hawaiian^2 + share_twoormore^2))*100,
         edu = as.factor(education),
         edu = relevel(edu, "No College Degree"),
         gndr = as.factor(gender),
         gndr = relevel(gndr, "Male"))

#####FULL MODEL#####

full_model <- glmer(favor01 ~ 1 + scale(whiteID) + scale(natID) + relID 
                    + soc_econ + gndr + edu
                    + scale(eth_div) + scale(unemp) + dem_rep_lean
                    + (1|state),
                    data = data_master,
                    family = binomial,
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(full_model)



#Selecting Sample from the Regression Model as New Dataset "model_sample": n = 4474 for All Models
vars_drop_na <- c("V200010b", "favor01", "whiteID", "natID",
                  "relID", "soc_econ", "gndr", "edu",
                  "eth_div", "unemp", "dem_rep_lean", "state")

model_sample <- data_master %>%
  drop_na(all_of(vars_drop_na))

#Create Table With Number of Observations by State
statetable <- model_sample %>%
  group_by(State) %>%
  summarise(n = n())
tab_df(statetable)

#####DIFFERENT MODEL SPECIFICATIONS#####

#Null Model
null_model <- glmer(favor01 ~ 1 + (1|state), data = model_sample, family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(null_model)

#Identity Model
identity_model <- glmer(favor01 ~ 1 + scale(whiteID) + scale(natID) + relID
                        + (1|state), data = model_sample, family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(identity_model)

#Individual Model
individual_model <- glmer(favor01 ~ 1 + scale(whiteID) + scale(natID) + relID
                                 + soc_econ + gndr
                                 + edu + (1|state), data = model_sample,
                                 family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(individual_model)


#ICC For The Null Model
icc_null <- performance::icc(glmer(favor01 ~ 1 + (1|state), data = model_sample, family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))
icc_null

#Likelihood Ratio Test for Comparison Between Two Most Complex Models
likelihood_ratio_test <- lrtest(individual_model, full_model)
tab_df(likelihood_ratio_test)

#Full Regression Table Including All 4 Models
regression_table <- tab_model(null_model, identity_model, individual_model, full_model,
                              show.ci = FALSE,
                              transform = NULL,
                              pred.labels = c("Intercept", "White Identity", "National Identity", "Religious Identity: Evangelical (Ref. Not Religious)", 
                                              "Religious Identity: Other Christian (Ref. Not Religious)", "Religious Identity: Other Religion (Ref. Not Religious)",
                                              "National Economy: Gotten Better (Ref. Stayed the same)", "National Economy: Gotten Worse (Ref. Stayed the same)",
                                              "Gender", "Education: College Degree (Ref. No College Degree)",
                                              "State: Ethnic Diversity", "State: Unemployment Rate", "State: Partisan Lean: Democratic (Ref. Republican)", 
                                              "State: Partisan Lean: Neutral (Ref. Republican)"),
                              dv.labels = c("Null Model", "Identity Model", "Individual Model", "Full Model"),
                              string.pred = "Coefficient",
                              string.ci = "Conf. Int (95%)",
                              string.p = "p-value",
                              string.se = "Std. Error",
                              string.est = "Logit",
                              p.style = "stars",
                              show.se = TRUE,
                              show.aic = TRUE,
                              show.reflvl = TRUE,
                              title = "Dependent Variable: Favoring Donald Trump (0) or Joe Biden (1)",
                              use.viewer = TRUE)
regression_table

#####FURTHER VISUALIZATIONS AND INSIGHTS FOR DIFFERENT MODELS #####

#Creating Help-Models Without Scaled Variables For Better Visualization
full_model_help <- glmer(favor01 ~ 1 + whiteID + natID + relID
                         + soc_econ + gndr + edu
                         + eth_div + unemp + dem_rep_lean
                         + (1|state),
                         data = data_master,
                         family = binomial,
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

identity_model_help <- glmer(favor01 ~ 1 + whiteID + natID + relID 
                             + (1|state), data = model_sample, 
                             family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

individual_model_help <- glmer(favor01 ~ 1 + whiteID + natID + relID
                                 + soc_econ + gndr
                                 + edu + (1|state), data = model_sample,
                                 family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Predicted Probabilities For Selected Independent Variables
plot_model(full_model_help, type = "pred", terms = c("whiteID"),
           title = "",
           axis.title = c("White Identity", "Predicted Probability")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14)) +
  ylim(c(0,1))

plot_model(full_model_help, type = "pred", terms = c("natID"),
           title = "",
           axis.title = c("National Identity", "Predicted Probability")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14)) +
  ylim(c(0,1))

plot_model(full_model_help, type = "pred", terms = c("relID"),
           title = "",
           axis.title = c("Religious Identity", "Predicted Probability")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14),
        plot.margin = margin(.1,2,.1,.5, "cm")) +
  ylim(c(0,1))

plot_model(full_model_help, type = "pred", terms = c("soc_econ"),
           title = "",
           axis.title = c("Perception of National Economy", "Predicted Probability")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14),
        plot.margin = margin(.1,2,.1,.5, "cm")) +
  ylim(c(0,1))

plot_model(full_model_help, type = "pred", terms = c("eth_div [all]"),
           title = "",
           axis.title = c("Ethnic Diversity", "Predicted Probability")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14)) +
  ylim(c(0,1))

plot_model(full_model_help, type = "pred", terms = c("dem_rep_lean"),
           title = "",
           axis.title = c("Partisan Lean", "Predicted Probability")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14)) +
  ylim(c(0,1))


#Average Marginal Effects
ame_full <- margins_summary(full_model_help)
ame_identity <- margins_summary(identity_model_help)
ame_individual <- margins_summary(individual_model_help)
tab_df(ame_full)
tab_df(ame_identity)
tab_df(ame_individual)


#Insigths To White Identity Effect
white_model <- glmer(favor01 ~ 1 + scale(whiteID)
                     + (1|state), data = model_sample, 
                     family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
whiteandnat_model <- glmer(favor01 ~ 1 + scale(whiteID) + scale(natID)
                           + (1|state), data = model_sample, 
                           family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(white_model)
summary(whiteandnat_model)

#Bivariate Relationship Between White Identity and National Identity
cor.test(model_sample$natID, model_sample$whiteID)

#Look for Interaction Effect Between White Identity and National Identity
full_interaction <- glmer(favor01 ~ 1 + scale(whiteID) + scale(natID) + relID 
      + scale(whiteID)*scale(natID)                    
      + soc_econ + gndr + edu
      + scale(eth_div) + scale(unemp) + dem_rep_lean
      + (1|state),
      data = data_master,
      family = binomial,
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(full_interaction)

interaction_table <- tab_model(full_interaction,
          show.ci = FALSE,
          transform = NULL,
          pred.labels = c("Intercept", "White Identity", "National Identity", "Religious Identity: Evangelical (Ref. Not Religious)", 
                          "Religious Identity: Other Christian (Ref. Not Religious)", "Religious Identity: Other Religion (Ref. Not Religious)",
                          "National Economy: Gotten Better (Ref. Stayed the same)", "National Economy: Gotten Worse (Ref. Stayed the same)",
                          "Gender", "Education: College Degree (Ref. No College Degree)",
                          "State: Ethnic Diversity", "State: Unemployment Rate", "State: Partisan Lean: Democratic (Ref. Republican)", 
                          "State: Partisan Lean: Neutral (Ref. Republican)", "White Identity x National Identity"),
          dv.labels = c("Interaction Model"),
          string.pred = "Coefficient",
          string.ci = "Conf. Int (95%)",
          string.p = "p-value",
          string.se = "Std. Error",
          string.est = "Logit",
          p.style = "stars",
          show.se = TRUE,
          show.aic = TRUE,
          show.reflvl = TRUE,
          title = "Dependent Variable: Favoring Donald Trump (0) or Joe Biden (1)",
          use.viewer = TRUE)
interaction_table

#Barplot White Identity Grouped by National Identity
ggplot(model_sample, aes(x = whiteID, fill = factor(natID))) +
  geom_bar(position = "dodge") +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "White Identity", breaks = seq(1,5,1), 
                     labels = c("Not at all\nimportant",
                                "A little\nimportant",
                                "Moderately\nimportant",
                                "Very\nimportant",
                                "Extremely\nimportant")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 45),
        title = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=14),
        legend.key.size = unit(1.5, 'cm'),
        legend.text=element_text(size = 20)) +
  scale_fill_manual(values=c("#68BBE3",
                             "#0E86D4",
                             "#055C9D",
                             "#000066",
                             "#003060"),
                    name = "National Identity",
                    labels = c("Not at all important",
                               "A little important",
                               "Moderately important",
                               "Very important",
                               "Extremely important")) +
  ggtitle("", subtitle = )


#####MODEL DIAGNOSTICS#####

#Plot Normal Distribution of Level-1 Residuals: Histogram
resid_df <- data.frame(residuals(full_model, type = "response")) %>%
  rename(residuals = residuals.full_model..type....response..) #get level-1 residuals from the full model

ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "#055C9D", bins = 15) +
  scale_x_continuous(name = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14),
        legend.text=element_text(size = 20)) +
  stat_function(fun = dnorm,
                args = list(mean = mean(resid_df$residuals),
                            sd = sd(resid_df$residuals))) +
  scale_y_continuous(name = "Density")


#Plot Normal Distribution of Random Intercepts: Histogram
full_model_coefficients <- coef(full_model)
randomeffects_coefficients <- full_model_coefficients$state #get random intercepts from the full model

ggplot(randomeffects_coefficients, aes(x = `(Intercept)`)) +
  geom_histogram(color = "white", fill = "#055C9D", bins = 7) +
  scale_x_continuous(name = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14),
        legend.text=element_text(size = 20)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(randomeffects_coefficients$`(Intercept)`), 
                            sd = sd(randomeffects_coefficients$`(Intercept)`))) +
  scale_y_continuous(name = "Count")

#####ROBUSTNESS CHECKS#####

#Random Effects With biden_thermometer as DV
randomeffects_biden <- lmer(biden_thermometer ~ 1 + scale(whiteID) + scale(natID) + relevel(relID, ref = "Not Religious")
                            + relevel(soc_econ, ref = "Stayed about the same") + relevel(gndr, ref = "Male") + relevel(edu, ref = "No College Degree")
                            + scale(eth_div) + scale(unemp) + relevel(dem_rep_lean, ref = "Republican")
                            + (1|state),
                            data = model_sample)
summary(randomeffects_biden)
BIC(randomeffects_biden)

#Random Effects With trump_thermometer as DV
randomeffects_trump <- lmer(trump_thermometer ~ 1 + scale(whiteID) + scale(natID) + relevel(relID, ref = "Not Religious")
                            + relevel(soc_econ, ref = "Stayed about the same") + relevel(gndr, ref = "Male") + relevel(edu, ref = "No College Degree")
                            + scale(eth_div) + scale(unemp) + relevel(dem_rep_lean, ref = "Republican") 
                            + (1|state),
                            data = model_sample)
summary(randomeffects_trump)
BIC(randomeffects_trump)

#Robustness Table
robustness_table <- tab_model(randomeffects_biden, randomeffects_trump,
                              show.ci = FALSE,
                              transform = NULL,
                              pred.labels = c("Intercept", "White Identity", "National Identity", "Religious Identity: Evangelical (Ref. Not Religious)", 
                                              "Religious Identity: Other Christian (Ref. Not Religious)", "Religious Identity: Other Religion (Ref. Not Religious)",
                                              "National Economy: Gotten Better (Ref. Stayed the same)", "National Economy: Gotten Worse (Ref. Stayed the same)",
                                              "Gender", "Education: College Degree (Ref. No College Degree)",
                                              "State: Ethnic Diversity", "State: Unemployment Rate", "State: Partisan Lean: Democratic (Ref. Republican)", 
                                              "State: Partisan Lean: Neutral (Ref. Republican)"),
                              dv.labels = c("DV: Thermometer Joe Biden", "DV: Thermometer Donald Trump"),
                              string.pred = "Coefficient",
                              string.ci = "Conf. Int (95%)",
                              string.p = "p-value",
                              string.se = "Std. Error",
                              string.est = "Coefficient",
                              p.style = "stars",
                              show.se = TRUE,
                              show.aic = TRUE,
                              show.reflvl = TRUE,
                              title = "Robustness Check with Original Thermometer Variables",
                              use.viewer = TRUE)
robustness_table

#####UNIVARIATE DESCIRPTIVES AND MAPS#####

#Create Survey Design Object to Weigh Data
design <- svydesign(ids = ~ 1,
                           data = model_sample,
                           weights = model_sample$V200010b)

#Weighted Descriptives for Individual-Level Continuous Variables
continuous_desc <- function(z){
  print(weighted_mean(z, weights = model_sample$V200010b))
  print(weighted_median(z, weights = model_sample$V200010b))
  print(weighted_sd(z, weights = model_sample$V200010b))
  print(wtd.var(z, weights = model_sample$V200010b))
  print(min(z))
  print(max(z))
  print(nrow(model_sample))
}

continuous_desc(model_sample$whiteID)
continuous_desc(model_sample$natID)
continuous_desc(model_sample$biden_thermometer)
continuous_desc(model_sample$trump_thermometer)

#Weighted Frequencies for Individual-Level Categorical Variables
wtd.table(model_sample$relID, weights = model_sample$V200010b)
wtd.table(model_sample$soc_econ, weights = model_sample$V200010b)
wtd.table(model_sample$gndr, weights = model_sample$V200010b)
wtd.table(model_sample$edu, weights = model_sample$V200010b)
wtd.table(model_sample$favor01, weights = model_sample$V200010b)


#New State Dataset "contextdata" for Descriptives at the Contextual Level and Maps
contextdata <- model_sample %>%
  select(state, eth_div, partisan_lean_2020, dem_rep_lean, unemp) %>%
  distinct() %>%
  mutate(id = state,
         state = case_when(state == 1 ~ "Alabama",
                           state == 2 ~ "Alaska",
                           state == 4 ~ "Arizona",
                           state == 5 ~ "Arkansas",
                           state == 6 ~ "California",
                           state == 8 ~ "Colorado",
                           state == 9 ~ "Connecticut",
                           state == 10 ~ "Delaware",
                           state == 11 ~ "District of Columbia",
                           state == 12 ~ "Florida",
                           state == 13 ~ "Georgia",
                           state == 15 ~ "Hawaii",
                           state == 16 ~ "Idaho",
                           state == 17 ~ "Illinois",
                           state == 18 ~ "Indiana",
                           state == 19 ~ "Iowa",
                           state == 20 ~ "Kansas",
                           state == 21 ~ "Kentucky",
                           state == 22 ~ "Louisiana",
                           state == 23 ~ "Maine",
                           state == 24 ~ "Maryland",
                           state == 25 ~ "Massachusetts",
                           state == 26 ~ "Michigan",
                           state == 27 ~ "Minnesota",
                           state == 28 ~ "Mississippi",
                           state == 29 ~ "Missouri",
                           state == 30 ~ "Montana",
                           state == 31 ~ "Nebraska",
                           state == 32 ~ "Nevada",
                           state == 33 ~ "New Hampshire",
                           state == 34 ~ "New Jersey",
                           state == 35 ~ "New Mexico",
                           state == 36 ~ "New York",
                           state == 37 ~ "North Carolina",
                           state == 38 ~ "North Dakota",
                           state == 39 ~ "Ohio",
                           state == 40 ~ "Oklahoma",
                           state == 41 ~ "Oregon",
                           state == 42 ~ "Pennsylvania",
                           state == 44 ~ "Rhode Island",
                           state == 45 ~ "South Carolina",
                           state == 46 ~ "South Dakota",
                           state == 47 ~ "Tennessee",
                           state == 48 ~ "Texas",
                           state == 49 ~ "Utah",
                           state == 50 ~ "Vermont",
                           state == 51 ~ "Virginia",
                           state == 53 ~ "Washington",
                           state == 54 ~ "West Virginia",
                           state == 55 ~ "Wisconsin",
                           state == 56 ~ "Wyoming"),
         abbr = case_when(state == "Alabama" ~ "AL",
                          state == "Alaska" ~ "AK",
                          state == "Arizona" ~ "AZ",
                          state == "Arkansas" ~ "AR",
                          state == "California" ~ "CA",
                          state == "Colorado" ~ "CO",
                          state == "Connecticut" ~ "CT",
                          state == "Delaware" ~ "DE",
                          state == "District of Columbia" ~ "DC",
                          state == "Florida" ~ "FL",
                          state == "Georgia" ~ "GA",
                          state == "Hawaii" ~ "HI",
                          state == "Idaho" ~ "ID",
                          state == "Illinois" ~ "IL",
                          state == "Indiana" ~ "IN",
                          state == "Iowa" ~ "IA",
                          state == "Kansas" ~ "KS",
                          state == "Kentucky" ~ "KY",
                          state == "Louisiana" ~ "LA",
                          state == "Maine" ~ "ME",
                          state == "Maryland" ~ "MD",
                          state == "Massachusetts" ~ "MA",
                          state == "Michigan" ~ "MI",
                          state == "Minnesota" ~ "MN",
                          state == "Mississippi" ~ "MS",
                          state == "Missouri" ~ "MO",
                          state == "Montana" ~ "MT",
                          state == "Nebraska" ~ "NE",
                          state == "Nevada" ~ "NV",
                          state == "New Hampshire" ~ "NH",
                          state == "New Jersey" ~ "NJ",
                          state == "New Mexico" ~ "NM",
                          state == "New York" ~ "NY",
                          state == "North Carolina" ~ "NC",
                          state == "North Dakota" ~ "ND",
                          state == "Ohio" ~ "OH",
                          state == "Oklahoma" ~ "OK",
                          state == "Oregon" ~ "OR",
                          state == "Pennsylvania" ~ "PA",
                          state == "Rhode Island" ~ "RI",
                          state == "South Carolina" ~ "SC",
                          state == "South Dakota" ~ "SD",
                          state == "Tennessee" ~ "TN",
                          state == "Texas" ~ "TX",
                          state == "Utah" ~ "UT",
                          state == "Vermont" ~ "VT",
                          state == "Virginia" ~ "VA",
                          state == "Washington" ~ "WA",
                          state == "West Virginia" ~ "WV",
                          state == "Wisconsin" ~ "WI",
                          state == "Wyoming" ~ "WY"))

#New Dataset "shares" Which Includes Two Variables for the Share of Biden and Trump Sympathizers Per State
shares <- model_sample %>%
  group_by(state) %>%
  summarise(n = n(),
            share_biden = mean(favor01),
            share_trump = 1 - mean(favor01))

#Merging "contextdata" and "shares" to Full Contextual-Level Dataset
contextdata <- contextdata %>%
  left_join(shares, by = c("id" = "state"))


#MAP: ethnic diversity by state
plot_usmap(data = contextdata, values = "eth_div", regions = "state", alpha = 0.8, labels = TRUE, color = "white") +
  scale_fill_viridis_c(name = "Ethnic Diversity", option = "D") + 
  labs(title = "", subtitle = "") +
  theme(legend.position = "right",
        text=element_text(family="Book Antiqua", size=14),
        legend.key.size = unit(1.5, 'cm'))
  

#MAP: unemployment by state
plot_usmap(data = contextdata, values = "unemp", regions = "state", alpha = 0.8, labels = TRUE, color = "white") +
  scale_fill_viridis_c(name = "Unemployment Rate", option = "D") + 
  labs(title = "", subtitle = "") +
  theme(legend.position = "right",
        text=element_text(family="Book Antiqua", size=14),
        legend.key.size = unit(1.5, 'cm'))

#MAP: partisan lean by state
plot_usmap(data = contextdata, values = "dem_rep_lean", alpha = 0.8, regions = "state", labels = TRUE, color = "white") +
  scale_fill_manual(values = c("#FF0000", "#0015BC", "grey"), name = "Partisan Lean", na.translate = FALSE) + 
  labs(title = "", subtitle = "") +
  theme(legend.position = "right",
        text=element_text(family="Book Antiqua", size=14),
        legend.key.size = unit(1.5, 'cm'))


#####BIVARIATE ANALYSES#####

#Bivariate Analysis: White identity x favor01

t.test(model_sample$whiteID ~ model_sample$favor01)

ggplot(model_sample, 
       aes(x = whiteID, 
           fill = factor(favor01,
                         labels = c("Donald Trump", "Joe Biden")))) +
  geom_bar(position = "dodge") +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "White Identity", breaks = seq(1,5,1), 
                     labels = c("Not at all important", "A little important", "Moderately important", "Very important", "Extremely important")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 20),
        axis.title.x = element_text(size = 45),
        axis.text.y = element_text(size = 30),
        title = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=30)) +
  scale_fill_manual(values=c("#FF0000",
                             "#0015BC"),
                    name = "Candidate Preference") +
  ggtitle("", subtitle = "")

#Bivariate Analysis: National identity x favor01

t.test(model_sample$natID ~ model_sample$favor01)

ggplot(model_sample, 
       aes(x = natID, 
           fill = factor(favor01,
                         labels = c("Donald Trump", "Joe Biden")))) +
  geom_bar(position = "dodge") +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "National Identity", breaks = seq(1,5,1), 
                     labels = c("Not at all important", "A little important", "Moderately important", "Very important", "Extremely important")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 20),
        axis.title.x = element_text(size = 45),
        axis.text.y = element_text(size = 30),
        title = element_text(size = 45),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=30)) +
  scale_fill_manual(values=c("#FF0000",
                             "#0015BC"),
                    name = "Candidate Preference") +
  ggtitle("", subtitle = "")

#Bivariate Analysis: Religious identity x favor01

chisq.test(model_sample$relID, model_sample$favor01)
cramerV(model_sample$relID, model_sample$favor01)

ggplot(model_sample, 
       aes(x = relID, 
           fill = factor(favor01,
                         labels = c("Donald Trump", "Joe Biden")))) +
  geom_bar(position = "dodge") +
  scale_y_continuous(name = "") +
  scale_x_discrete(name = "Religious Identity",
                     labels = c("Evangelical Christians", "Not Religious", "Other Christians", "Other Religion")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 20),
        axis.title.x = element_text(size = 25),
        title = element_text(size = 16),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=25)) +
  scale_fill_manual(values=c("#FF0000",
                             "#0015BC"),
                    name = "Candidate Preference") +
  ggtitle("", subtitle = "")

#Bivariate Analysis: National Economy x favor01

chisq.test(model_sample$soc_econ, model_sample$favor01)
cramerV(model_sample$soc_econ, model_sample$favor01)

ggplot(model_sample, 
       aes(x = soc_econ, 
           fill = factor(favor01,
                         labels = c("Donald Trump", "Joe Biden")))) +
  geom_bar(position = "dodge") +
  scale_y_continuous(name = "") +
  scale_x_discrete(name = "Perception of National Economy",
                   labels = c("Gotten better", "Gotten worse", "Stayed about the same")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 20),
        axis.title.x = element_text(size = 25),
        title = element_text(size = 16),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=25)) +
  scale_fill_manual(values=c("#FF0000",
                             "#0015BC"),
                    name = "Candidate Preference") +
  ggtitle("", subtitle = "")



#Bivariate Analysis Contextual Level: Ethnic Dicversity x share_biden

cor.test(contextdata$eth_div, contextdata$share_biden, method = "pearson")

ggplot(contextdata, aes(eth_div, share_biden*100)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkblue") +
  theme_minimal() +
  theme(axis.text = element_text(size = 25),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=14)) +
  scale_x_continuous(name = "Ethnic Diversity Index in %") +
  scale_y_continuous(name = "% Preferring Joe Biden")


#Bivariate Analysis Contextual Level: Unemployment x share_biden

cor.test(contextdata$unemp, contextdata$share_biden, method = "pearson")

ggplot(contextdata, aes(unemp*100, share_biden*100)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkblue") +
  theme_minimal() +
  theme(axis.text = element_text(size = 25),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=14)) +
  scale_x_continuous(name = "Unemployment Rate in %") +
  scale_y_continuous(name = "% Preferring Joe Biden")

#Bivariate Analysis Contextual Level: Partisan Lean x share_biden

helpcontext <- contextdata %>%
  filter(dem_rep_lean %in% c("Democratic", "Republican"))
t.test(helpcontext$share_biden ~ helpcontext$dem_rep_lean)

ggplot(helpcontext, 
       aes(x = factor(dem_rep_lean,
                      labels = c("Republican Lean",
                                 "Democratic Lean")), 
           y = share_biden*100)) +
  geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 3,
               color = c("red", "darkblue")) +
  geom_jitter(alpha = 0.5, 
              width=.2) +
  scale_y_continuous(name = "% Preferring Joe Biden") +
  labs(title = "", 
       subtitle = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 25),
        axis.title.y = element_text(size = 40),
        title = element_text(size = 40),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=14)) +
  theme(legend.position = "none") +
  coord_flip()

#Bivariate Analysis Contextual Level: Ethnic Diversity x share_trump

cor.test(contextdata$eth_div, contextdata$share_trump, method = "pearson")

ggplot(contextdata, aes(eth_div, share_trump*100)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  theme(axis.text = element_text(size = 25),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40),
        title = element_text(size = 16),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=14)) +
  scale_x_continuous(name = "Ethnic Diversity Index in %") +
  scale_y_continuous(name = "% Preferring Donald Trump")

#Bivariate Analysis Contextual Level: Unemployment x share_trump

cor.test(contextdata$unemp, contextdata$share_trump, method = "pearson")

ggplot(contextdata, aes(unemp*100, share_trump*100)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  theme(axis.text = element_text(size = 25),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40),
        title = element_text(size = 16),
        plot.title = element_text(face = "bold"),
        text=element_text(family="Book Antiqua", size=14)) +
  scale_x_continuous(name = "Unemployment Rate in %") +
  scale_y_continuous(name = "% Preferring Donald Trump")

#Bivariate Analysis Contextual Level: Partisan Lean x share_trump

helpcontext <- contextdata %>%
  filter(dem_rep_lean %in% c("Democratic", "Republican"))
t.test(helpcontext$share_trump ~ helpcontext$dem_rep_lean)

ggplot(helpcontext, 
       aes(x = factor(dem_rep_lean,
                      labels = c("Republican Lean",
                                 "Democratic Lean")), 
           y = share_trump*100)) +
  geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 3,
               color = c("red", "darkblue")) +
  geom_jitter(alpha = 0.5, 
              width=.2) +
  scale_y_continuous(name = "% Preferring Donald Trump") +
  labs(title = "", 
       subtitle = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 25),
        axis.title.y = element_text(size = 40),
        axis.title.x = element_text(size = 40),
        plot.title = element_text(face = "bold"),
        text = element_text(family="Book Antiqua", size=14)) +
  theme(legend.position = "none") +
  coord_flip()