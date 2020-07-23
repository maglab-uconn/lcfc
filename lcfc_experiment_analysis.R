# Basic R setup ----
rm(list = ls(all = TRUE))

# Load packages.
# Loading data.
library(here)
library(bit64)

# Data manipulation.
library(data.table)
library(Rmisc)
library(dplyr)
library(stringr)
library(rio)
library(tidyverse)
library(janitor)

# Plots.
library(ggplot2)
library(cowplot)
library(psych)

# Analyses.
library(afex)
library(lme4)
library(lmerTest)

theme_set(theme_bw(base_size = 20))

# Format the data ####
# Read in files
file_names <- list.files(path = "./experiment_lcfc", pattern = "*.csv", all.files = FALSE,
                         full.names = TRUE, recursive = FALSE)

# Create data frame
df <- data.frame()

# Loop to create combined dataframe
for (i in file_names) {
  data <- fread(i, header = TRUE, sep = ",")
  df <- rbind(df, data)
}

# Clean up environment
rm(data); rm(file_names); rm(i) 

# Make variable names syntactically valid - everything is machine readable now
df <- clean_names(df)

# Give columns more informative names
colnames(df)[which(colnames(df)=="checkpoint_7g4v")] <- "headphone_check"
colnames(df)[which(colnames(df)=="randomiser_ku42")] <- "cb"
colnames(df)[which(colnames(df)=="display")] <- "task"
colnames(df)[which(colnames(df)=="fname")] <- "stimulus"
colnames(df)[which(colnames(df)=="participant_public_id")] <- "subject"


# Read in demographics
demographics <- read.csv('experiment_lcfc/questionnaire/lcfc-experiment-demographics.csv', header = TRUE, sep = ',', na.strings = "#N/A")
demographics <- clean_names(demographics)
demographics <- select(demographics, participant_id, age, sex)
colnames(demographics)[which(colnames(demographics)=="participant_id")] <- "subject"

# Add demographics to main df and clean up environment
df <- merge(df, demographics, by = "subject")
rm(demographics)

# Select only rows/columns we want
df <- dplyr::filter(df,df$trial_number != "BEGIN TASK")
df <- dplyr::filter(df,df$trial_number != "END TASK")
df <- droplevels(subset(df,zone_type=="response_keyboard_single"))

df <- select(df, subject, age, sex, 
             headphone_check, 
             task,cb,
             spreadsheet_row,trial_number,stimulus,
             context, front_word, back_word, step, step_scaled, target_contrast,
             response,reaction_time)

# Recode responses
df$front_response <- NA
df$front_response[which(df$response=="S sound" | df$response=="s")] <- 1
df$front_response[which(df$response=="SH sound" | df$response=="sh")] <- 0

# Note whether context word ends with front segment or back segment
df$context_bias <- NA
df$context_bias[which(df$context == "isolate" | df$context == "pocketful")] <- "front"
df$context_bias[which(df$context == "maniac" | df$context == "questionnaire")] <- "back"

# Note whether context word ends with front segment or back segment
df$context_contrast <- NA
df$context_contrast[which(df$context == "isolate" | df$context == "maniac")] <- "___[t/k]"
df$context_contrast[which(df$context == "pocketful" | df$context == "questionnaire")] <- "___[l/r]"
                
# Set variable type
to.factor <- c('subject','task','stimulus','context','front_word','back_word','target_contrast','context_contrast','context_bias', 'sex') 
df[, to.factor] <- lapply(df[, to.factor], as.factor)

to.numeric <- c('spreadsheet_row','trial_number','reaction_time','step','step_scaled','front_response', 'age')
df[, to.numeric] <- lapply(df[, to.numeric], as.numeric)

# Give levels of "target_contrast" variable better labels
levels(df$target_contrast)[which(levels(df$target_contrast)=="s-sh")] <- "[s/∫]____"

# Remove bad subjects ----
# Remove subjects if they never passed the headphone check
# We usually wouldn't have downloaded their data anyway, but this is a good safeguard
df <- filter(df, headphone_check == "Pass")

# Remove subjects with low accuracy (< 80%) on endpoints
# First, calculate accuracy on endpoints
df.endpoints <- droplevels(subset(df, step_scaled == -2 | step_scaled == 2))
df.endpoints$Acc <- NA
df.endpoints$Acc[which(df.endpoints$step_scaled==-2 & df.endpoints$front_response == 1)] <- 1
df.endpoints$Acc[which(df.endpoints$step_scaled==-2 & df.endpoints$front_response == 0)] <- 0
df.endpoints$Acc[which(df.endpoints$step_scaled==2 & df.endpoints$front_response == 0)] <- 1
df.endpoints$Acc[which(df.endpoints$step_scaled==2 & df.endpoints$front_response == 1)] <- 0

df.endpointAcc <- summarySE(df.endpoints,
                            measurevar = "Acc", 
                            groupvars = c("subject","cb"), 
                            na.rm = TRUE)
summary(df.endpointAcc$Acc) 

exclude <- levels(droplevels(df.endpointAcc$subject[which(df.endpointAcc$Acc<0.8)]))


# Remove subjects who timed out on more than 10% of trials
df.respSummary <- df %>%
  group_by(subject) %>% 
  dplyr::summarise(count = length(response)) 

exclude <- unique(union(exclude, levels(droplevels(df.respSummary$subject[which(df.respSummary$count <= (0.9*600))]))))

# Exclude bad subjects
df.goodSubj <- droplevels(df %>% filter(!subject %in% exclude))

# Data summary ----
# Check how many subjects in each version
df_subj_cb_summary <- df.goodSubj %>%
  group_by(cb, subject) %>% 
  dplyr::summarise(count = length(subject)) %>% 
  dplyr::summarise(count = length(count))
df_subj_cb_summary
sum(df_subj_cb_summary$count) #number of participants

# Get participant demographics
demographics <- df.goodSubj %>%
  group_by(subject, sex) %>%
  dplyr::summarise(age = mean(age))

summary(demographics$age) #age summary
summary(droplevels(demographics$sex)) #breakdown by sex

# Plots ----
# (1) Collapse across context and target contrasts
df.goodSubj_resp_byBias_summary <- na.exclude(summarySE(df.goodSubj, 
                                               measurevar = "front_response", 
                                               groupvars = c("context_bias","step_scaled"), 
                                               na.rm = TRUE))

ggplot(df.goodSubj_resp_byBias_summary) + 
  aes(step_scaled, front_response, color = context_bias) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "Implied \nPoA for last \nsegment of \ncontext item")
ggsave("lcfc_experiment_n40_continua.png", width = 25, height = 20, units = "cm")


# (2) Break down by context contrast and target contrast
df.goodSubj_resp_byContrasts_byBias_summary <- na.exclude(summarySE(df.goodSubj, 
                                                           measurevar = "front_response", 
                                                           groupvars = c("context_contrast","target_contrast","context_bias","step_scaled"), 
                                                           na.rm = TRUE))

ggplot(df.goodSubj_resp_byContrasts_byBias_summary) + 
  aes(step_scaled, front_response, color = context_bias) +
  facet_grid(cols=vars(context_contrast), rows=vars(target_contrast)) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "Implied \nPoA for last \nsegment of \ncontext item")
ggsave("lcfc_experiment_n40_continua_byContrast.png", width = 30, height = 20, units = "cm")


# (3) By subject
df.goodSubj_resp_byBias_summary_bySubj <- na.exclude(summarySE(df.goodSubj, 
                                                        measurevar = "front_response", 
                                                        groupvars = c("subject","context_bias","step_scaled"), 
                                                        na.rm = TRUE))

ggplot(df.goodSubj_resp_byBias_summary_bySubj) + 
  aes(step_scaled, front_response, color = context_bias) +
  facet_wrap(~subject, nrow= 5) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "Implied \nPoA for last \nsegment of \ncontext item")
ggsave("lcfc_experiment_n40_continua_byContrast_bySubj.png", width = 40, height = 40, units = "cm")


# Look at RT data ----

df.RT <- na.exclude(summarySE(df, 
                                        measurevar = "reaction_time", 
                                        groupvars = c("step_scaled"), 
                                        na.rm = TRUE))
ggplot(df.RT) + 
  aes(step_scaled, reaction_time) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=reaction_time-ci, ymax = reaction_time+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Response time")


df.RT.byContext <- na.exclude(summarySE(df, 
                              measurevar = "reaction_time", 
                              groupvars = c("context_bias","step_scaled"), 
                              na.rm = TRUE))
ggplot(df.RT.byContext) + 
  aes(step_scaled, reaction_time, color = context_bias) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=reaction_time-se, ymax = reaction_time+se, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Response time", color = "Implied \nPoA for last \nsegment of \ncontext item")

# Analysis ----
df.goodSubj$step_scaled <- scale(df.goodSubj$step_scaled)
contrasts(df.goodSubj$context_bias) <- c(-1,1)

m.full <- mixed(front_response ~ context_bias * step_scaled +
                  (context_bias*step_scaled | subject), data = df.goodSubj, 
                family=binomial(link="logit"), method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, 
                                       optCtrl = list(maxfun = 150000)))

m.noRC <- mixed(front_response ~ context_bias * step_scaled +
                  (0 + context_bias:step_scaled | subject) + 
                  (0 + context_bias | subject) + (0 + step_scaled | subject) +
                  (1 | subject), data = df.goodSubj, 
                family=binomial(link="logit"), method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, 
                                       optCtrl = list(maxfun = 150000)))

anova(m.full, m.noRC)

# The full model is a significantly better fit, so we select that one
rm(m.noRC)

m.full
# Mixed Model Anova Table (Type 3 tests, LRT-method)
# 
# Model: front_response ~ context_bias * step_scaled + (context_bias * step_scaled | subject)
# Data: df.goodSubj
# Df full model: 14
# Effect df      Chisq p.value
# 1             context_bias  1  42.26 ***  <.0001
# 2              step_scaled  1 124.35 ***  <.0001
# 3 context_bias:step_scaled  1     4.54 *     .03
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

summary(m.full)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: front_response ~ context_bias * step_scaled + (1 + re1.context_bias1 +  
#                                                           re1.step_scaled + re1.context_bias1_by_step_scaled | subject)
# Data: data
# Control: 
#   glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000))
# 
# AIC      BIC   logLik deviance df.resid 
# 18150.7  18263.9  -9061.4  18122.7    23867 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -15.2318  -0.3841  -0.1326   0.4167  19.0231 
# 
# Random effects:
#   Groups  Name                             Variance Std.Dev. Corr             
# subject (Intercept)                      0.457187 0.67616                   
# re1.context_bias1                0.029562 0.17194  -0.06            
# re1.step_scaled                  0.221509 0.47065  -0.14 -0.37      
# re1.context_bias1_by_step_scaled 0.003641 0.06034  -0.28 -0.92  0.58
# Number of obs: 23881, groups:  subject, 40
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.70913    0.10905  -6.503 7.89e-11 ***
#   context_bias1              0.29732    0.03396   8.756  < 2e-16 ***
#   step_scaled               -2.31268    0.07980 -28.983  < 2e-16 ***
#   context_bias1:step_scaled -0.06571    0.02850  -2.305   0.0211 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) cntx_1 stp_sc
# context_bs1 -0.060              
# step_scaled -0.108 -0.306       
# cntxt_bs1:_ -0.116 -0.067  0.192
