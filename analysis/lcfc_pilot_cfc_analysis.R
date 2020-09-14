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
file_names <- list.files(path = "./pilot_cfc", pattern = "*.csv", all.files = FALSE,
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
colnames(df)[which(colnames(df)=="spreadsheet_name")] <- "order"
colnames(df)[which(colnames(df)=="display")] <- "task"
colnames(df)[which(colnames(df)=="fname")] <- "stimulus"
colnames(df)[which(colnames(df)=="participant_public_id")] <- "subject"
colnames(df)[which(colnames(df)=="contrast")] <- "target_contrast"

# Drop extra subject to equalize counterbalancing conditions
df <- droplevels(subset(df, subject != "5efa9017170fdf8d483d1c1a"))


# Read in demographics
demographics <- read.csv('pilot_cfc/questionnaire/lcfc-pilot-cfc-demographics.csv', header = TRUE, sep = ',', na.strings = "#N/A")
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
             order,task,
             spreadsheet_row,trial_number,stimulus,
             context, front_word, back_word, step, step_scaled, target_contrast,
             response,reaction_time)

# Recode responses
df$front_response <- NA
df$front_response[which(df$response=="T sound" | df$response=="S sound" | df$response=="D sound" | df$response=="L sound" | df$response=="t" | df$response=="s" | df$response=="d" | df$response=="l")] <- 1
df$front_response[which(df$response=="K sound" | df$response=="SH sound" | df$response=="G sound" | df$response=="R sound" | df$response=="k" | df$response=="sh" | df$response=="g" | df$response=="r")] <- 0

# Note whether context word ends with front segment or back segment
df$context_bias <- NA
df$context_bias[which(df$context == "dangerous" | df$context == "isolate" | 
                        df$context == "overboard" | df$context == "pocketful" | 
                        df$context == "universe")] <- "front"
df$context_bias[which(df$context == "abolish" | df$context == "catalogue" | 
                        df$context == "demolish" | df$context == "maniac" | 
                        df$context == "questionnaire")] <- "back"


# Note whether context word ends with front segment or back segment
df$context_contrast <- NA
df$context_contrast[which(df$context == "dangerous" | df$context == "universe" | df$context == "abolish" | df$context == "demolish")] <- "___[s/∫]"
df$context_contrast[which(df$context == "overboard" | df$context == "catalogue")] <- "___[d/g]"
df$context_contrast[which(df$context == "isolate" | df$context == "maniac")] <- "___[t/k]"
df$context_contrast[which(df$context == "pocketful" | df$context == "questionnaire")] <- "___[l/r]"
                
# Set variable type
to.factor <- c('subject','order','task','context','front_word','back_word','target_contrast','context_contrast','context_bias', 'sex')
df[, to.factor] <- lapply(df[, to.factor], as.factor)

to.numeric <- c('spreadsheet_row','trial_number','reaction_time','step','step_scaled','front_response', 'age')
df[, to.numeric] <- lapply(df[, to.numeric], as.numeric)

# Give levels of "target_contrast" variable better labels
levels(df$target_contrast)[which(levels(df$target_contrast)=="d-g")] <- "[d/g]____"
levels(df$target_contrast)[which(levels(df$target_contrast)=="l-r")] <- "[l/r]____"
levels(df$target_contrast)[which(levels(df$target_contrast)=="s-sh")] <- "[s/∫]____"
levels(df$target_contrast)[which(levels(df$target_contrast)=="t-k")] <- "[t/k]____"


# Remove subjects if they never passed the headphone check
# We usually wouldn't have downloaded their data anyway, but this is a good safeguard
df <- filter(df, headphone_check == "Pass")

# Data summary ----
# Check how many subjects in each version
df_subj_cb_summary <- df %>%
  group_by(order, subject) %>% 
  dplyr::summarise(count = length(subject)) %>% 
  dplyr::summarise(count = length(count))
df_subj_cb_summary
sum(df_subj_cb_summary$count) #number of participants

# Get participant demographics
demographics <- df %>%
  group_by(subject, sex) %>% 
  dplyr::summarise(age = mean(age)) 

summary(demographics$age) #age summary
summary(droplevels(demographics$sex)) #breakdown by sex  

# Plots for all subjects ----

# (1) Collapse across context and target contrasts
df_resp_byBias_summary <- na.exclude(summarySE(df, 
                                               measurevar = "front_response", 
                                               groupvars = c("context_bias","step_scaled"), 
                                               na.rm = TRUE))
ggplot(df_resp_byBias_summary) + 
  aes(step_scaled, front_response, color = context_bias) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "PoA for last \nsegment of \ncontext item")
ggsave("lcfc_pilot_cfc_n20_continua.png", width = 25, height = 20, units = "cm")

# (2) Break down by context contrast
df_resp_byContextContrast_byBias_summary <- na.exclude(summarySE(df, 
                                               measurevar = "front_response", 
                                               groupvars = c("context_contrast","context_bias","step_scaled"), 
                                               na.rm = TRUE))

ggplot(df_resp_byContextContrast_byBias_summary) + 
  aes(step_scaled, front_response, color = context_bias) +
  facet_grid(~context_contrast) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "PoA for last \nsegment of \ncontext item")
ggsave("lcfc_pilot_cfc_n20_continua_byContextContrast.png", width = 25, height = 13, units = "cm")


# (3) Break down by target contrast
df_resp_byTargetContrast_byBias_summary <- na.exclude(summarySE(df, 
                                                                 measurevar = "front_response", 
                                                                 groupvars = c("target_contrast","context_bias","step_scaled"), 
                                                                 na.rm = TRUE))

ggplot(df_resp_byTargetContrast_byBias_summary) + 
  aes(step_scaled, front_response, color = context_bias) +
  facet_grid(~target_contrast) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "PoA for last \nsegment of \ncontext item")
ggsave("lcfc_pilot_cfc_n20_continua_byTargetContrast.png", width = 25, height = 13, units = "cm")


# (4) Break down by context contrast and target contrast
df_resp_byContrasts_byBias_summary <- na.exclude(summarySE(df, 
                                                                measurevar = "front_response", 
                                                                groupvars = c("context_contrast","target_contrast","context_bias","step_scaled"), 
                                                                na.rm = TRUE))
ggplot(df_resp_byContrasts_byBias_summary) + 
  aes(step_scaled, front_response, color = context_bias) +
  facet_grid(cols=vars(context_contrast), rows=vars(target_contrast)) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "PoA for last \nsegment of \ncontext item")
ggsave("lcfc_pilot_cfc_n20_continua_byContrast.png", width = 25, height = 25, units = "cm")


# Repeat analyses with only subjects who showed good accuracy on endpoints ----
# First, check accuracy on endpoints by subject
df.endpoints <- droplevels(subset(df, step_scaled == -2 | step_scaled == 2))

df.endpoints$Acc <- NA
df.endpoints$Acc[which(df.endpoints$step_scaled==-2 & df.endpoints$front_response == 1)] <- 1
df.endpoints$Acc[which(df.endpoints$step_scaled==-2 & df.endpoints$front_response == 0)] <- 0
df.endpoints$Acc[which(df.endpoints$step_scaled==2 & df.endpoints$front_response == 0)] <- 1
df.endpoints$Acc[which(df.endpoints$step_scaled==2 & df.endpoints$front_response == 1)] <- 0

df.endpointAcc <- summarySE(df.endpoints,
                            measurevar = "Acc", 
                            groupvars = c("subject","order"), 
                            na.rm = TRUE)
summary(df.endpointAcc$Acc) 

exclude <- levels(droplevels(df.endpointAcc$subject[which(df.endpointAcc$Acc<=0.8)]))

# Exclude subjects with low accuracy on endpoints
df.goodSubj <- droplevels(df %>% filter(!subject %in% 
                                 exclude))

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
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "PoA for last \nsegment of \ncontext item")
ggsave("lcfc_pilot_cfc_n16good_continua.png", width = 25, height = 20, units = "cm")

# (2) Break down by context contrast
df.goodSubj_resp_byContextContrast_byBias_summary <- na.exclude(summarySE(df.goodSubj, 
                                                                 measurevar = "front_response", 
                                                                 groupvars = c("context_contrast","context_bias","step_scaled"), 
                                                                 na.rm = TRUE))

ggplot(df.goodSubj_resp_byContextContrast_byBias_summary) + 
  aes(step_scaled, front_response, color = context_bias) +
  facet_grid(~context_contrast) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "PoA for last \nsegment of \ncontext item")
ggsave("lcfc_pilot_cfc_n16good_continua_byContextContrast.png", width = 25, height = 13, units = "cm")


# (3) Break down by target contrast
df.goodSubj_resp_byTargetContrast_byBias_summary <- na.exclude(summarySE(df.goodSubj, 
                                                                measurevar = "front_response", 
                                                                groupvars = c("target_contrast","context_bias","step_scaled"), 
                                                                na.rm = TRUE))

ggplot(df.goodSubj_resp_byTargetContrast_byBias_summary) + 
  aes(step_scaled, front_response, color = context_bias) +
  facet_grid(~target_contrast) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "PoA for last \nsegment of \ncontext item")
ggsave("lcfc_pilot_cfc_n16good_continua_byTargetContrast.png", width = 25, height = 13, units = "cm")


# (4) Break down by context contrast and target contrast
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
  labs(x = "Step (scaled)", y = "Proportion front-PoA response", color = "PoA for last \nsegment of \ncontext item")
ggsave("lcfc_pilot_cfc_n16good_continua_byContrast.png", width = 25, height = 25, units = "cm")

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
  labs(x = "Step (scaled)", y = "Response time", color = "PoA for last \nsegment of \ncontext item")

# Looking only at items we want to use for LCFC experiment
df.selected <- droplevels(subset(df, context == "pocketful" | context == "questionnaire" | context == "isolate" | context == "maniac"))
df.selected <- droplevels(subset(df.selected, target_contrast == "[s/∫]____"))

df.selected.RT <- na.exclude(summarySE(df.selected, 
                                       measurevar = "reaction_time", 
                                       groupvars = c("step_scaled"), 
                                       na.rm = TRUE))
ggplot(df.selected.RT) + 
  aes(step_scaled, reaction_time) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=reaction_time-ci, ymax = reaction_time+ci, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Response time")

df.selected.RT.byContext <- na.exclude(summarySE(df.selected, 
                                       measurevar = "reaction_time", 
                                       groupvars = c("context_bias","step_scaled"), 
                                       na.rm = TRUE))
ggplot(df.selected.RT.byContext) + 
  aes(step_scaled, reaction_time, color = context_bias) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=reaction_time-se, ymax = reaction_time+se, width = 0.03)) + 
  labs(x = "Step (scaled)", y = "Response time", color = "PoA for last \nsegment of \ncontext item")

