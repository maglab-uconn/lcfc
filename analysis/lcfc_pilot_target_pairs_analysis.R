# Basic R setup ----
rm(list = ls(all = TRUE))

# Load packages.
# Loading data.
library(here)
library(bit64)

# Data manipulation.
library(data.table)
library(dplyr)
library(stringr)
library(rio)
library(tidyverse)
library(janitor)
library(Rmisc)

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
file_names <- list.files(path = "./pilot_target_pairs/", pattern = "*.csv", all.files = FALSE,
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
colnames(df)[which(colnames(df)=="randomiser_7lkq")] <- "order"
colnames(df)[which(colnames(df)=="display")] <- "task"
colnames(df)[which(colnames(df)=="fname")] <- "stimulus"
colnames(df)[which(colnames(df)=="spreadsheet_name")] <- "contrast_order"
colnames(df)[which(colnames(df)=="participant_public_id")] <- "subject"

# Read in demographics
demographics <- read.csv('pilot_target_pairs/questionnaire/lcfc-pilot-targetstim-demographics.csv', header = TRUE, sep = ',', na.strings = "#N/A")
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
             contrast_order,
             spreadsheet_row,trial_number,
             reaction_time,response,task,stimulus)

# Split up stimulus
df <- separate(df, stimulus, c("front_word", "back_word", "step"), sep = "_", remove = FALSE)
df$step <- substr(df$step, 1, nchar(df$step)-4)

# Indicate which contrast subjects were hearing
df$contrast <- NA
df$contrast[which(df$response=="T" | df$response=="K")] <- "t-k"
df$contrast[which(df$response=="S" | df$response=="SH")] <- "s-sh"
df$contrast[which(df$response=="D" | df$response=="G")] <- "d-g"
df$contrast[which(df$response=="L" | df$response=="R")] <- "l-r"

# Recode responses
df$front_response <- NA
df$front_response[which(df$response=="T" | df$response=="S" | df$response=="D" | df$response=="L")] <- 1
df$front_response[which(df$response=="K" | df$response=="SH" | df$response=="G" | df$response=="R")] <- 0

# Set variable type
to.factor <- c('subject','contrast_order','task','front_word','back_word','contrast', 'sex')
df[, to.factor] <- lapply(df[, to.factor], as.factor)

to.numeric <- c('spreadsheet_row','trial_number','reaction_time','step','front_response', 'age')
df[, to.numeric] <- lapply(df[, to.numeric], as.numeric)

# Remove subjects if they never passed the headphone check
# We usually wouldn't have downloaded their data anyway, but this is a good safeguard
df <- filter(df, headphone_check == "Pass")

# Data summary ----
# Check how many subjects in each version
df_subj_cb_summary <- df %>%
  group_by(contrast_order, subject) %>% 
  dplyr::summarise(count = length(subject)) %>% 
  dplyr::summarise(count = length(count))
df_subj_cb_summary
sum(df_subj_cb_summary$count) #number of participants

# Get participant demographics
demographics <- df %>%
  group_by(subject, sex) %>% 
  dplyr::summarise(age = mean(age)) 

summary(demographics$age) #age summary
count(demographics, vars = "sex") #breakdown by sex  


# Check how many front responses made for each contrast, continuum, and step
df_resp_byCont_summary <- na.exclude(summarySE(df, 
                               measurevar = "front_response", 
                               groupvars = c("contrast","front_word","step"), 
                               na.rm = TRUE))
df_resp_byCont_summary

# Look at continuum for each item
ggplot(df_resp_byCont_summary) + 
  aes(step, front_response) + 
  facet_wrap(contrast ~ front_word, nrow = 4, ncol = 8) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step", y = "Proportion front-PoA response")

ggsave("lcfc_pilot_target_pairs_n20_continua.png", width = 20, height = 20, units = "cm")

# Check accuracy on endpoints by subject
df.endpoints <- droplevels(subset(df, step == 2 | step == 10))

df.endpoints$Acc <- NA
df.endpoints$Acc[which(df.endpoints$step==2 & df.endpoints$front_response == 1)] <- 1
df.endpoints$Acc[which(df.endpoints$step==2 & df.endpoints$front_response == 0)] <- 0
df.endpoints$Acc[which(df.endpoints$step==10 & df.endpoints$front_response == 0)] <- 1
df.endpoints$Acc[which(df.endpoints$step==10 & df.endpoints$front_response == 1)] <- 0

df.endpointAcc <- summarySE(df.endpoints,
                            measurevar = "Acc", 
                            groupvars = c("subject"), 
                            na.rm = TRUE)
summary(df.endpointAcc$Acc) # All subjects had accuracy above 80%
