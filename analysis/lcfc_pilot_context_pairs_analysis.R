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
file_names <- list.files(path = "./pilot_context_pairs", pattern = "*.csv", all.files = FALSE,
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
colnames(df)[which(colnames(df)=="spreadsheet_name")] <- "condition"
colnames(df)[which(colnames(df)=="participant_public_id")] <- "subject"


# Read in demographics
demographics <- read.csv('pilot_context_pairs/questionnaire/lcfc-pilot-contextstim-demographics.csv', header = TRUE, sep = ',', na.strings = "#N/A")
demographics <- clean_names(demographics)
demographics <- select(demographics, participant_id, age, sex)
colnames(demographics)[which(colnames(demographics)=="participant_id")] <- "subject"

# Add demographics to main df and clean up environment
df <- merge(df, demographics, by = "subject")
rm(demographics)

# Select only rows we want
df <- dplyr::filter(df,df$trial_number != "BEGIN TASK")
df <- dplyr::filter(df,df$trial_number != "END TASK")
df <- droplevels(subset(df,zone_type=="response_keyboard_single"))

# Split up condition
df <- separate(df, condition, c("condition", NA, "order"), sep = "_", remove = TRUE)
df$condition[(which(df$condition=="nw"))] <- "nonword-nonword"
df$condition[(which(df$condition=="w"))] <- "word-nonword"

# Split up stimulus
df$stimulus <- gsub("nonword_", "", df$stimulus)
df <- separate(df, stimulus, c("word", "nonword", "step"), sep = "_", remove = FALSE)
df$step <- substr(df$step, 1, nchar(df$step)-4)

# We need to fix an error in stimulus naming for a few items (automobile, episode, isolate)
# so that words are all listed in word col and nonwords are in nonword col

# First, change step number for these items (so that all continua are from word to nonword)
df$step_renum <- NA
df$step_renum[which(df$word != "automobeer" & df$word != "episogue" & df$word != "isolake")] <- df$step[which(df$word != "automobeer" & df$word != "episogue" & df$word != "isolake")]

df$step_renum[which(df$word == "automobeer" & df$step == "001")] <- "011"
df$step_renum[which(df$word == "automobeer" & df$step == "002")] <- "010"
df$step_renum[which(df$word == "automobeer" & df$step == "003")] <- "009"
df$step_renum[which(df$word == "automobeer" & df$step == "004")] <- "008"
df$step_renum[which(df$word == "automobeer" & df$step == "005")] <- "007"
df$step_renum[which(df$word == "automobeer" & df$step == "006")] <- "006"
df$step_renum[which(df$word == "automobeer" & df$step == "007")] <- "005"
df$step_renum[which(df$word == "automobeer" & df$step == "008")] <- "004"
df$step_renum[which(df$word == "automobeer" & df$step == "009")] <- "003"
df$step_renum[which(df$word == "automobeer" & df$step == "010")] <- "002"
df$step_renum[which(df$word == "automobeer" & df$step == "011")] <- "001"

df$step_renum[which(df$word == "episogue" & df$step == "001")] <- "011"
df$step_renum[which(df$word == "episogue" & df$step == "002")] <- "010"
df$step_renum[which(df$word == "episogue" & df$step == "003")] <- "009"
df$step_renum[which(df$word == "episogue" & df$step == "004")] <- "008"
df$step_renum[which(df$word == "episogue" & df$step == "005")] <- "007"
df$step_renum[which(df$word == "episogue" & df$step == "006")] <- "006"
df$step_renum[which(df$word == "episogue" & df$step == "007")] <- "005"
df$step_renum[which(df$word == "episogue" & df$step == "008")] <- "004"
df$step_renum[which(df$word == "episogue" & df$step == "009")] <- "003"
df$step_renum[which(df$word == "episogue" & df$step == "010")] <- "002"
df$step_renum[which(df$word == "episogue" & df$step == "011")] <- "001"

df$step_renum[which(df$word == "isolake" & df$step == "001")] <- "011"
df$step_renum[which(df$word == "isolake" & df$step == "002")] <- "010"
df$step_renum[which(df$word == "isolake" & df$step == "003")] <- "009"
df$step_renum[which(df$word == "isolake" & df$step == "004")] <- "008"
df$step_renum[which(df$word == "isolake" & df$step == "005")] <- "007"
df$step_renum[which(df$word == "isolake" & df$step == "006")] <- "006"
df$step_renum[which(df$word == "isolake" & df$step == "007")] <- "005"
df$step_renum[which(df$word == "isolake" & df$step == "008")] <- "004"
df$step_renum[which(df$word == "isolake" & df$step == "009")] <- "003"
df$step_renum[which(df$word == "isolake" & df$step == "010")] <- "002"
df$step_renum[which(df$word == "isolake" & df$step == "011")] <- "001"

# Then flip word/nonword labels
df$word[which(df$word == "automobeer")] <- "automobile"
df$word[which(df$word == "episogue")] <- "episode"
df$word[which(df$word == "isolake")] <- "isolate"
df$nonword[which(df$nonword == "automobile")] <- "automobeer"
df$nonword[which(df$nonword == "episode")] <- "episogue"
df$nonword[which(df$nonword == "isolake")] <- "isolate"

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

# Remove an extra partcipant to equate cb 
df <- droplevels(subset(df, subject != "5eb109857ef2bc1ca31f94f2"))

df <- select(df, subject, age, sex, 
             headphone_check,
             condition, order, 
             spreadsheet_row, trial_number,
             reaction_time, response, front_response, task,
             contrast, word, nonword, step_renum)

# Set variable type
to.factor <- c('subject','condition', 'order', 'task','word','nonword','contrast', 'sex')
df[, to.factor] <- lapply(df[, to.factor], as.factor)

to.numeric <- c('spreadsheet_row','trial_number','reaction_time','step_renum','front_response', 'age')
df[, to.numeric] <- lapply(df[, to.numeric], as.numeric)

# Remove subjects if they never passed the headphone check
# We usually wouldn't have downloaded their data anyway, but this is a good safeguard
df <- filter(df, headphone_check == "Pass")


# Data summary ----
# Check how many subjects in each version
df_subj_cb_summary <- df %>%
  group_by(condition, order, subject) %>% 
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

# Check endpoint accuracy for each experiment ----

# First, subset to endpoints
df.endpoints <- droplevels(subset(df, step_renum == 2 | step_renum == 10))


# First, code direction of continuum
df.endpoints$continuum_bias <- NA
df.endpoints$continuum_bias[which(df.endpoints$word == "automobile" | df.endpoints$word == "confidence" | df.endpoints$word == "daffodil" | df.endpoints$word == "dangerous" | df.endpoints$word == "domicile" | df.endpoints$word == 
                                    "episode" | df.endpoints$word == "favorite" | df.endpoints$word == "generate" | df.endpoints$word == "inherit" | df.endpoints$word == "invalid" | df.endpoints$word == 
                                    "isolate" | df.endpoints$word == "juvenile" | df.endpoints$word == "lemonade" | df.endpoints$word == "nightingale" | df.endpoints$word == "overboard" | df.endpoints$word == 
                                    "paranoid" | df.endpoints$word == "pocketful" | df.endpoints$word == "religious" | df.endpoints$word == "universe")] <- "front-back"
df.endpoints$continuum_bias[which(df.endpoints$word == "abolish" | df.endpoints$word == "analogue" | df.endpoints$word == "atmosphere" | df.endpoints$word == "catalogue" | df.endpoints$word == "ceramic" | df.endpoints$word == 
                                    "demolish" | df.endpoints$word == "dialogue" | df.endpoints$word == "epilogue" | df.endpoints$word == "macintosh" | df.endpoints$word == "maniac" | df.endpoints$word == 
                                    "metaphor" | df.endpoints$word == "monologue" | df.endpoints$word == "organic" | df.endpoints$word == "outlandish" | df.endpoints$word == "overlook" | df.endpoints$word == 
                                    "questionnaire")] <- "back-front"



df.endpoints$Acc <- NA
df.endpoints$Acc[which(df.endpoints$continuum_bias == "front-back" & 
                         df.endpoints$step_renum == 2 & 
                         df.endpoints$front_response == 1)] <- 1
df.endpoints$Acc[which(df.endpoints$continuum_bias == "front-back" & 
                         df.endpoints$step_renum == 2 & 
                         df.endpoints$front_response == 0)] <- 0
df.endpoints$Acc[which(df.endpoints$continuum_bias == "front-back" & 
                         df.endpoints$step_renum == 10 & 
                         df.endpoints$front_response == 0)] <- 1
df.endpoints$Acc[which(df.endpoints$continuum_bias == "front-back" & 
                         df.endpoints$step_renum == 10 & 
                         df.endpoints$front_response == 1)] <- 0

df.endpoints$Acc[which(df.endpoints$continuum_bias == "back-front" & 
                         df.endpoints$step_renum == 2 & 
                         df.endpoints$front_response == 0)] <- 1
df.endpoints$Acc[which(df.endpoints$continuum_bias == "back-front" & 
                         df.endpoints$step_renum == 2 & 
                         df.endpoints$front_response == 1)] <- 0
df.endpoints$Acc[which(df.endpoints$continuum_bias == "back-front" & 
                         df.endpoints$step_renum == 10 & 
                         df.endpoints$front_response == 1)] <- 1
df.endpoints$Acc[which(df.endpoints$continuum_bias == "back-front" & 
                         df.endpoints$step_renum == 10 & 
                         df.endpoints$front_response == 0)] <- 0


# Accuracy on nonword-nonword pilot
df.endpointAcc.nn <- summarySE(droplevels(subset(df.endpoints, condition == "nonword-nonword")),
                               measurevar = "Acc", 
                               groupvars = c("subject"), 
                               na.rm = TRUE)
summary(df.endpointAcc.nn$Acc) 

exclude <- levels(droplevels(df.endpointAcc.nn$subject[which(df.endpointAcc.nn$Acc < 0.8)]))

# Accuracy on word-nonword pilot
df.endpointAcc.wn <- summarySE(droplevels(subset(df.endpoints, condition == "word-nonword")),
                               measurevar = "Acc", 
                               groupvars = c("subject"), 
                               na.rm = TRUE)
summary(df.endpointAcc.wn$Acc) 

exclude <- union(exclude, levels(droplevels(df.endpointAcc.wn$subject[which(df.endpointAcc.wn$Acc < 0.8)])))

# Drop subjects with less than 80% acc on endpoints
df <- droplevels(df %>% filter(!subject %in% exclude))

# Summarize data ----

# Summarize data for nonword-nonword pilot
df_nn_byCont_summary <- na.exclude(summarySE(droplevels(subset(df, condition == "nonword-nonword")), 
                                               measurevar = "front_response", 
                                               groupvars = c("contrast","word","step_renum"), 
                                               na.rm = TRUE))
ggplot(df_nn_byCont_summary) + 
  aes(step_renum, front_response) + 
  facet_wrap(contrast ~ word, nrow = 5, ncol = 7) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step", y = "Proportion front-PoA response")

ggsave("lcfc_pilot_context_pairs_nonwords_n20_continua.png", width = 30, height = 30, units = "cm")


# Summarize data for word-nonword pilot
df_wn_byCont_summary <- na.exclude(summarySE(droplevels(subset(df, condition == "word-nonword")), 
                                            measurevar = "front_response", 
                                            groupvars = c("contrast","word","step_renum"), 
                                            na.rm = TRUE))
ggplot(df_wn_byCont_summary) + 
  aes(step_renum, front_response) + 
  facet_wrap(contrast ~ word, nrow = 5, ncol = 7) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step", y = "Proportion front-PoA response")

ggsave("lcfc_pilot_context_pairs_ganong_n20_continua.png", width = 30, height = 30, units = "cm")

# Plot with both pilots together
df_byCont_summary <- na.exclude(summarySE(df, 
                                             measurevar = "front_response", 
                                             groupvars = c("condition","contrast","word","step_renum"), 
                                             na.rm = TRUE))
ggplot(df_byCont_summary) + 
  aes(step_renum, front_response, color = condition) + 
  facet_wrap(contrast ~ word, nrow = 5, ncol = 7) +
  geom_point() + 
  geom_line() +   
  geom_errorbar(aes(ymin=front_response-ci, ymax = front_response+ci, width = 0.03)) + 
  labs(x = "Step", y = "Proportion front-PoA response") +
  theme(legend.position = "bottom")

ggsave("lcfc_pilot_context_pairs_n33goodsubj_continua.png", width = 30, height = 30, units = "cm")


