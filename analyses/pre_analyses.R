##############################
### Run AgeDD Pre-Analyses ###
##############################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 2/4/26

# Inputs: WASI matrix reasoning, subjective perception of time, subjective value of money, and demographic data
# Assesses:
# - do WASI matrix reasoning age-adjusted t-scores change with age? (should be no!)
# - do subjective perception of time ratings change with age? (open question!)
# - do subjective value of money ratings change with age? (open question!)
# Outputs: 
# - png plot of WASI matrix reasoning age-adjusted t-score distribution
# - png plot of WASI matrix reasoning age-adjusted t-scores across age
# - txt file of WASI matrix reasoning regression
# - png plot of WASI matrix reasoning age-adjusted t-score distribution
# - png plot of WASI matrix reasoning age-adjusted t-scores across age
# - txt file of WASI matrix reasoning regression

### Set up Script -----

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr, # for %>% and other operators
       ggplot2, # for plotting 
       sjPlot, # for plot_model()
       lmerTest, # for mixed effects models
       performance, # for check_predictions()
       car) # for Anova()

# Load shared AgeDD variables
source("utilities.R")

# Set path to data
in_path <- '../data/'

# Set output path
out_path <- '../results/pre_analyses/'

# Read in data
demog <- read.csv(paste0(in_path, "demog.csv"))
wasi <- read.csv(paste0(in_path, "wasi.csv"))
subj_time <- read.csv(paste0(in_path, "subj_time.csv"))
subj_money <- read.csv(paste0(in_path, "subj_money.csv"))

# Merge demog with other data
demog_simp <- demog[, c("StudyID", "ExactAge")]
wasi <- merge(wasi, demog_simp, by = "StudyID")
subj_time <- merge(subj_time, demog_simp, by = "StudyID") 
subj_money <- merge(subj_money, demog_simp, by = "StudyID")

# Get missing data counts
length(wasi$StudyID) # N = 157
length(unique(subj_time$StudyID)) # N = 159
length(unique(subj_money$StudyID)) # N = 159

# Change variable types
str(wasi)
wasi$StudyID <- as.factor(wasi$StudyID)
str(wasi)
str(subj_time)
subj_time$StudyID <- as.factor(subj_time$StudyID)
subj_time$Length <- factor(subj_time$Length, levels = c("tomorrow", "1 week", "1 month", "3 months", "6 months", "1 year"))
str(subj_time)
str(subj_money)
subj_money$StudyID <- as.factor(subj_money$StudyID)
subj_money$Amount <- factor(subj_money$Amount, levels = c("$10", "$20", "$100", "$200"))
str(subj_money)

### Matrix Reasoning Visualizations -----

# Save matrix reasoning t-score distribution
ggplot(data = wasi, aes(x = t_score)) +
  scale_x_continuous(breaks = seq(24, 84, by = 6)) +
  geom_hline(yintercept = seq(0, 40, by = 8), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 40, by = 8)) + 
  geom_histogram(binwidth = 6, alpha = .75, fill = cust_grey, color = cust_grey) +
  labs(x = "T-Score", y = "Number of Participants") +
  agedd_theme
ggsave(paste0(out_path, "wasi_t_dist.png"), plot = last_plot(), width = 7, height = 5)

# Save matrix reasoning t-scores across age
ggplot(data = wasi, aes(x = ExactAge, y = t_score)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = seq(25, 85, by = 10), colour = 'grey90') +
  scale_y_continuous(breaks = seq(25, 85, by = 10)) +  
  geom_point(fill = cust_grey, color = cust_grey, alpha = 1, size = 1.5) +
  geom_smooth(fill = cust_grey, color = cust_grey, method = "lm") + 
  labs(x = "Age (Years)", y = "T-Score") +
  agedd_theme
ggsave(paste0(out_path, "wasi_t_age.png"), width = 7, height = 5)

### Matrix Reasoning Regression -----

# Statistically consider age-related changes in matrix reasoning t-scores (Gaussian distribution assessment)
wasi_t_age <- lm(t_score ~ ExactAge, data = wasi)
set.seed(123)
check_predictions(wasi_t_age) # Great!
qqnorm(residuals(wasi_t_age), pch = 1, frame = FALSE)
qqline(residuals(wasi_t_age), col = "red", lwd = 2) # Great!

# Save statistical model output for age-related changes in matrix reasoning t-scores (Gaussian distribution result)
summary(wasi_t_age)
plot_model(wasi_t_age, type = "pred", terms = c("ExactAge"))
# (Sanity check) matrix reasoning t-scores are stable across age
wasi_model <- paste0(out_path, 'wasi_age_lm.txt')
sink(wasi_model)
print(summary(wasi_t_age))
sink()

### Subjective Perception of Time Visualizations -----

# Save distributions of subjective perception of time ratings
ggplot(data = subj_time, aes(x = Length, y = Report, fill = Length, color = Length)) +
  geom_hline(yintercept = c(0, 100), colour = 'black', linetype = "dashed") +
  geom_hline(yintercept = seq(20, 80, 20), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 100, 20)) +  
  geom_violin(alpha = 0.25, size = 1) +
  geom_point(position = position_jitter(width = .1), alpha = .25, size = 1.5) +
  geom_boxplot(width = 0.2, alpha = .25, size = 1) +
  scale_color_manual(values = c(cust_blue, cust_pink, light_green, light_orange, cust_teal, dark_orange)) +
  scale_fill_manual(values = c(cust_blue, cust_pink, light_green, light_orange, cust_teal, dark_orange)) +
  labs(y = "How long?") +
  agedd_theme + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.6))
ggsave(paste0(out_path, 'subj_time_dist.png'), width = 7, height = 5)

# Save subjective perception of time ratings across age
subj_time$LengthTemp <- factor(subj_time$Length, levels = c("tomorrow", "3 months", "1 week", "6 months", "1 month", "1 year")) # Hack to make legend read left --> right
ggplot(data = subj_time, aes(x = ExactAge, y = Report, fill = LengthTemp, color = LengthTemp)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(0, 100), colour = 'black', linetype = "dashed") +
  geom_hline(yintercept = seq(20, 80, 20), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 100, 20)) +  
  geom_point(alpha = 1, size = 2.75) +
  geom_smooth(method = "lm", size = 1.5, alpha = .5) + 
  scale_color_manual(values = c(cust_blue, light_orange, cust_pink, cust_teal, light_green, dark_orange)) +
  scale_fill_manual(values = c(cust_blue, light_orange, cust_pink, cust_teal, light_green, dark_orange)) +
  labs(x = "Age (Years)", y = "How long?", fill = "Length", color = "Length") +
  agedd_theme
ggsave(paste0(out_path, 'figS2.png'), width = 7, height = 5)

### Subjective Perception of Time Mixed-Effects Model -----

# Model subjective perception of time ratings across age -- fit model
hist(subj_time$Report) # Gaussian distribution seems reasonable
subper_age <- lmer(Report ~ ExactAge * Length + (1|StudyID), data = subj_time)
set.seed(123)
check_predictions(subper_age) # Good enough
qqnorm(residuals(subper_age), pch = 1, frame = FALSE)
qqline(residuals(subper_age), col = "red", lwd = 2) # Good enough
# Gaussian distribution fulfills model assumptions

# Model subjective perception of time ratings across age -- interpret model
summary(subper_age)
Anova(subper_age, type = "II")
plot_model(subper_age, type = "pred", terms = c("ExactAge"))
plot_model(subper_age, type = "pred", terms = c("Length"))
plot_model(subper_age, type = "pred", terms = c("ExactAge", "Length"))
# Subjective perception of time ratings do not change with age
# Subjective perception of time ratings increase with length
# Length-based differences in subjective perception of time ratings are larger in younger participants

# Save subjective perception of time model output
subper_model <- paste0(out_path, 'subj_time_age_lmer.txt')
sink(subper_model)
cat("CATEGORICAL LENGTH\n\n")
Anova(subper_age, type = "II")
sink()

### Subjective Value of Money Visualizations -----

# Save distributions of subjective value of money ratings
ggplot(data = subj_money, aes(x = Amount, y = Report, fill = Amount, color = Amount)) +
  geom_hline(yintercept = c(0, 100), colour = 'black', linetype = "dashed") +
  geom_hline(yintercept = seq(20, 80, 20), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 100, 20)) +  
  geom_violin(alpha = 0.25, size = 1) +
  geom_point(position = position_jitter(width = .1), alpha = .25, size = 1.5) +
  geom_boxplot(width = 0.2, alpha = .25, size = 1) +
  scale_color_manual(values = c(cust_blue, light_orange, cust_pink, cust_teal)) +
  scale_fill_manual(values = c(cust_blue, light_orange, cust_pink, cust_teal)) +
  labs(y = "How much?") +
  agedd_theme + theme(legend.position = "none")
ggsave(paste0(out_path, 'subj_money_dist.png'), width = 7, height = 5)

# Save subjective value of money ratings across age
ggplot(data = subj_money, aes(x = ExactAge, y = Report, fill = Amount, color = Amount)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(0, 100), colour = 'black', linetype = "dashed") +
  geom_hline(yintercept = seq(20, 80, 20), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 100, 20)) +  
  geom_point(alpha = 1, size = 2.75) +
  geom_smooth(method = "lm", size = 1.5, alpha = .5) + 
  scale_color_manual(values = c(cust_blue, light_orange, cust_pink, cust_teal)) +
  scale_fill_manual(values = c(cust_blue, light_orange, cust_pink, cust_teal)) +
  labs(x = "Age (Years)", y = "How much?") +
  agedd_theme
ggsave(paste0(out_path, 'figS3.png'), width = 7, height = 5)

### Subjective Value of Money Mixed-Effects Model -----

# Model subjective value of money ratings across age -- fit model
hist(subj_money$Report) # Gaussian distribution seems reasonable
subval_age <- lmer(Report ~ ExactAge * Amount + (1|StudyID), data = subj_money)
set.seed(123)
check_predictions(subval_age) # Good enough
qqnorm(residuals(subval_age), pch = 1, frame = FALSE)
qqline(residuals(subval_age), col = "red", lwd = 2) # Great!
# Gaussian distribution fulfills model assumptions

# Model subjective value of money ratings across age -- interpret model
summary(subval_age)
Anova(subval_age, type = "II")
plot_model(subval_age, type = "pred", terms = c("ExactAge"))
plot_model(subval_age, type = "pred", terms = c("Amount"))
plot_model(subval_age, type = "pred", terms = c("ExactAge", "Amount"))
# Subjective value of money ratings do not change with age
# Subjective value of money ratings increase with amount
# Amount-based differences in subjective value of money ratings are consistent across age

# Save subjective value of money model output
subval_model <- paste0(out_path, 'subj_money_age_lmer.txt')
sink(subval_model)
cat("CATEGORICAL AMOUNT\n\n")
Anova(subval_age, type = "II")
sink()
