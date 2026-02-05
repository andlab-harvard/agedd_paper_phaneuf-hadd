##################################################
### Calculate AgeDD Demographic Data Summaries ###
##################################################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 2/4/26

# Inputs: demographic data
# Computes: 
# - visual summary of sample
# - statistical summary of sample
# Outputs: 
# - png plot of age-gender distribution
# - txt file of demographic summary stats

### Set up Script -----

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr) # for %>% and other operators

# Load shared AgeDD variables
source("utilities.R")

# Set path to data
in_path <- '../data/'

# Set output path
out_path <- '../results/demographics/'

# Read in demographic data
demog <- read.csv(paste0(in_path, "demog.csv"))

# Remove participants for failed catch trials
demog <- demog[!(demog$StudyID %in% c(56, 4)), ]

### Save Age-Gender Distribution -----

ggplot(data = demog, aes(x = FlooredAge, fill = Gender, color = Gender)) +
  scale_fill_manual(values = c(cust_pink, cust_blue)) + 
  scale_color_manual(values = c(cust_pink, cust_blue)) + 
  scale_x_continuous(breaks = c(10:20)) +
  geom_hline(yintercept = seq(0, 15, by = 3), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 15, by = 3)) + 
  geom_histogram(binwidth = 1, alpha = .75) +
  labs(x = "Age (Years)", y = "# of Participants") +
  agedd_theme
ggsave(paste0(out_path, "figS1.png"), plot = last_plot(), width = 5, height = 5)

### Save Demographic Summaries -----

# Write to file instead of the terminal
sink(paste0(out_path, 'demog.txt'))

cat("All Participants\n")
cat("Total N:", length(demog$StudyID), "\n")
cat("\nAGE\n")
cat("Total Children:", sum(demog$AgeGroup == "Children"), "\n")
cat("Total Adolescents:", sum(demog$AgeGroup == "Adolescents"), "\n")
cat("Total Adults:", sum(demog$AgeGroup == "Adults"), "\n")
cat("Mean Age:", mean(demog$ExactAge), "\n")
cat("SD Age:", sd(demog$ExactAge), "\n")
cat("\nGENDER\n")
cat("Total Feminine:", sum(demog$Gender == "Feminine"), "\n")
cat("Total Masculine:", sum(demog$Gender == "Masculine"), "\n")
cat("Total Not Captured by Options:", sum(demog$Gender == "Not Captured by Options"), "\n")
cat("\nRACE\n")
cat("Total American Indian or Alaska Native:", sum(demog$Race == "American Indian or Alaska Native", na.rm = TRUE), "\n")
cat("Total Asian:", sum(demog$Race == "Asian", na.rm = TRUE), "\n")
cat("Total Black or African American:", sum(demog$Race == "Black or African American", na.rm = TRUE), "\n")
cat("Total Mixed Race:", sum(demog$Race == "Mixed Race", na.rm = TRUE), "\n")
cat("Total White:", sum(demog$Race == "White", na.rm = TRUE), "\n")
cat("Total Not Captured by Options:", sum(demog$Race == "Not Captured by Options"), "\n")
cat("Total Not Reported:", sum(demog$Race == "Not Reported"), "\n")
cat("\nETHNICITY\n")
cat("Total Hispanic:", sum(demog$Ethnicity == "Hispanic"), "\n")
cat("Total Not Hispanic:", sum(demog$Ethnicity == "Not Hispanic"), "\n")
cat("Total Not Reported:", sum(demog$Ethnicity == "Not Reported"), "\n")

# Stop writing to file
sink()
