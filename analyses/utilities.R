#########################################
### Define Shared Variables for AgeDD ###
#########################################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 2/4/26

##########################
### Plotting Utilities ###
##########################

# Set custom color scheme
cust_grey <- "#A6A6A6"
cust_purple <- "#94849B"
cust_blue <- "#597590"
cust_teal <- "#43AB8B"
light_green <- "#90BF6D"
dark_green <- "#345511"
light_orange <- "#FF9300"
dark_orange <- "#C04F15"
cust_pink <- "#FF95B1"
# Note: grey90 is used for background plotting features

# Set theme
agedd_theme <- theme(title = element_text(size = 24, face = "bold", family = "Avenir"),
                     plot.title = element_text(hjust = .5),
                     axis.title.x = element_text(size = 24, family = "Avenir"),
                     axis.title.y = element_text(size = 24, family = "Avenir"),
                     axis.text.x = element_text(size = 18, colour = "black", family = "Avenir"),
                     axis.text.y = element_text(size = 18, colour = "black", family = "Avenir"),
                     legend.text = element_text(size = 18, colour = "black", family = "Avenir"),
                     legend.position = "bottom",
                     legend.key = element_rect(fill = "transparent", color = NA),
                     strip.text.x = element_text(size = 18, colour = "black", family = "Avenir"),
                     strip.text.y = element_text(size = 18, colour = "black", family = "Avenir"),
                     panel.grid.major = element_blank(), # Remove grid marks
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))
