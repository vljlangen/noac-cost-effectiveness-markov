#Tipping point for OAC medication main script
#by: Aleksi Kristian Winsten
#contact: alkrwi@utu.fi
#date: 08.02.2025
#last update: 2025-11-21
#University of Turku, Finland

############################################################################
############################################################################
###                                                                      ###
###           TIME DEPENDENT MARKOV DECISION PROCESS ALGORITHM           ###
###                                                                      ###
############################################################################
############################################################################

# set paths
# path to working directory where all the code files are located
# setwd("path/to/your/project/") # set your path here if not using R project
# path to create_main_data.R
 create_main_data_path <<- "create_main_data_V7.1.R"
# path to cost_sampler_V7.1.R 
 cost_sampler_path <<- "cost_sampler_V7.1.R"
# path to severity_matrix.R 
 severity_matrix_path <<- "severity_matrix.R"
# path to main_results.R
 main_results_path <<- "main_results_V7.1.R" 
# path to probabilistic_sensitivity_analysis_create_data.R
 psa_create_data_path <<- "probabilistic_sensitivity_analysis_create_data_V7.1.R"  

##################################################################
##                        Load libraries                        ##
##################################################################

# Load pacman
library(pacman)

# Load rest of packages with pacman
p_load(ggplot2, ggthemes, tibble, dplyr, showtext, magick,
       tidyr, forcats, circlize, patchwork, gt,
       ggstream, cowplot, pdftools, foreach, doParallel, doRNG, interp)

# ##################################################################
# ##                        Load libraries for UNIX               ##
# ##################################################################
# 

# library(pacman)
# p_load(ggplot2, ggthemes, tibble, dplyr, showtext, tidyr, forcats)
# p_load(patchwork, ggstream, cowplot, foreach, doParallel, doRNG)


# # The following could not be build on UNIX
# #library(magick)
# #library(circlize)
# #library(gt)
# #library(pdftools)


##################################################################
##               How to Run All the Scripts Below               ##
##################################################################

# 1. Open RStudio.
#
# 2. Create a New R Project:
#    - Go to `File` -> `New Project...`.
#    - Choose `New Directory` and then `Project`.
#    - Enter a name for your project and select a location for the project folder.
#    - Click `Create Project`.
#    - This action will create a `.Rproj` file in your project folder.
#
# 3. Place All Files in the Project Root:
#    - Move or save all relevant files (scripts mentioned below) into the project folder.
#    - RStudio will automatically set the working directory to the project root.
#
# 4. Start Your Session Correctly:
#    - Always start your R session by double-clicking the `.Rproj` file created in Step 2.
#    - This ensures that the working directory is set correctly to the project root.

############################################################################
############################################################################
###                                                                      ###
###                 THE MAIN FUNCTION THAT HAS TO BE RUN                 ###
###                                                                      ###
############################################################################
############################################################################

source(cost_sampler_path)

source(severity_matrix_path)

############################################################################
############################################################################
###                                                                      ###
###                           CREATE MAIN DATA                           ###
###                                                                      ###
############################################################################
############################################################################


#set simulation size
sim <- 10000
#set global seed for the random number generator
seed <- 46692

monthly.noac.cost <- 50

discount.factor <- TRUE

bleed.rate <- 0.0106

stroke.rate <- 0.0105

death.rate <- 0.0426

# the data frames test.no.10k and test.yes.10k contain the simulation results for no NOAC and yes NOAC policies

source(create_main_data_path)

source(main_results_path)

###########################################################################
##                       Cost-effectiveness analysis                     ##
###########################################################################

#probabilistic sensitivity
sim <- 1000
iteration <- 2000

#create data
source(psa_create_data_path)


####  beehive plot ####
beehive.df <- test[,c(3,4)]
colnames(beehive.df) <- c("QALY_diff", "Cost_diff")

beehive.means <- colMeans(beehive.df, na.rm = TRUE)
qaly_diff_mean <- beehive.means["QALY_diff"]
cost_diff_mean <- beehive.means["Cost_diff"]
cost_effectiveness_ratio <- cost_diff_mean / qaly_diff_mean
print(sprintf("Mean QALY difference in prob sens analysis: %.3f", qaly_diff_mean))
print(sprintf("Mean Cost difference in prob sens analysis: %.2f", cost_diff_mean))
print(sprintf("Cost-effectiveness ratio in prob sens analysis: %.2f", cost_effectiveness_ratio))

#################################################################

willingness_to_pay_threshold <- 50000
p_ce_acceptance <- sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * willingness_to_pay_threshold, na.rm = TRUE) / dim(beehive.df)[1]
print(sprintf("Probability of cost-effectiveness at WTP %.2f is: %.2f", willingness_to_pay_threshold, p_ce_acceptance))



# plot beehive with fixed axis limits
p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_point(alpha = 0.65, size = 2, color = "#2C7BB6") +
  geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
  labs(x = "QALY difference (With NOAC − Without NOAC)",
       y = "Cost difference (With NOAC − Without NOAC)",
       title = "Cost vs QALY differences (beehive)") +
  #scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
  #scale_y_continuous(limits = c(-2000, 4000), expand = c(0, 0)) +
  theme_minimal()

print(p)

# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")
showtext_auto()

# plot beehive with reference styling (p2)
p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_point(alpha = 0.65, size = 1.2, color = "#2C7BB6") +
  geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
  theme_classic(base_family = "rosario") +
  theme(
    axis.text = element_text(size = 14),
    axis.text.x = element_text(margin = margin(t = 8, unit = "pt")),
    axis.text.y = element_text(margin = margin(r = 8, unit = "pt")),
    axis.title = element_text(size = 17),
    plot.margin = margin(20, 20, 40, 20),
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
  ) +
  theme(
    axis.title.x = element_text(
      margin = margin(t = 16, unit = "pt")),
    axis.title.y = element_text(
      margin = margin(r = 16, unit = "pt"))
  ) +
  labs(
    x = "Incremental QALYs per patient",
    y = "Incremental cost (€) per patient",
    title = "Incremental cost-effectiveness plane"
  ) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.8, 0.8)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(-20000, -10000, 0, 10000, 20000),
                     labels = c("-20 000", "-10 000", "0", "10 000", "20 000"),
                     limits = c(-20000, 20000)) +
  annotate("text", x = 0.42, y = 16000, 
           label = "WTP 50 000 €/QALY",
           size = 4.0, color = "black", hjust = 0,
           family = "rosario") +
  annotate("segment", x = 0.41, y = 16000, 
           xend = 0.34, yend = 16000,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           color = "black", linewidth = 0.5)

print(p)

#print(p2)

# optional: save file
#ggsave("beehive_qaly_cost_scatter.png", plot = p, width = 7, height = 5, dpi = 300)

# EHRA instructions max. 1920 x 1080

# # # Save as PDF with dpi specified
ggsave("beehive_qaly_cost_scatter.pdf", dpi = 300,
       bg = "white", width = 8, height = 6)



# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter.pdf", density = 300)

# Save it as PNG
image_write(pdf_image,
            path = "beehive_qaly_cost_scatter.png",
            format = "png",
            density = 300)


# Save it as TIFF
image_write(pdf_image,
            path = "beehive_qaly_cost_scatter.tiff",
            format = "tiff",
            density = 300,
            compression = "LZW")

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter.pdf", density = 180)


# Save it as JPEG
image_write(pdf_image,
            path = "beehive_qaly_cost_scatter.jpeg",
            format = "jpeg",
            quality = 100)
