#Tipping point for OAC medication main script
#by: Aleksi Kristian Winsten
#contact: alkrwi@utu.fi
#date: 08.02.2025
#last update: 2026-03-09
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
# setwd("path/to/your/project/") # set your path here if not using an RStudio Project (.Rproj)
# path to create_main_data.R
 create_main_data_path <<- "create_main_data_V8.1.R"
# path to cost_sampler_V4.R 
 cost_sampler_path <<- "cost_sampler_V8.1.R"
# path to severity_matrix.R 
 severity_matrix_path <<- "severity_matrix.R"
# path to main_results.R
 main_results_path <<- "main_results_V8.1.R" 
# path to probabilistic_sensitivity_analysis_create_data.R
 psa_create_data_path <<- "probabilistic_sensitivity_analysis_create_data_V8.1.R"  

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


# # The following could not be built on UNIX
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

sink("main_analysis_results.txt")
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
# one-way sens indicators
change.bleed.distribution <- FALSE
noac.effect <- FALSE

## basic parameters ##

monthly.noac.cost <- 50

discount.factor <- TRUE

bleed.rate <- 0.0106

stroke.rate <- 0.0105

death.rate <- 0.0426

d.value <- 0.03 #discount value

# the data frames test.no.10k and test.yes.10k contain the simulation results for no NOAC and yes NOAC policies

source(create_main_data_path)

source(main_results_path)

sink()


########################################
##    One-way sensitivity analysis    ##
########################################
table.df <- data.frame(row.name = character(),
                       name = character(), 
                       value = character()
                       )
#sink("oneway_sensitivity_analysis.txt")
print("stroke rate 50% higher")
stroke.rate <- 0.0105 + 0.0105/2
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Stroke rate",
  name = sprintf("%d%% higher", round(abs(stroke.rate/0.0105-1)*100, digits = 0)),
  value = cpQg))
stroke.rate <- 0.0105
print("")

print("stroke rate 50% lower")
stroke.rate <- 0.0105 - 0.0105/2
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Stroke rate",
  name = sprintf("%d%% lower", round(abs(stroke.rate/0.0105-1)*100, digits = 0)),
  value = cpQg))
stroke.rate <- 0.0105
print("")

print("bleed rate 50% higher")
bleed.rate <- 0.0106 + 0.0106/2
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Bleed rate",
  name = sprintf("%d%% higher", round(abs(bleed.rate/0.0106-1)*100, digits = 0)),
  value = cpQg))
bleed.rate <- 0.0106
print("")

print("bleed rate 50% lower")
bleed.rate <- 0.0106 - 0.0106/2
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Bleed rate",
  name = sprintf("%d%% lower", round(abs(bleed.rate/0.0106-1)*100, digits = 0)),
  value = cpQg))
bleed.rate <- 0.0106
print("")

print("death rate 50% higher")
death.rate <- 0.0426 + 0.0426/2
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Death rate",
  name = sprintf("%d%% higher", round(abs(death.rate/0.0426-1)*100, digits = 0)),
  value = cpQg))
death.rate <- 0.0426
print("")

print("death rate 50% lower")
death.rate <- 0.0426 - 0.0426/2
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Death rate",
  name = sprintf("%d%% lower", round(abs(death.rate/0.0426-1)*100, digits = 0)),
  value = cpQg))
death.rate <- 0.0426
print("")

print("70% proportion of bleed are non-intracranial")
change.bleed.distribution <- TRUE
bleed.distribution <- c(0.15, 0.15, 0.7) #(ICH, Subdural, other bleeding)
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Proportion of NOAC caused bleeds are ICH",
  name = sprintf("%d%% lower", round(abs(0.70/0.80-1)*100, digits = 0)),
  value = cpQg))
change.bleed.proportions <- FALSE
print("")

print("90% proportion of bleeds are non-intracranial")
change.bleed.distribution <- TRUE
bleed.distribution <- c(0.05, 0.05, 0.9) #(ICH, Subdural, other bleeding)
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Proportion of NOAC caused bleeds are ICH",
  name = sprintf("%d%% higher", round((0.90/0.80-1)*100, digits = 0)),
  value = cpQg))
change.bleed.proportions <- FALSE
print("")

print("noac effect to stroke 0.79")
noac.effect <- TRUE
oneway.effectCoefficients <- c(Stroke = 0.79, Bleed = 1.62)
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "NOAC Relative Risk",
  name = sprintf("%d%% higher on stroke", round((0.79/0.68-1*100), digits = 0)),
  value = cpQg))
noac.effect <- FALSE
print("")

print("noac effect to bleed 2.10")
noac.effect <- TRUE
oneway.effectCoefficients <- c(Stroke = 0.68, Bleed = 2.10)
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "NOAC Relative Risk",
  name = sprintf("%d%% higher on bleeds", round(abs(2.10/1.62-1)*100, digits = 0)),
  value = cpQg))
noac.effect <- FALSE
print("")

print("noac effect to stroke 0.62")
noac.effect <- TRUE
oneway.effectCoefficients <- c(Stroke = 0.62, Bleed = 1.62)
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "NOAC Relative Risk",
  name = sprintf("%d%% lower on stroke", round(abs(0.62/0.68-1)*100, digits = 0)),
  value = cpQg))
noac.effect <- FALSE
print("")

print("noac effect to bleed 1.36")
noac.effect <- TRUE
oneway.effectCoefficients <- c(Stroke = 0.68, Bleed = 1.36)
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "NOAC Relative Risk",
  name = sprintf("%d%% lower on bleeds", round(abs(1.36/1.62-1)*100, digits = 0)),
  value = cpQg))
noac.effect <- FALSE
print("")

print("0% discount")
d.value <- 0
source(create_main_data_path)
source(main_results_path)
d.value <- 0.03
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Discounting rate",
  name = "0%",
  value = cpQg))
print("")

print("5% discount")
d.value <- 0.05
source(create_main_data_path)
source(main_results_path)
d.value <- 0.03
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Discounting rate",
  name = "5%",
  value = cpQg))
print("")

#sink()

## extra sensitivity analyses ##
#sink("extra_sensitivity_analyses.txt")
print("DOAC cost 30e")
monthly.noac.cost <- 30
source(create_main_data_path)
source(main_results_path)
monthly.noac.cost <- 50
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "DOAC cost",
  name = "30€",
  value = cpQg))
print("")

print("DOAC cost 70e")
monthly.noac.cost <- 70
source(create_main_data_path)
source(main_results_path)
monthly.noac.cost <- 50
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "DOAC cost",
  name = "70€",
  value = cpQg))
print("")

print("stroke 0.019, bleed 0.017, death 0.085")
stroke.rate <- 0.019
bleed.rate <- 0.017
death.rate <- 0.085
source(create_main_data_path)
source(main_results_path)
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "Base variables",
  name = "Stroke, Bleed, and Death rates higher",
  value = cpQg))
stroke.rate <- 0.0105
bleed.rate <- 0.0106
death.rate <- 0.0426
print("")

print("noac effect to stroke 0.79 AND noac effect to bleed 2.10")
noac.effect <- TRUE
oneway.effectCoefficients <- c(Stroke = 0.79, Bleed = 2.10)
source(create_main_data_path)
source(main_results_path)
noac.effect <- FALSE
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "NOAC effect on stroke and bleeds",
  name = "both higher",
  value = cpQg))
print("")

print("noac effect to stroke 0.62 AND noac effect to bleed 1.36")
noac.effect <- TRUE
oneway.effectCoefficients <- c(Stroke = 0.62, Bleed = 1.36)
source(create_main_data_path)
source(main_results_path)
noac.effect <- FALSE
cpQg <- ifelse(icer < 0, 
               ifelse(inc.cost < 0, "Dominant", "Dominated"), 
               sprintf("€ %.02f", icer)) #cost per qaly gained
table.df <- rbind(table.df, data.frame(
  row.name = "NOAC effect on stroke and bleeds",
  name = "both lower",
  value = cpQg))
print("")

#sink()

gt_table <- gt(table.df, groupname_col = "row.name") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  cols_label(
    name = "",
    value = "Cost per QALY gained (€)"
  ) %>%
  tab_footnote(
    footnote = "Base values per 100 patient years: stroke 1.05, bleed 1.06, death 4.26.",
    locations = cells_body(columns = name, rows = 1:3)
  ) %>%
  tab_footnote(
    footnote = "Base proportion of extra major bleeds from all bleeding events caused by NOAC is 80%",
    locations = cells_body(columns = name, rows = 4:5)
  ) %>%
  tab_footnote(
    footnote = "Base Relative Risk is for stroke 0.68 and for bleed 1.62.",
    locations = cells_body(columns = name, rows = 6:9)
  ) %>%
  tab_footnote(
    footnote = "Stroke, Bleed, and Death rates are equally higher than in the one-way sensitivity analysis.",
    locations = cells_body(columns = name, rows = 12)
  ) %>%
  tab_footnote(
    footnote = "NOAC effect on stroke and bleed is equally highre (or lower) than in the one-way sensitivity analysis.",
    locations = cells_body(columns = name, rows = 13:14)
  )
  

# show the gt table
gt_table
gtsave(gt_table, "table3.html")
gtsave(gt_table, "table3.pdf")
gtsave(gt_table, "table3.tex")
################
##  TABLE 3 ####
################



###########################################################################
##                       Cost-effectiveness analysis                     ##
###########################################################################
# CHAD2DS2-VASc < 4 PSA
# probabilistic sensitivity
sim <- 1000
iteration <- 2000
###log-normal parameters
# assuming geometric mean given and its 95% CI
stroke_mean <- log(0.87)
bleed_mean <- log(1.27)
stroke_sd <- (log(1.52)-stroke_mean)/1.96
bleed_sd <- (log(2.03)-bleed_mean)/1.96
death_mean <- log(1.08)
death_sd <- (log(1.21)-death_mean)/1.96

#create data
source(psa_create_data_path)

####  beehive plot ####
sink("CHAD2DS2-VASc_less_than_4_PSA.txt")
print("CHAD2DS2-VASc < 4 PSA")
print(sprintf("The NOAC was beneficial in the %.2f%% of the cases", 100*(sum(test[,3]>0) / length(test[,3]))))

beehive.df <- test[,c(3,4)]
colnames(beehive.df) <- c("QALY_diff", "Cost_diff")

beehive.means <- colMeans(beehive.df, na.rm = TRUE)
qaly_diff_mean <- beehive.means["QALY_diff"]
cost_diff_mean <- beehive.means["Cost_diff"]
cost_effectiveness_ratio <- cost_diff_mean / qaly_diff_mean
print(sprintf("Mean QALY difference in prob sens analysis: %.3f", qaly_diff_mean))
print(sprintf("Mean Cost difference in prob sens analysis: %.2f", cost_diff_mean))
print(sprintf("Cost-effectiveness ratio in prob sens analysis: %.2f", cost_effectiveness_ratio))

willingness_to_pay_threshold <- 50000
p_dominated <- sum(beehive.df["Cost_diff"]>0 & beehive.df["QALY_diff"] < 0)/dim(beehive.df)[1] #calculates proportion that the treatment caused negative qaly and posivite costs
p_ce_acceptance <- sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * willingness_to_pay_threshold, na.rm = TRUE) / dim(beehive.df)[1]
print(sprintf("Probability of cost-effectiveness at WTP %.2f is: %.2f", willingness_to_pay_threshold, p_ce_acceptance))
sink()
## Cost-Effectiveness Acceptability Curve ##

wtp_vector <- seq(0, 200000, by = 10000)
p_ce_acceptance_vector <- sapply(wtp_vector, function(wtp) {
  sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * wtp, na.rm = TRUE) / dim(beehive.df)[1]
})
# CEAC plot
ceac_df_less_4 <- data.frame(
  wtp = wtp_vector,
  p_ce_acceptance = p_ce_acceptance_vector
)

ceac_less_4 <- ggplot(ceac_df_less_4, aes(x = wtp, y = p_ce_acceptance)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Willingness-to-pay threshold",
    y = "Probability cost-effective (acceptance)",
    title = "Cost-Effectiveness Acceptability Curve (CEAC)"
  ) +
  theme_minimal()

#####################################
##    Cost-Effectiveness Plane     ##
#####################################
saveRDS(beehive.df, file = "beehive.df_CHAD2DS2-VASc_less_than_4.rds")
# # plot beehive with fixed axis limits
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   labs(x = "QALY difference (With NOAC − Without NOAC)",
#        y = "Cost difference (With NOAC − Without NOAC)",
#        title = "Cost vs QALY differences (beehive)") +
#   #scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
#   #scale_y_continuous(limits = c(-2000, 4000), expand = c(0, 0)) +
#   theme_minimal()
# 
# print(p)
# 
# # Load specific font from Google Fonts
# #font_add_google("Rosario", family = "rosario")
# #showtext_auto()
# 
# # plot beehive with reference styling (p2)
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 1.2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   theme_classic(base_family = "rosario") +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.text.x = element_text(margin = margin(t = 8, unit = "pt")),
#     axis.text.y = element_text(margin = margin(r = 8, unit = "pt")),
#     axis.title = element_text(size = 17),
#     plot.margin = margin(20, 20, 40, 20),
#     axis.title.x = element_text(face = "bold"), 
#     axis.title.y = element_text(face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
#     panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
#   ) +
#   theme(
#     axis.title.x = element_text(
#       margin = margin(t = 16, unit = "pt")),
#     axis.title.y = element_text(
#       margin = margin(r = 16, unit = "pt"))
#   ) +
#   labs(
#     x = "Incremental QALYs per patient",
#     y = "Incremental cost (€) per patient",
#     title = "Incremental cost-effectiveness plane"
#   ) +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.8, 0.8)) +
#   scale_y_continuous(expand = c(0, 0),
#                      breaks = c(-20000, -10000, 0, 10000, 20000),
#                      labels = c("-20 000", "-10 000", "0", "10 000", "20 000"),
#                      limits = c(-20000, 20000)) +
#   annotate("text", x = 0.42, y = 16000, 
#            label = "WTP 50 000 €/QALY",
#            size = 4.0, color = "black", hjust = 0,
#            family = "rosario") +
#   annotate("segment", x = 0.41, y = 16000, 
#            xend = 0.34, yend = 16000,
#            arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#            color = "black", linewidth = 0.5)
# 
# print(p)
# 
# 
# # # # Save as PDF with dpi specified
# ggsave("beehive_qaly_cost_scatter_CHA2DS2VASc_below_4.pdf", dpi = 300,
#        bg = "white", width = 8, height = 6)
# 
# 
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_below_4.pdf", density = 300)
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_below_4.png",
#             format = "png",
#             density = 300)
# 
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_below_4.tiff",
#             format = "tiff",
#             density = 300,
#             compression = "LZW")
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_below_4.pdf", density = 180)
# 
# 
# # Save it as JPEG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_below_4.jpeg",
#             format = "jpeg",
#             quality = 100)
# 

################################################################
# CHAD2DS2-VASc = 4 PSA
# probabilistic sensitivity
sim <- 1000
iteration <- 2000
###log-normal parameters
# assuming geometric mean given and its 95% CI
stroke_mean <- log(0.63)
bleed_mean <- log(1.31)
stroke_sd <- (log(1.27)-stroke_mean)/1.96
bleed_sd <- (log(2.29)-bleed_mean)/1.96
death_mean <- log(1.08)
death_sd <- (log(1.21)-death_mean)/1.96

#create data
source(psa_create_data_path)

####  beehive plot ####
sink("CHAD2DS2-VASc_equal_4_PSA.txt")
print("CHAD2DS2-VASc = 4 PSA")
print(sprintf("The NOAC was beneficial in the %.2f%% of the cases", 100*(sum(test[,3]>0) / length(test[,3]))))

beehive.df <- test[,c(3,4)]
colnames(beehive.df) <- c("QALY_diff", "Cost_diff")

beehive.means <- colMeans(beehive.df, na.rm = TRUE)
qaly_diff_mean <- beehive.means["QALY_diff"]
cost_diff_mean <- beehive.means["Cost_diff"]
cost_effectiveness_ratio <- cost_diff_mean / qaly_diff_mean
print(sprintf("Mean QALY difference in prob sens analysis: %.3f", qaly_diff_mean))
print(sprintf("Mean Cost difference in prob sens analysis: %.2f", cost_diff_mean))
print(sprintf("Cost-effectiveness ratio in prob sens analysis: %.2f", cost_effectiveness_ratio))

willingness_to_pay_threshold <- 50000
p_ce_acceptance <- sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * willingness_to_pay_threshold, na.rm = TRUE) / dim(beehive.df)[1]
print(sprintf("Probability of cost-effectiveness at WTP %.2f is: %.2f", willingness_to_pay_threshold, p_ce_acceptance))
sink()

## Cost-Effectiveness Acceptability Curve ##

wtp_vector <- seq(0, 200000, by = 10000)
p_ce_acceptance_vector <- sapply(wtp_vector, function(wtp) {
  sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * wtp, na.rm = TRUE) / dim(beehive.df)[1]
})
# CEAC plot
ceac_df_equal_4 <- data.frame(
  wtp = wtp_vector,
  p_ce_acceptance = p_ce_acceptance_vector
)

ceac_equal_4 <- ggplot(ceac_df_equal_4, aes(x = wtp, y = p_ce_acceptance)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Willingness-to-pay threshold",
    y = "Probability cost-effective (acceptance)",
    title = "Cost-Effectiveness Acceptability Curve (CEAC)"
  ) +
  theme_minimal()

#####################################
##    Cost-Effectiveness Plane     ##
#####################################
saveRDS(beehive.df, file = "beehive.df_CHAD2DS2-VASc_equal_4.rds")
# # plot beehive with fixed axis limits
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   labs(x = "QALY difference (With NOAC − Without NOAC)",
#        y = "Cost difference (With NOAC − Without NOAC)",
#        title = "Cost vs QALY differences (beehive)") +
#   #scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
#   #scale_y_continuous(limits = c(-2000, 4000), expand = c(0, 0)) +
#   theme_minimal()
# 
# print(p)
# 
# # Load specific font from Google Fonts
# #font_add_google("Rosario", family = "rosario")
# #showtext_auto()
# 
# # plot beehive with reference styling (p2)
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 1.2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   theme_classic(base_family = "rosario") +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.text.x = element_text(margin = margin(t = 8, unit = "pt")),
#     axis.text.y = element_text(margin = margin(r = 8, unit = "pt")),
#     axis.title = element_text(size = 17),
#     plot.margin = margin(20, 20, 40, 20),
#     axis.title.x = element_text(face = "bold"), 
#     axis.title.y = element_text(face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
#     panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
#   ) +
#   theme(
#     axis.title.x = element_text(
#       margin = margin(t = 16, unit = "pt")),
#     axis.title.y = element_text(
#       margin = margin(r = 16, unit = "pt"))
#   ) +
#   labs(
#     x = "Incremental QALYs per patient",
#     y = "Incremental cost (€) per patient",
#     title = "Incremental cost-effectiveness plane"
#   ) +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.8, 0.8)) +
#   scale_y_continuous(expand = c(0, 0),
#                      breaks = c(-20000, -10000, 0, 10000, 20000),
#                      labels = c("-20 000", "-10 000", "0", "10 000", "20 000"),
#                      limits = c(-20000, 20000)) +
#   annotate("text", x = 0.42, y = 16000, 
#            label = "WTP 50 000 €/QALY",
#            size = 4.0, color = "black", hjust = 0,
#            family = "rosario") +
#   annotate("segment", x = 0.41, y = 16000, 
#            xend = 0.34, yend = 16000,
#            arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#            color = "black", linewidth = 0.5)
# 
# print(p)
# 
# 
# # # # Save as PDF with dpi specified
# ggsave("beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4.pdf", dpi = 300,
#        bg = "white", width = 8, height = 6)
# 
# 
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4.pdf", density = 300)
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4.png",
#             format = "png",
#             density = 300)
# 
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4.tiff",
#             format = "tiff",
#             density = 300,
#             compression = "LZW")
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4.pdf", density = 180)
# 
# 
# # Save it as JPEG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4.jpeg",
#             format = "jpeg",
#             quality = 100)

##################################################################
# CHAD2DS2-VASc > 4 PSA
# probabilistic sensitivity
sim <- 1000
iteration <- 2000
###log-normal parameters
# assuming geometric mean given and its 95% CI
stroke_mean <- log(0.44)
bleed_mean <- log(1.48)
stroke_sd <- (log(0.77)-stroke_mean)/1.96
bleed_sd <- (log(2.45)-bleed_mean)/1.96
death_mean <- log(1.08)
death_sd <- (log(1.21)-death_mean)/1.96

#create data
source(psa_create_data_path)

####  beehive plot ####
sink("CHAD2DS2-VASc_more_than_4_PSA.txt")
print("CHAD2DS2-VASc > 4 PSA")
print(sprintf("The NOAC was beneficial in the %.2f%% of the cases", 100*(sum(test[,3]>0) / length(test[,3]))))

beehive.df <- test[,c(3,4)]
colnames(beehive.df) <- c("QALY_diff", "Cost_diff")

beehive.means <- colMeans(beehive.df, na.rm = TRUE)
qaly_diff_mean <- beehive.means["QALY_diff"]
cost_diff_mean <- beehive.means["Cost_diff"]
cost_effectiveness_ratio <- cost_diff_mean / qaly_diff_mean
print(sprintf("Mean QALY difference in prob sens analysis: %.3f", qaly_diff_mean))
print(sprintf("Mean Cost difference in prob sens analysis: %.2f", cost_diff_mean))
print(sprintf("Cost-effectiveness ratio in prob sens analysis: %.2f", cost_effectiveness_ratio))

willingness_to_pay_threshold <- 50000
p_ce_acceptance <- sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * willingness_to_pay_threshold, na.rm = TRUE) / dim(beehive.df)[1]
print(sprintf("Probability of cost-effectiveness at WTP %.2f is: %.2f", willingness_to_pay_threshold, p_ce_acceptance))
sink()

## Cost-Effectiveness Acceptability Curve ##

wtp_vector <- seq(0, 200000, by = 10000)
p_ce_acceptance_vector <- sapply(wtp_vector, function(wtp) {
  sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * wtp, na.rm = TRUE) / dim(beehive.df)[1]
})
# CEAC plot
ceac_df_more_4 <- data.frame(
  wtp = wtp_vector,
  p_ce_acceptance = p_ce_acceptance_vector
)

ceac_more_4 <- ggplot(ceac_df_more_4, aes(x = wtp, y = p_ce_acceptance)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Willingness-to-pay threshold",
    y = "Probability cost-effective (acceptance)",
    title = "Cost-Effectiveness Acceptability Curve (CEAC)"
  ) +
  theme_minimal()

#####################################
##    Cost-Effectiveness Plane     ##
#####################################
saveRDS(beehive.df, file = "beehive.df_CHAD2DS2-VASc_more_than_4.rds")
# # plot beehive with fixed axis limits
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   labs(x = "QALY difference (With NOAC − Without NOAC)",
#        y = "Cost difference (With NOAC − Without NOAC)",
#        title = "Cost vs QALY differences (beehive)") +
#   #scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
#   #scale_y_continuous(limits = c(-2000, 4000), expand = c(0, 0)) +
#   theme_minimal()
# 
# print(p)
# 
# # Load specific font from Google Fonts
# #font_add_google("Rosario", family = "rosario")
# #showtext_auto()
# 
# # plot beehive with reference styling (p2)
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 1.2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   theme_classic(base_family = "rosario") +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.text.x = element_text(margin = margin(t = 8, unit = "pt")),
#     axis.text.y = element_text(margin = margin(r = 8, unit = "pt")),
#     axis.title = element_text(size = 17),
#     plot.margin = margin(20, 20, 40, 20),
#     axis.title.x = element_text(face = "bold"), 
#     axis.title.y = element_text(face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
#     panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
#   ) +
#   theme(
#     axis.title.x = element_text(
#       margin = margin(t = 16, unit = "pt")),
#     axis.title.y = element_text(
#       margin = margin(r = 16, unit = "pt"))
#   ) +
#   labs(
#     x = "Incremental QALYs per patient",
#     y = "Incremental cost (€) per patient",
#     title = "Incremental cost-effectiveness plane"
#   ) +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.8, 0.8)) +
#   scale_y_continuous(expand = c(0, 0),
#                      breaks = c(-20000, -10000, 0, 10000, 20000),
#                      labels = c("-20 000", "-10 000", "0", "10 000", "20 000"),
#                      limits = c(-20000, 20000)) +
#   annotate("text", x = 0.42, y = 16000, 
#            label = "WTP 50 000 €/QALY",
#            size = 4.0, color = "black", hjust = 0,
#            family = "rosario") +
#   annotate("segment", x = 0.41, y = 16000, 
#            xend = 0.34, yend = 16000,
#            arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#            color = "black", linewidth = 0.5)
# 
# print(p)
# 
# 
# # # # Save as PDF with dpi specified
# ggsave("beehive_qaly_cost_scatter_CHA2DS2VASc_above_4.pdf", dpi = 300,
#        bg = "white", width = 8, height = 6)
# 
# 
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_above_4.pdf", density = 300)
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_above_4.png",
#             format = "png",
#             density = 300)
# 
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_above_4.tiff",
#             format = "tiff",
#             density = 300,
#             compression = "LZW")
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_above_4.pdf", density = 180)
# 
# 
# # Save it as JPEG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_above_4.jpeg",
#             format = "jpeg",
#             quality = 100)
#################################################################


## MAIN PROBABILISTIC SENSITIVITY ANALYSIS FOR SUBCLINICAL AF ##
#probabilistic sensitivity
sim <- 1000
iteration <- 2000
###log-normal parameters
# assuming geometric mean given and its 95% CI
stroke_mean <- log(0.68)
bleed_mean <- log(1.62)
stroke_sd <- (log(0.92)-stroke_mean)/1.96
bleed_sd <- (log(2.5)-bleed_mean)/1.96
death_mean <- log(1.08)
death_sd <- (log(1.21)-death_mean)/1.96

#create data
source(psa_create_data_path)


####  beehive plot ####
sink("MAIN PRBOABILITY SENSITIVITY ANALYSIS FOR SUBCLINICAL AF.txt")
print("MAIN PRBOABILITY SENSITIVITY ANALYSIS FOR SUBCLINICAL AF")
print(sprintf("The NOAC was beneficial in the %.2f%% of the cases", 100*(sum(test[,3]>0) / length(test[,3]))))

beehive.df <- test[,c(3,4)]
colnames(beehive.df) <- c("QALY_diff", "Cost_diff")

beehive.means <- colMeans(beehive.df, na.rm = TRUE)
qaly_diff_mean <- beehive.means["QALY_diff"]
cost_diff_mean <- beehive.means["Cost_diff"]
cost_effectiveness_ratio <- cost_diff_mean / qaly_diff_mean
print(sprintf("Mean QALY difference in prob sens analysis: %.3f", qaly_diff_mean))
print(sprintf("Mean Cost difference in prob sens analysis: %.2f", cost_diff_mean))
print(sprintf("Cost-effectiveness ratio in prob sens analysis: %.2f", cost_effectiveness_ratio))

willingness_to_pay_threshold <- 50000
p_ce_acceptance <- sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * willingness_to_pay_threshold, na.rm = TRUE) / dim(beehive.df)[1]
print(sprintf("Probability of cost-effectiveness at WTP %.2f is: %.2f", willingness_to_pay_threshold, p_ce_acceptance))
sink()

#################################################################
## Cost-Effectiveness Acceptability Curve ##

wtp_vector <- seq(0, 200000, by = 10000)
p_ce_acceptance_vector <- sapply(wtp_vector, function(wtp) {
  sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * wtp, na.rm = TRUE) / dim(beehive.df)[1]
})
# CEAC plot
ceac_df <- data.frame(
  wtp = wtp_vector,
  p_ce_acceptance = p_ce_acceptance_vector
)

ceac <- ggplot(ceac_df, aes(x = wtp, y = p_ce_acceptance)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Willingness-to-pay threshold",
    y = "Probability cost-effective (acceptance)",
    title = "Cost-Effectiveness Acceptability Curve (CEAC)"
  ) +
  theme_minimal()

print(ceac)
################################################################

#####################################
##    Cost-Effectiveness Plane     ##
#####################################
saveRDS(beehive.df, file = "beehive.df_main_PSA.rds")
# # plot beehive with fixed axis limits
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   labs(x = "QALY difference (With NOAC − Without NOAC)",
#        y = "Cost difference (With NOAC − Without NOAC)",
#        title = "Cost vs QALY differences (beehive)") +
#   #scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
#   #scale_y_continuous(limits = c(-2000, 4000), expand = c(0, 0)) +
#   theme_minimal()
# 
# print(p)
# 
# # Load specific font from Google Fonts
# #font_add_google("Rosario", family = "rosario")
# #showtext_auto()
# 
# # plot beehive with reference styling (p2)
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 1.2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   theme_classic(base_family = "rosario") +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.text.x = element_text(margin = margin(t = 8, unit = "pt")),
#     axis.text.y = element_text(margin = margin(r = 8, unit = "pt")),
#     axis.title = element_text(size = 17),
#     plot.margin = margin(20, 20, 40, 20),
#     axis.title.x = element_text(face = "bold"), 
#     axis.title.y = element_text(face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
#     panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
#   ) +
#   theme(
#     axis.title.x = element_text(
#       margin = margin(t = 16, unit = "pt")),
#     axis.title.y = element_text(
#       margin = margin(r = 16, unit = "pt"))
#   ) +
#   labs(
#     x = "Incremental QALYs per patient",
#     y = "Incremental cost (€) per patient",
#     title = "Incremental cost-effectiveness plane"
#   ) +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.8, 0.8)) +
#   scale_y_continuous(expand = c(0, 0),
#                      breaks = c(-20000, -10000, 0, 10000, 20000),
#                      labels = c("-20 000", "-10 000", "0", "10 000", "20 000"),
#                      limits = c(-20000, 20000)) +
#   annotate("text", x = 0.42, y = 16000, 
#            label = "WTP 50 000 €/QALY",
#            size = 4.0, color = "black", hjust = 0,
#            family = "rosario") +
#   annotate("segment", x = 0.41, y = 16000, 
#            xend = 0.34, yend = 16000,
#            arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#            color = "black", linewidth = 0.5)
# 
# print(p)
# 
# #print(p2)
# 
# # optional: save file
# #ggsave("beehive_qaly_cost_scatter.png", plot = p, width = 7, height = 5, dpi = 300)
# 
# # EHRA instructions max. 1920 x 1080
# 
# # # # Save as PDF with dpi specified
# ggsave("beehive_qaly_cost_scatter.pdf", dpi = 300,
#        bg = "white", width = 8, height = 6)
# 
# 
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter.pdf", density = 300)
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter.png",
#             format = "png",
#             density = 300)
# 
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter.tiff",
#             format = "tiff",
#             density = 300,
#             compression = "LZW")
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter.pdf", density = 180)
# 
# 
# # Save it as JPEG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter.jpeg",
#             format = "jpeg",
#             quality = 100)

######################
## save environment ##
######################
save.image(file = "workspace_environment.RData")
saveRDS(ceac_df, file ="ceac_main_psa.rds")
saveRDS(ceac_df_equal_4, file ="ceac_cha2ds2vasc_equal_4.rds")
saveRDS(ceac_df_less_4, file ="ceac_cha2ds2vasc_less_4.rds")
saveRDS(ceac_df_more_4, file ="ceac_cha2ds2vasc_more_4.rds")


##############################
## PSA WITHOUT RANDOM DEATH ##
##############################
# CHAD2DS2-VASc < 4 PSA
# probabilistic sensitivity
sim <- 1000
iteration <- 2000
###log-normal parameters
# assuming geometric mean given and its 95% CI
stroke_mean <- log(0.87)
bleed_mean <- log(1.27)
stroke_sd <- (log(1.52)-stroke_mean)/1.96
bleed_sd <- (log(2.03)-bleed_mean)/1.96
death_mean <- 0
death_sd <- 0

#create data
source(psa_create_data_path)

####  beehive plot ####
sink("CHAD2DS2-VASc_less_than_4_PSA_without_death.txt")
print("CHAD2DS2-VASc < 4 PSA")
print(sprintf("The NOAC was beneficial in the %.2f%% of the cases", 100*(sum(test[,3]>0) / length(test[,3]))))

beehive.df <- test[,c(3,4)]
colnames(beehive.df) <- c("QALY_diff", "Cost_diff")

beehive.means <- colMeans(beehive.df, na.rm = TRUE)
qaly_diff_mean <- beehive.means["QALY_diff"]
cost_diff_mean <- beehive.means["Cost_diff"]
cost_effectiveness_ratio <- cost_diff_mean / qaly_diff_mean
print(sprintf("Mean QALY difference in prob sens analysis: %.3f", qaly_diff_mean))
print(sprintf("Mean Cost difference in prob sens analysis: %.2f", cost_diff_mean))
print(sprintf("Cost-effectiveness ratio in prob sens analysis: %.2f", cost_effectiveness_ratio))

willingness_to_pay_threshold <- 50000
p_ce_acceptance <- sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * willingness_to_pay_threshold, na.rm = TRUE) / dim(beehive.df)[1]
print(sprintf("Probability of cost-effectiveness at WTP %.2f is: %.2f", willingness_to_pay_threshold, p_ce_acceptance))
sink()
## Cost-Effectiveness Acceptability Curve ##

wtp_vector <- seq(0, 200000, by = 10000)
p_ce_acceptance_vector <- sapply(wtp_vector, function(wtp) {
  sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * wtp, na.rm = TRUE) / dim(beehive.df)[1]
})
# CEAC plot
ceac_df_less_4_wo_death <- data.frame(
  wtp = wtp_vector,
  p_ce_acceptance = p_ce_acceptance_vector
)

ceac_less_4_wo_death <- ggplot(ceac_df_less_4_wo_death, aes(x = wtp, y = p_ce_acceptance)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Willingness-to-pay threshold",
    y = "Probability cost-effective (acceptance)",
    title = "Cost-Effectiveness Acceptability Curve (CEAC)"
  ) +
  theme_minimal()

saveRDS(ceac_df_less_4_wo_death, file = "ceac_cha2ds2vasc_less_4_without_death.rds")
#####################################
##    Cost-Effectiveness Plane     ##
#####################################
saveRDS(beehive.df, file = "beehive.df_CHAD2DS2-VASc_less_than_4_without_death.rds")
# # plot beehive with fixed axis limits
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   labs(x = "QALY difference (With NOAC − Without NOAC)",
#        y = "Cost difference (With NOAC − Without NOAC)",
#        title = "Cost vs QALY differences (beehive)") +
#   #scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
#   #scale_y_continuous(limits = c(-2000, 4000), expand = c(0, 0)) +
#   theme_minimal()
# 
# print(p)
# 
# # Load specific font from Google Fonts
# #font_add_google("Rosario", family = "rosario")
# #showtext_auto()
# 
# # plot beehive with reference styling (p2)
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 1.2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   theme_classic(base_family = "rosario") +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.text.x = element_text(margin = margin(t = 8, unit = "pt")),
#     axis.text.y = element_text(margin = margin(r = 8, unit = "pt")),
#     axis.title = element_text(size = 17),
#     plot.margin = margin(20, 20, 40, 20),
#     axis.title.x = element_text(face = "bold"), 
#     axis.title.y = element_text(face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
#     panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
#   ) +
#   theme(
#     axis.title.x = element_text(
#       margin = margin(t = 16, unit = "pt")),
#     axis.title.y = element_text(
#       margin = margin(r = 16, unit = "pt"))
#   ) +
#   labs(
#     x = "Incremental QALYs per patient",
#     y = "Incremental cost (€) per patient",
#     title = "Incremental cost-effectiveness plane"
#   ) +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.8, 0.8)) +
#   scale_y_continuous(expand = c(0, 0),
#                      breaks = c(-20000, -10000, 0, 10000, 20000),
#                      labels = c("-20 000", "-10 000", "0", "10 000", "20 000"),
#                      limits = c(-20000, 20000)) +
#   annotate("text", x = 0.42, y = 16000, 
#            label = "WTP 50 000 €/QALY",
#            size = 4.0, color = "black", hjust = 0,
#            family = "rosario") +
#   annotate("segment", x = 0.41, y = 16000, 
#            xend = 0.34, yend = 16000,
#            arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#            color = "black", linewidth = 0.5)
# 
# print(p)
# 
# 
# # # # Save as PDF with dpi specified
# ggsave("beehive_qaly_cost_scatter_CHA2DS2VASc_below_4_without_death.pdf", dpi = 300,
#        bg = "white", width = 8, height = 6)
# 
# 
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_below_4_without_death.pdf", density = 300)
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_below_4_without_death.png",
#             format = "png",
#             density = 300)
# 
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_below_4_without_death.tiff",
#             format = "tiff",
#             density = 300,
#             compression = "LZW")
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_below_4_without_death.pdf", density = 180)
# 
# 
# # Save it as JPEG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_below_4_without_death.jpeg",
#             format = "jpeg",
#             quality = 100)
# 

################################################################
# CHAD2DS2-VASc = 4 PSA
# probabilistic sensitivity
sim <- 1000
iteration <- 2000
###log-normal parameters
# assuming geometric mean given and its 95% CI
stroke_mean <- log(0.63)
bleed_mean <- log(1.31)
stroke_sd <- (log(1.27)-stroke_mean)/1.96
bleed_sd <- (log(2.29)-bleed_mean)/1.96
death_mean <- 0
death_sd <- 0

#create data
source(psa_create_data_path)

####  beehive plot ####
sink("CHAD2DS2-VASc_equal_4_PSA_without_death.txt")
print("CHAD2DS2-VASc = 4 PSA")
print(sprintf("The NOAC was beneficial in the %.2f%% of the cases", 100*(sum(test[,3]>0) / length(test[,3]))))

beehive.df <- test[,c(3,4)]
colnames(beehive.df) <- c("QALY_diff", "Cost_diff")

beehive.means <- colMeans(beehive.df, na.rm = TRUE)
qaly_diff_mean <- beehive.means["QALY_diff"]
cost_diff_mean <- beehive.means["Cost_diff"]
cost_effectiveness_ratio <- cost_diff_mean / qaly_diff_mean
print(sprintf("Mean QALY difference in prob sens analysis: %.3f", qaly_diff_mean))
print(sprintf("Mean Cost difference in prob sens analysis: %.2f", cost_diff_mean))
print(sprintf("Cost-effectiveness ratio in prob sens analysis: %.2f", cost_effectiveness_ratio))

willingness_to_pay_threshold <- 50000
p_ce_acceptance <- sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * willingness_to_pay_threshold, na.rm = TRUE) / dim(beehive.df)[1]
print(sprintf("Probability of cost-effectiveness at WTP %.2f is: %.2f", willingness_to_pay_threshold, p_ce_acceptance))
sink()

## Cost-Effectiveness Acceptability Curve ##

wtp_vector <- seq(0, 200000, by = 10000)
p_ce_acceptance_vector <- sapply(wtp_vector, function(wtp) {
  sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * wtp, na.rm = TRUE) / dim(beehive.df)[1]
})
# CEAC plot
ceac_df_equal_4_wo_death <- data.frame(
  wtp = wtp_vector,
  p_ce_acceptance = p_ce_acceptance_vector
)

ceac_equal_4_wo_death <- ggplot(ceac_df_equal_4_wo_death, aes(x = wtp, y = p_ce_acceptance)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Willingness-to-pay threshold",
    y = "Probability cost-effective (acceptance)",
    title = "Cost-Effectiveness Acceptability Curve (CEAC)"
  ) +
  theme_minimal()

saveRDS(ceac_df_equal_4_wo_death, file = "ceac_cha2ds2vasc_equal_4_without_death.rds")
#####################################
##    Cost-Effectiveness Plane     ##
#####################################
saveRDS(beehive.df, file = "beehive.df_CHAD2DS2-VASc_equal_4_without_death.rds")
# # plot beehive with fixed axis limits
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   labs(x = "QALY difference (With NOAC − Without NOAC)",
#        y = "Cost difference (With NOAC − Without NOAC)",
#        title = "Cost vs QALY differences (beehive)") +
#   #scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
#   #scale_y_continuous(limits = c(-2000, 4000), expand = c(0, 0)) +
#   theme_minimal()
# 
# print(p)
# 
# # Load specific font from Google Fonts
# #font_add_google("Rosario", family = "rosario")
# #showtext_auto()
# 
# # plot beehive with reference styling (p2)
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 1.2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   theme_classic(base_family = "rosario") +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.text.x = element_text(margin = margin(t = 8, unit = "pt")),
#     axis.text.y = element_text(margin = margin(r = 8, unit = "pt")),
#     axis.title = element_text(size = 17),
#     plot.margin = margin(20, 20, 40, 20),
#     axis.title.x = element_text(face = "bold"), 
#     axis.title.y = element_text(face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
#     panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
#   ) +
#   theme(
#     axis.title.x = element_text(
#       margin = margin(t = 16, unit = "pt")),
#     axis.title.y = element_text(
#       margin = margin(r = 16, unit = "pt"))
#   ) +
#   labs(
#     x = "Incremental QALYs per patient",
#     y = "Incremental cost (€) per patient",
#     title = "Incremental cost-effectiveness plane"
#   ) +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.8, 0.8)) +
#   scale_y_continuous(expand = c(0, 0),
#                      breaks = c(-20000, -10000, 0, 10000, 20000),
#                      labels = c("-20 000", "-10 000", "0", "10 000", "20 000"),
#                      limits = c(-20000, 20000)) +
#   annotate("text", x = 0.42, y = 16000, 
#            label = "WTP 50 000 €/QALY",
#            size = 4.0, color = "black", hjust = 0,
#            family = "rosario") +
#   annotate("segment", x = 0.41, y = 16000, 
#            xend = 0.34, yend = 16000,
#            arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#            color = "black", linewidth = 0.5)
# 
# print(p)
# 
# 
# # # # Save as PDF with dpi specified
# ggsave("beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4_without_death.pdf", dpi = 300,
#        bg = "white", width = 8, height = 6)
# 
# 
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4_without_death.pdf", density = 300)
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4_without_death.png",
#             format = "png",
#             density = 300)
# 
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4_without_death.tiff",
#             format = "tiff",
#             density = 300,
#             compression = "LZW")
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4_without_death.pdf", density = 180)
# 
# 
# # Save it as JPEG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_equal_4_without_death.jpeg",
#             format = "jpeg",
#             quality = 100)

##################################################################
# CHAD2DS2-VASc > 4 PSA
# probabilistic sensitivity
sim <- 1000
iteration <- 2000
###log-normal parameters
# assuming geometric mean given and its 95% CI
stroke_mean <- log(0.44)
bleed_mean <- log(1.48)
stroke_sd <- (log(0.77)-stroke_mean)/1.96
bleed_sd <- (log(2.45)-bleed_mean)/1.96
death_mean <- 0
death_sd <- 0

#create data
source(psa_create_data_path)

####  beehive plot ####
sink("CHAD2DS2-VASc_more_than_4_PSA_without_death.txt")
print("CHAD2DS2-VASc > 4 PSA")
print(sprintf("The NOAC was beneficial in the %.2f%% of the cases", 100*(sum(test[,3]>0) / length(test[,3]))))

beehive.df <- test[,c(3,4)]
colnames(beehive.df) <- c("QALY_diff", "Cost_diff")

beehive.means <- colMeans(beehive.df, na.rm = TRUE)
qaly_diff_mean <- beehive.means["QALY_diff"]
cost_diff_mean <- beehive.means["Cost_diff"]
cost_effectiveness_ratio <- cost_diff_mean / qaly_diff_mean
print(sprintf("Mean QALY difference in prob sens analysis: %.3f", qaly_diff_mean))
print(sprintf("Mean Cost difference in prob sens analysis: %.2f", cost_diff_mean))
print(sprintf("Cost-effectiveness ratio in prob sens analysis: %.2f", cost_effectiveness_ratio))

willingness_to_pay_threshold <- 50000
p_ce_acceptance <- sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * willingness_to_pay_threshold, na.rm = TRUE) / dim(beehive.df)[1]
print(sprintf("Probability of cost-effectiveness at WTP %.2f is: %.2f", willingness_to_pay_threshold, p_ce_acceptance))
sink()

## Cost-Effectiveness Acceptability Curve ##

wtp_vector <- seq(0, 200000, by = 10000)
p_ce_acceptance_vector <- sapply(wtp_vector, function(wtp) {
  sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * wtp, na.rm = TRUE) / dim(beehive.df)[1]
})
# CEAC plot
ceac_df_more_4_wo_death <- data.frame(
  wtp = wtp_vector,
  p_ce_acceptance = p_ce_acceptance_vector
)

ceac_more_4_wo_death <- ggplot(ceac_df_more_4_wo_death, aes(x = wtp, y = p_ce_acceptance)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Willingness-to-pay threshold",
    y = "Probability cost-effective (acceptance)",
    title = "Cost-Effectiveness Acceptability Curve (CEAC)"
  ) +
  theme_minimal()

saveRDS(ceac_df_more_4_wo_death, file = "ceac_cha2ds2vasc_more_4_without_death.rds")
#####################################
##    Cost-Effectiveness Plane     ##
#####################################
saveRDS(beehive.df, file = "beehive.df_CHAD2DS2-VASc_more_than_4_without_death.rds")
# # plot beehive with fixed axis limits
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   labs(x = "QALY difference (With NOAC − Without NOAC)",
#        y = "Cost difference (With NOAC − Without NOAC)",
#        title = "Cost vs QALY differences (beehive)") +
#   #scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
#   #scale_y_continuous(limits = c(-2000, 4000), expand = c(0, 0)) +
#   theme_minimal()
# 
# print(p)
# 
# # Load specific font from Google Fonts
# #font_add_google("Rosario", family = "rosario")
# #showtext_auto()
# 
# # plot beehive with reference styling (p2)
# p <- ggplot(beehive.df, aes(x = QALY_diff, y = Cost_diff)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
#   geom_point(alpha = 0.65, size = 1.2, color = "#2C7BB6") +
#   geom_abline(intercept = 0, slope = willingness_to_pay_threshold, color = "darkred", size = 0.8) +
#   theme_classic(base_family = "rosario") +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.text.x = element_text(margin = margin(t = 8, unit = "pt")),
#     axis.text.y = element_text(margin = margin(r = 8, unit = "pt")),
#     axis.title = element_text(size = 17),
#     plot.margin = margin(20, 20, 40, 20),
#     axis.title.x = element_text(face = "bold"), 
#     axis.title.y = element_text(face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
#     panel.grid.minor = element_line(color = "grey95", linewidth = 0.25)
#   ) +
#   theme(
#     axis.title.x = element_text(
#       margin = margin(t = 16, unit = "pt")),
#     axis.title.y = element_text(
#       margin = margin(r = 16, unit = "pt"))
#   ) +
#   labs(
#     x = "Incremental QALYs per patient",
#     y = "Incremental cost (€) per patient",
#     title = "Incremental cost-effectiveness plane"
#   ) +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.8, 0.8)) +
#   scale_y_continuous(expand = c(0, 0),
#                      breaks = c(-20000, -10000, 0, 10000, 20000),
#                      labels = c("-20 000", "-10 000", "0", "10 000", "20 000"),
#                      limits = c(-20000, 20000)) +
#   annotate("text", x = 0.42, y = 16000, 
#            label = "WTP 50 000 €/QALY",
#            size = 4.0, color = "black", hjust = 0,
#            family = "rosario") +
#   annotate("segment", x = 0.41, y = 16000, 
#            xend = 0.34, yend = 16000,
#            arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
#            color = "black", linewidth = 0.5)
# 
# print(p)
# 
# 
# # # # Save as PDF with dpi specified
# ggsave("beehive_qaly_cost_scatter_CHA2DS2VASc_above_4_without_death.pdf", dpi = 300,
#        bg = "white", width = 8, height = 6)
# 
# 
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_above_4_without_death.pdf", density = 300)
# 
# # Save it as PNG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_above_4_without_death.png",
#             format = "png",
#             density = 300)
# 
# 
# # Save it as TIFF
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_above_4_without_death.tiff",
#             format = "tiff",
#             density = 300,
#             compression = "LZW")
# 
# # Load that pdf file with the magick package
# pdf_image <- magick::image_read_pdf("beehive_qaly_cost_scatter_CHA2DS2VASc_above_4_without_death.pdf", density = 180)
# 

# # Save it as JPEG
# image_write(pdf_image,
#             path = "beehive_qaly_cost_scatter_CHA2DS2VASc_above_4_without_death.jpeg",
#             format = "jpeg",
#             quality = 100)
#################################################################
##    FIX AXES FROM FIGURES ##
##############################
files <- list.files(
  pattern = "^beehive\\.df.*rds$",
  full.names = TRUE
)

# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")
showtext_auto()


for (i in 1:length(files)){
  file.name <- sub("^\\./", "", files[i])
  beehive.df <- readRDS(file.name)
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
                       limits = c(-0.6, 0.6)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = c(-10000, -5000, 0, 5000, 10000),
                       labels = c("-10 000", "-5000", "0", "5000", "10 000"),
                       limits = c(-10000, 10000)) +
    annotate("text", x = 0.22, y = 6000, 
             label = "WTP 50 000 €/QALY",
             size = 4.0, color = "black", hjust = 0,
             family = "rosario") +
    annotate("segment", x = 0.21, y = 6000, 
             xend = 0.14, yend = 6000,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             color = "black", linewidth = 0.5)
  
  print(p)
  
  
  # # # Save as PDF with dpi specified
  ggsave(paste0(sub(".rds$", "", file.name),".pdf"), dpi = 300,
         bg = "white", width = 8, height = 6)
  
  
  
  # Load that pdf file with the magick package
  pdf_image <- magick::image_read_pdf(paste0(sub(".rds$", "", file.name),".pdf"), density = 300)
  
  # Save it as PNG
  image_write(pdf_image,
              path = paste0(sub(".rds$", "", file.name),".png"),
              format = "png",
              density = 300)
  
  
  # Save it as TIFF
  image_write(pdf_image,
              path = paste0(sub(".rds$", "", file.name),".tiff"),
              format = "tiff",
              density = 300,
              compression = "LZW")
  
  # Load that pdf file with the magick package
  pdf_image <- magick::image_read_pdf(paste0(sub(".rds$", "", file.name),".pdf"), density = 180)
  
  
  # Save it as JPEG
  image_write(pdf_image,
              path = paste0(sub(".rds$", "", file.name),".jpeg"),
              format = "jpeg",
              quality = 100)
  
}


##########################
##  CEAC FIGURES NICELY ##
##########################
# files_ceac <- list.files(
#    pattern = "^ceac.*rds$",
#    full.names = TRUE
# )

files <- list.files(
  pattern = "^beehive\\.df.*rds$",
  full.names = TRUE
)

# Load specific font from Google Fonts
#font_add_google("Rosario", family = "rosario")
#showtext_auto()


for (i in 1:length(files)){
  file.name <- sub("^\\./", "", files[i])
  beehive.df <- readRDS(file.name)
  
  wtp_vector <- seq(0, 300000, by = 10000)
  p_ce_acceptance_vector <- sapply(wtp_vector, function(wtp) {
    sum(beehive.df["Cost_diff"] < beehive.df["QALY_diff"] * wtp, na.rm = TRUE) / dim(beehive.df)[1]
  })
  # CEAC plot
  ceac.df <- data.frame(
    wtp = wtp_vector,
    p_ce_acceptance = p_ce_acceptance_vector
  )

p <- ggplot(ceac.df, aes(x = wtp, y = p_ce_acceptance)) +
  geom_line(linewidth = 1, color = "darkred") +
  #geom_point(data = subset(ceac.df, wtp == 50000), color = "black", size = 3) +
  #geom_point(data = subset(ceac.df, wtp == 150000), color = "black", size = 3) +
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
    x = "Willingness-to-pay threshold",
    y = "Probability of cost-effectiveness"
    #title = "Cost-Effectiveness Acceptability Curve (CEAC)"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = scales::comma)
  # annotate("text", x = 50000, y = 0.05, 
  #          label = "Highly cost-effective",
  #          size = 4.0, color = "black", hjust = 0,
  #          family = "rosario") +
  # annotate("text", x = 150000, y = 0.05, 
  #          label = "Cost-effective",
  #          size = 4.0, color = "black", hjust = 0,
  #          family = "rosario")
  # 
print(p)

file.name <- sub("beehive.df", "ceac", file.name)
# # # Save as PDF with dpi specified
ggsave(paste0(sub(".rds$", "", file.name),".pdf"), dpi = 300,
       bg = "white", width = 8, height = 6)



# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf(paste0(sub(".rds$", "", file.name),".pdf"), density = 300)

# Save it as PNG
image_write(pdf_image,
            path = paste0(sub(".rds$", "", file.name),".png"),
            format = "png",
            density = 300)


# Save it as TIFF
image_write(pdf_image,
            path = paste0(sub(".rds$", "", file.name),".tiff"),
            format = "tiff",
            density = 300,
            compression = "LZW")

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf(paste0(sub(".rds$", "", file.name),".pdf"), density = 180)


# Save it as JPEG
image_write(pdf_image,
            path = paste0(sub(".rds$", "", file.name),".jpeg"),
            format = "jpeg",
            quality = 100)

}





