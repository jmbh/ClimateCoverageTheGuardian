# jonashaslbeck@protonmail.com; March, 2026

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Main results on percentages

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

# Database
library(DBI)
library(RMySQL)

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(jsonlite)

library(ggplot2)

library(plyr)
library(RColorBrewer)

source("0_Helpers.R") 
source("5_Plotting_Meta.R") # For labels, colors, item selection, etc


# --------------------------------------------
# --------- Version of AI Run ----------------
# --------------------------------------------

ai_handle <- "7.0.2-Mistral-Large-"


# --------------------------------------------
# --------- Load Data ------------------------
# --------------------------------------------

# ----- Select filter version -----
# filter <- "broadly"
filter <- "mainly"
# filter <- "noBM"

# ----- Load Data -----
data_bin_ss <- readRDS(file = paste0("Files/G_Df_bin_Filtered_", filter, "_", ai_handle, ".RDS"))
N <- nrow(data_bin_ss)
N

# --------------------------------------------
# --------- Main 2x2 Figure [Across Years] ---
# --------------------------------------------

# Loop over four main paper topics
sc <- 1.2
pdf(paste0("Figures/Fig_G_barplots_2x2_", filter, ".pdf"), width=10*sc, height=7.2*sc)

par(mfrow=c(2,2))
for(i in 1:4)   Plot1Barplot(data = data_bin_ss, 
                             ind_qu = l_labels_P[[i]], 
                             labels_qu = l_indP_lab_lb[[i]], 
                             cols = cols,
                             outlets = "theguardian.com",
                             legend = FALSE, 
                             title = paste0(v_cats[i]), 
                             ymax=90, 
                             SEs = TRUE)

dev.off()


# ---- Export Probabilities of this Figure for other Analysis ----
# Subset 2010-2024 (exclude 2025) to make comparable
# Compute proportions
v_props_Guardian <- colMeans(data_bin_ss[, unlist(l_labels_P)])
v_props_Guardian <- as.data.frame(v_props_Guardian)
round(v_props_Guardian, 2)
saveRDS(v_props_Guardian, paste0("Files/Guard_prop_2x2_", filter, "_", ai_handle, ".RDS"))

# ----- Numbers for Paper ------
round(v_props_Guardian, 3)
m_Guard_mean_SE <- cbind(v_props_Guardian, sqrt(v_props_Guardian*(1-v_props_Guardian)/N))

# Compute mean OR within category
round(mean(apply(data_bin_ss[, l_labels_P[[1]]], 1, function(x) sum(x) > 0)), 3)
round(mean(apply(data_bin_ss[, l_labels_P[[2]]], 1, function(x) sum(x) > 0)), 3)
round(mean(apply(data_bin_ss[, l_labels_P[[3]]], 1, function(x) sum(x) > 0)), 3)
round(mean(apply(data_bin_ss[, l_labels_P[[4]]], 1, function(x) sum(x) > 0)), 3)

# ---- Export Probabilities of this Figure for Comparison with German Data ----
# Subset 2010-2024 (exclude 2025) to make comparable
data_bin_ss_no25 <- data_bin_ss[data_bin_ss$year != 2025, ]
# Compute proportions
v_props_Guardian_no25 <- colMeans(data_bin_ss_no25[, unlist(l_labels_P)])
v_props_Guardian_no25 <- as.data.frame(v_props_Guardian_no25)
round(v_props_Guardian_no25, 2)
saveRDS(v_props_Guardian_no25, paste0("Files/Guard_prop_2x2_", filter, "_", ai_handle, "_no2025.RDS"))


# ------------------------------------------------
# --------- Plot Main Result with Diff Filters ---
# ------------------------------------------------

# ----- Compute means -----
# noBM
data_bin_ss_noBM <- readRDS(file = paste0("Files/G_Df_bin_Filtered_", "noBM", "_", ai_handle, ".RDS"))
v_noBM <- colMeans(data_bin_ss_noBM[, unlist(l_labels_P)])
# Broadly
data_bin_ss_broadly <- readRDS(file = paste0("Files/G_Df_bin_Filtered_", "broadly", "_", ai_handle, ".RDS"))
v_broadly <- colMeans(data_bin_ss_broadly[, unlist(l_labels_P)])
# Mainly
data_bin_ss_mainly <- readRDS(file = paste0("Files/G_Df_bin_Filtered_", "mainly", "_", ai_handle, ".RDS"))
v_mainly <- colMeans(data_bin_ss_no25[, unlist(l_labels_P)])
# Combine
v_cmb_filters <- as.data.frame(rbind(v_mainly, v_broadly, v_noBM))


pdf(paste0("Figures/Fig_G_barplots_2x2_FiltersCombined.pdf"), width=10*sc, height=7.2*sc)

par(mfrow=c(2,2))
for(i in 1:4) Plot1Barplot_Tab(tab_i = as.matrix(v_cmb_filters[, l_labels_P[[i]]]), 
                               ind_qu = l_labels_P[[i]], 
                               labels_qu = l_indP_lab_lb[[i]], 
                               cols = c("#1F3A6D", "#6B7280", "#8B5E3C"),
                               outlets = c("Mainly", "Broadly", "No Filter"),
                               legend = c(1,0,0,0)[i], 
                               title = paste0(v_cats[i]), 
                               ymax=90, 
                               SEs = FALSE)

dev.off()



# ------------------------------------------------
# --------- Plot Together with Germany Results ---
# ------------------------------------------------

# Read Germany Results from other paper
# These data come from this earlier paper: https://osf.io/preprints/socarxiv/mv2q6_v1
m_props_Germany <- readRDS("Files/Datasum_prop_2x2_mainly_7.0.2-Mistral-Large-.RDS")
dim(m_props_Germany)

# Add Guardian Results:
m_props_combined <- rbind(m_props_Germany, v_props_Guardian_no25[, 1])
v_ran <- list(1:7, 8:14, 15:21, 22:28)

pdf(paste0("Figures/Fig_G_barplots_2x2_", filter, "_withGerman.pdf"), width=10*sc, height=7.2*sc)

par(mfrow=c(2,2))
for(i in 1:4)   Plot1Barplot_Tab(tab_i = as.matrix(m_props_combined[, v_ran[[i]]]), 
                                 ind_qu = l_labels_P[[i]], 
                                 labels_qu = l_indP_lab_lb[[i]], 
                                 cols = cols_wG,
                                 outlets = outlets_wG,
                                 legend = c(0,1,0,0)[i], 
                                 title = paste0(v_cats[i]), 
                                 ymax=90, 
                                 SEs = FALSE)

dev.off()




