# jonashaslbeck@protonmail.com; August, 2026

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# We do some basic analysis on the AI responses we got from
# 100 x each newspaper


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

library(plyr)
library(RColorBrewer)

source("0_Helpers.R")
source("3_Plotting_Meta.R") # For labels, colors, item selection, etc


# --------------------------------------------
# --------- Load Data ------------------------
# --------------------------------------------

valdata <- read.csv('Files/validation_results.csv')


# --------------------------------------------
# --------- Figure: Accuracy & Dice ----------
# --------------------------------------------


# dim(valdata)
colnames(valdata)


# Loop over four main paper topics
sc <- 1.2
pdf(paste0("Figures/Fig_Validation_Mistral_F3.pdf"), width=10*sc, height=8*sc)

par(mfrow=c(2,2))

for(i in 1:4) {
  
  # ----- Prep Data -----
  # Fix Fabian's annoying alphabetical order
  vec <- valdata[valdata$question %in% l_labels_P[[i]], 1]
  ord <- order(factor(vec, levels = l_labels_P[[i]]))
  acc_dice <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], c("accuracy_consensus_gpt", "dice_consensus_gpt")]))[, ord]
  exp_dice <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], "edice_consensus_gpt"]))[, ord]
  exp_acc <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], "eaccuracy_consensus_gpt"]))[, ord]
  # CIs
  acc_CI <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], c("accuracy_lo", "accuracy_hi")]))[, ord]
  dice_CI <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], c("dice_lo", "dice_hi")]))[, ord]
  
  # --- Plotting ---
  v_names <- c("Causes", "Impacts", "Mitigation", "Adaptation")
  par(mar=c(4.5,4,4,2))
  barplot(acc_dice, beside=TRUE, las=2, ylim=c(0, 1),  # 0.65
          col=cols_val[1:2], xaxt = "n")
  abline(h=seq(0, 1, length=6), lty=3, col="grey")
  # grid()
  title(ylab="Accuracy / DICE")
  title(main=v_names[i], font.main=1)
  bp <- barplot(acc_dice, beside=TRUE, las=2, ylim=c(0,0.65),
                col=cols_val[1:2], add=TRUE, xaxt = "n")
  # Plot expected dice
  points(bp[2, ], exp_dice, cex=2, pch=19)
  points(bp[2, ], exp_dice, cex=1.5, pch=19, col=cols_val[2])
  # Plot expected accuracy
  points(bp[1, ], exp_acc, cex=2, pch=19)
  points(bp[1, ], exp_acc, cex=1.5, pch=19, col=cols_val[1])
  # Axis labels
  axis(2, las=2)
  axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  text(
    x = colMeans(bp),
    y = -0.09,
    labels = l_indP_lab_lb[[i]],
    xpd = NA,
    srt = 0,           # rotate if needed
    adj = 0.5,
    cex = 0.90
  )
  
  # Add Error bars
  for(j in 1:ncol(acc_dice)) {
    # Accuracy
    xs <- 0.1
    segments(bp[1, j], acc_CI[1, j], bp[1, j], acc_CI[2, j], col="black", lwd=2)
    segments(bp[1, j]-xs, acc_CI[1, j], bp[1, j]+xs, acc_CI[1, j], col="black", lwd=2)
    segments(bp[1, j]-xs, acc_CI[2, j], bp[1, j]+xs, acc_CI[2, j], col="black", lwd=2)
    # Dice
    segments(bp[2, j], dice_CI[1, j], bp[2, j], dice_CI[2, j], col="black", lwd=2)
    segments(bp[2, j]-xs, dice_CI[1, j], bp[2, j]+xs, dice_CI[1, j], col="black", lwd=2)
    segments(bp[2, j]-xs, dice_CI[2, j], bp[2, j]+xs, dice_CI[2, j], col="black", lwd=2)
    
    # segments(bp[2, j], acc_dice[2, j], bp[2, j], exp_dice[j], col=cols[2], lwd=2)
  }
  
  
  if(i==1) legend(9.5, 1.05, legend=c("Accuracy", "Dice Similarity"), text.col=cols_val[1:2], bty="n", cex=1.25)
  # title(main=v_cats[i], font.main=1, cex.main=1.5, line=0.5)
  
} #end for: cats

dev.off()

# ------- Some Numbers for the Paper ------
round(mean(valdata$accuracy_consensus_gpt), 3)
round(mean(valdata$eaccuracy_consensus_gpt), 3)

valdata$question[which.min(valdata$accuracy_consensus_gpt)]
valdata$accuracy_consensus_gpt[which.min(valdata$accuracy_consensus_gpt)]
valdata$question[which.max(valdata$accuracy_consensus_gpt)]
valdata$accuracy_consensus_gpt[which.max(valdata$accuracy_consensus_gpt)]

# --------------------------------------------
# --------- Figure: Baserates ----------------
# --------------------------------------------

# Loop over four main paper topics
sc <- 1.2
pdf(paste0("Figures/Fig_Baserate_Mistral_F3.pdf"), width=10*sc, height=8*sc)

par(mfrow=c(2,2))

for(i in 1:4) {
  
  # ----- Prep Data -----
  # Fix Fabian's annoying alphabetical order
  vec <- valdata[valdata$question %in% l_labels_P[[i]], 1]
  ord <- order(factor(vec, levels = l_labels_P[[i]]))
  baserates <- t(as.matrix(valdata[valdata$question %in% l_labels_P[[i]], c("baserate_human", 
                                                                            "baserate_ai")]))[, ord]
  # --- Plotting ---
  par(mar=c(4,4,1,1))
  barplot(baserates, beside=TRUE, las=2, ylim=c(0, 1),  # 0.65
          col=cols_val[c(4, 3)], xaxt = "n")
  abline(h=seq(0, 1, length=6), lty=3, col="grey")
  # grid()
  title(ylab="Proportion")
  title(main=v_names[i], font.main=1)
  bp <- barplot(baserates, beside=TRUE, las=2, ylim=c(0,0.65),
                col=cols_val[c(4, 3)], add=TRUE, xaxt = "n")
  # Plot in expected dice
  # points(bp[2, ], exp_dice, cex=2, pch=19)
  axis(2, las=2)
  axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  text(
    x = colMeans(bp),
    y = -0.09,
    labels = l_indP_lab_lb[[i]],
    xpd = NA,
    srt = 0,           # rotate if needed
    adj = 0.5,
    cex = 0.90
  )
  
  if(i==1) legend("topright", legend=c("Baserate Human", "Baserate AI"), text.col=cols_val[c(4, 3)], bty="n", cex=1.25)
  # title(main=v_cats[i], font.main=1, cex.main=1.5, line=0.5)
  
} #end for: cats

dev.off()


# --------------------------------------------
# --------- Numeric Results for Paper --------
# --------------------------------------------
# 
# valdata_ss_disp <- valdata[valdata$question %in% unlist(l_labels_P), ]
# 
# # Mean
# mean(valdata_ss_disp$accuracy)
# # Min
# valdata_ss_disp[which.min(valdata_ss_disp$accuracy), ]
# # Max
# valdata_ss_disp[which.max(valdata_ss_disp$accuracy), ]
# 
# 




# 
# # --------------------------------------------
# # --------- AUX DUMP ----------
# # --------------------------------------------
# 
# library(readr)
# 
# data_val <- read_csv("Validation/df_validation_guardian.csv")
# # View(data_val)
# 
# # Plot to text file to copy to validation form
# writeLines(data_val$url_numbered, "Validation/url_numbered.txt")
# 
# writeLines(data_val$url, "Validation/url.txt")
# 
# data_val$content[34]



