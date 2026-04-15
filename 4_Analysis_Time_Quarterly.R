# jonashaslbeck@protonmail.com; April, 2026

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Analyses with Time Focus

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

library(tidyr)
library(stringr)
library(jsonlite)
library(readr)

library(lubridate)


library(dplyr)
library(lubridate)

library(ggplot2)

library(plyr)
library(RColorBrewer)


source("0_Helpers.R")
source("5_Plotting_Meta.R") # For labels, colors, item selection, etc


# --------------------------------------------
# --------- Version of AI Run ----------------
# --------------------------------------------

ai_handle <- "7.0.2-Mistral-Large-"

# Total Years
Nyear <- 16 # 16 with 2025
# Total number of months
tot_qu <- Nyear*4

# --------------------------------------------
# --------- Load Data ------------------------
# --------------------------------------------

# ----- AI Data -----
## Select filter version
filter <- "mainly"
# filter <- "broadly"
# filter <- "noBM"
data_bin_ss <- readRDS(file = paste0("Files/G_Df_bin_Filtered_", filter, "_", ai_handle, ".RDS"))
# Load only with data quality filters
data_bin_ss_noFil <- readRDS(file = paste0("Files/G_Df_bin_Filtered_", "noBM", "_", ai_handle, ".RDS"))

# ----- Climate Total Data ------
# These are all the articles with the keyword "climate"
# A stratified (year) random subset of those we analyzed with the AI
df_climate_total <- readRDS(file = paste0("Files/Guard_Total_noAI.RDS"))
df_climate_total$publication_date_day <- substr(df_climate_total$publication_date, 1, 10)
tb_climate_total <- table(df_climate_total$publication_date_day)

# ----- Total Data [Monthly] ------
# Monthly
df_total_monthly <- read_csv("Files/guardian_monthly_counts.csv")
dim(df_total_monthly)
# Additional monthly counts *before* Simon's filtering, to be able to show the impact of fitering
df_total_month_prefilter <- read_csv("Files/guardian_monthly_counts_unfiltered.csv")
dim(df_total_month_prefilter)


# --------------------------------------------
# --------- Total Counts Analysis ------------
# --------------------------------------------

# ----- Combine: Climate Total with Absolute Total [Quarterly Level] -----
df_counts_qu <- data.frame(matrix(NA, nrow=tot_qu, ncol=3))
colnames(df_counts_qu) <- c("date", "n_total", "n_climate")
df_counts_qu$date <- paste0(
  rep(2010:2025, each = 4),
  "-Q",
  rep(1:4, times = Nyear)
)

for(q in 1:tot_qu) {
  # Construct target char
  year_q <- as.numeric(substr(df_counts_qu$date[q], 1, 4))
  quarter_q <- substr(df_counts_qu$date[q], 7, 7)
  if(quarter_q == "1") {
    months_q <- c("01", "02", "03")
  } else if(quarter_q == "2") {
    months_q <- c("04", "05", "06")
  } else if(quarter_q == "3") {
    months_q <- c("07", "08", "09")
  } else if(quarter_q == "4") {
    months_q <- c("10", "11", "12")
  }
  
  ## Total Counts from df_total_monthly
  year_q_df <- substr(df_total_monthly$date, 1, 7)
  df_q <- df_total_monthly[year_q_df %in% paste0(year_q, "-", months_q), ]
  df_counts_qu$n_total[q] <- sum(df_q$article_count)
  
  ## Climate Counts from df_climate_total
  year_q_df2 <- substr(df_climate_total$publication_date_day, 1, 7)
  df_q2 <- df_climate_total[year_q_df2 %in% paste0(year_q, "-", months_q), ]
  df_counts_qu$n_climate[q] <- nrow(df_q2)
} # end for: quarters


# ----------------------------------------------------
# ------- Quarterly Raw Totals With/Without Filter ---
# ----------------------------------------------------

# ----- Compute Quarterly unfiltered data from monthly *filtered* data ----
date_raw <-  as.POSIXct(df_total_monthly$date , tz = "UTC")
date_raw_quarter <- paste0(format(date_raw, "%Y"), "-", quarters(date_raw))
u_qu <- unique(date_raw_quarter)
# Storage
counts_quarters_filtered <- rep(NA, tot_qu)
# Loop
for(q in 1:tot_qu) counts_quarters_filtered[q] <- sum(df_total_monthly$article_count[date_raw_quarter == u_qu[q]])

# ----- Compute Quarterly unfiltered data from monthly unfiltered data ----
date_raw <-  as.POSIXct(df_total_month_prefilter$date , tz = "UTC")
date_raw_quarter <- paste0(format(date_raw, "%Y"), "-", quarters(date_raw))
u_qu <- unique(date_raw_quarter)
# Storage
counts_quarters_UNfiltered <- rep(NA, tot_qu)
# Loop
for(q in 1:tot_qu) counts_quarters_UNfiltered[q] <- sum(df_total_month_prefilter$article_count[date_raw_quarter == u_qu[q]])


# ---- Plotting -----
pdf("Figures/Fig_Total_Articles_per_quarter.pdf", width=7, height = 5)
par(mar=c(4,6,2,1))
plot.new()
plot.window(xlim=c(1, 64), ylim=c(15000, 35000))
grid()
axis(1, at=seq(1, 64, by=4), labels=rep(2010:2025), las=2)
axis(2, las=2)
lines(counts_quarters_filtered, type="l", lwd=2)
lines(counts_quarters_UNfiltered, type="l", lwd=2, lty=2)
title(ylab="Total number of Articles", line=4)
title(main="Total number of Articles / Quarter", font.main=1, cex=1.5)
legend("topright", c("Without Filter", "With Filter"), lty=c(2,1), cex=1.5, bty="n")
dev.off()


# --------------------------------------------
# --------- Get Point Estimates --------------
# --------------------------------------------
# Functions are in Helpers.R
m_qu_filter <- ComputeFilter(data_bin_ss_noFil)$m_qu_filter # % articles with keyword "climate"
m_qu_filters_norm <- ComputeFilter(data_bin_ss_noFil)$m_qu_filters_norm # % articles total
a_qu_OR_norm <- ComputeOR(data_bin_ss)
m_qu_items_norm <- ComputeItems(data_bin_ss)

# --------------------------------------------
# --------- Numbers Reported in Paper --------
# --------------------------------------------
df_filters <- cbind(df_counts_qu$date, m_qu_filters_norm)
# Broadly before 2018
round(mean(df_filters$broadly[1:32]), 3)
# Broadly from 2018
round(mean(df_filters$broadly[33:64]), 3)
# Mainly before 2018
round(mean(df_filters$mainly[1:32]), 3)
# Mainly from 2018
round(mean(df_filters$mainly[33:64]), 3)


# --------------------------------------------
# --------- Bootstrap CIs --------------------
# --------------------------------------------

# ----- Bootstrap -----
Nb <- 500 # Bootstrap samples
# Filters
m_qu_filters_norm_Boot <- array(NA, dim=c(tot_qu, 2, Nb))
m_qu_OR_norm_Boot <- array(NA, dim=c(tot_qu, 4, Nb))
N_data <- nrow(data_bin_ss_noFil)
for(b in 1:Nb) {
  set.seed(b)
  subset <- data_bin_ss_noFil[sample(1:N_data, size=N_data, replace=TRUE), ]
  m_qu_filters_norm_Boot[, , b] <- as.matrix(ComputeFilter(subset)$m_qu_filters_norm)
  m_qu_OR_norm_Boot[, , b] <- as.matrix(ComputeOR(subset))
  print(b)
}

# ----- Compute CIs -----
# Filters
m_qu_filters_norm_CIs <- apply(m_qu_filters_norm_Boot, 1:2, function(x) {
  quantile(x, probs=c(0.025, 0.975))
})
dim(m_qu_filters_norm_CIs)
saveRDS(m_qu_filters_norm_CIs, "Files/Bootstrap_CIs_Filters.RDS")
# OR-questions
m_qu_OR_norm_CIs <- apply(m_qu_OR_norm_Boot, 1:2, function(x) {
  quantile(x, probs=c(0.025, 0.975))
})
dim(m_qu_OR_norm_CIs)
saveRDS(m_qu_OR_norm_CIs, "Files/Bootstrap_CIs_ORs.RDS")



# --------------------------------------------
# --------- Make Figure ----------------------
# --------------------------------------------

# ------ Prepare Data -------
var_names <- c("Broadly", "Mainly", "Causes", "Impacts", "Mitigation", "Adaptation")
Y <- cbind(m_qu_filters_norm, a_qu_OR_norm)
colnames(Y)  <- var_names

# data.frame with point estimates
df <- as.data.frame(Y) |>
  mutate(time = seq_len(nrow(Y))) |>
  pivot_longer(
    cols = all_of(var_names),
    names_to = "variable",
    values_to = "estimate"
  )
# df$se <- NA
df$estimate <- df$estimate * 100
df$lower <- df$upper <- NA
head(df)

# Add CIs
cnt <- 1
for(q in 1:tot_qu) {
  for(j in 1:6) {
    if(j %in% 1:2) {
      df$lower[cnt] <- m_qu_filters_norm_CIs[1, q, j] * 100
      df$upper[cnt] <- m_qu_filters_norm_CIs[2, q, j] * 100
    } 
    if(j %in% 3:6) {
      df$lower[cnt] <- m_qu_OR_norm_CIs[1, q, j-2] * 100
      df$upper[cnt] <- m_qu_OR_norm_CIs[2, q, j-2] * 100
    } 
    cnt <- cnt + 1
  }
}
df <- as.data.frame(df)
df$variable <- factor(df$variable, levels = var_names)
head(df)

# ------ Plotting -------
p <- plotFig1(df)
ggsave('Figures/Figure1_new.pdf', p, width = 12, height = 7)


