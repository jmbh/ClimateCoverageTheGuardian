# jonashaslbeck@protonmail.com; March 2026

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Here we parse the JSON, do some checks, and binarize the data
# The output is the dataframe that is being used for analysis

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

# Database
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(jsonlite)
library(dplyr)

library(plyr)
library(RColorBrewer)

source("0_Helpers.R")
source("5_Plotting_Meta.R")


# --------------------------------------------
# --------- Version of AI Run ----------------
# --------------------------------------------
ai_handle <- "7.0.2-Mistral-Large-"


# --------------------------------------------
# --------- Load Data ------------------------
# --------------------------------------------

# AI Data
data_cmpl <- readRDS(paste0("Files/Guard_RawData_With_WordCount", ai_handle, ".RDS"))
dim(data_cmpl)


# --------------------------------------------
# --------- Processing JSON ------------------
# --------------------------------------------

N <- nrow(data_cmpl)
l_df <- list()

for(i in 1:N) {
  
  # Parse JSON into a list
  parsed <- fromJSON(data_cmpl$response[i])
  
  # Keep only the first element for each key
  first_only <- lapply(parsed, function(x) {
    if (length(x) > 1) x[[1]] else x
  })
  
  # Convert to a one-row dataframe
  l_df[[i]] <- as.data.frame(first_only, stringsAsFactors = FALSE)
  l_df[[i]]$url <- data_cmpl$url[i]
  l_df[[i]]$content <- data_cmpl$content[i]
  l_df[[i]]$newspaper <- data_cmpl$newspaper[i]
  l_df[[i]]$publication_date <- data_cmpl$publication_date[i]
  l_df[[i]]$year <- data_cmpl$year[i]
  l_df[[i]]$wordcount <- data_cmpl$wordcount[i]
  
  print(paste0(i, "/", N))
  
} # end for


# --------------------------------------------
# --------- Analyze JSON Failures ------------
# --------------------------------------------

# ---- Check on: Number of Columns -----
if(ai_handle == "7.0.2-Mistral-Large-") n_columns <- 91
v_ncol <- unlist(lapply(l_df, ncol))
table(v_ncol) 
1-mean(v_ncol==n_columns)

# Only for a single article the parsing failed
# And this case we exclude
if(sum(v_ncol != n_columns) > 0) {
  l_df_cl <- l_df[-which(v_ncol != n_columns)] 
} else {
  l_df_cl <- l_df
}
# ---- Check on: Column Names -----
N <- length(l_df_cl)
correct_coln <- names(l_df_cl[[1]])
v_check_coln <- rep(N)
for(i in 1:N) v_check_coln[i] <- all(colnames(l_df_cl[[1]]) == colnames(l_df_cl[[i]]))
table(v_check_coln)
# Remove the one with issues
l_df_cl2 <- l_df[-which(v_check_coln==0)] 
length(l_df_cl2)


# --------------------------------------------
# --------- Combine into Dataframe -----------
# --------------------------------------------

data_pr <- do.call(rbind, l_df_cl2)
dim(data_pr)


# --------------------------------------------
# --------- Check Year Variable --------------
# --------------------------------------------

table(is.na(data_pr$year)) # no problem!
table(data_pr$year) 


# --------------------------------------------
# --------- Recode 4-Responses ---------------
# --------------------------------------------

source("5_Plotting_Meta.R")

# ----- Get proportion of the four response options -----
tb <- table(as.character(unlist(data_pr[, unlist(l_labels_P)]))) # 28 core questions
round(prop.table(tb), 5)
tbn <- prop.table(tb)[-5]
names(tbn) <- c("None", "Weakly", "Strongly", "Mentioned")
round(tbn , 4)

# ----- Recode responses before binarization -----
if(ai_handle=="7.0.2-Mistral-Large-") {
  data_pr_cop <- data_pr_cop_i <- data_pr[, -(1:8)]
  data_pr_cop[data_pr_cop_i == "0"] <- 1
  data_pr_cop[data_pr_cop_i == "1"] <- 2
  data_pr_cop[data_pr_cop_i == "2"] <- 3
  data_pr_cop[data_pr_cop_i == "3"] <- 4
  data_pr[, -(1:8)] <- data_pr_cop
}


# --------------------------------------------
# --------- Recode Binary-Responses ----------
# --------------------------------------------

data_pr[data_pr == "no"] <- 0
data_pr[data_pr == "yes"] <- 1


# --------------------------------------------
# --------- Binarize -------------------------
# --------------------------------------------

if(ai_handle == "7.0.2-Mistral-Large-") not <- c(1:8, 83:91) # For Final F3 Mistral Model

data <- data_pr
data[, -not] <- ifelse(data[, -not] > 1, 1, 0)


# --------------------------------------------
# --------- Save -----------------------------
# --------------------------------------------

saveRDS(data, paste0("Files/Guard_Df_binarized_rdy_", ai_handle, ".RDS"))
dim(data)

