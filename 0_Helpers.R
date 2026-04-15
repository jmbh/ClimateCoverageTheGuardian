# jonashaslbeck@protonmail.com; April 2026

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Helpers for the media analysis project


# -----------------------------------------
# ----- Plotting Cor Matrix ---------------
# -----------------------------------------


# Plotting function, works both with symmetric and not
plot_acc <- function(X, digits = 2, min, max, title, 
                     leg_title = "Correlation", 
                     symmetric = TRUE) {
  df <- as.data.frame(as.table(X))
  colnames(df) <- c("Var1", "Var2", "Accuracy")
  
  # keep only lower triangular if symmetric = TRUE
  if (symmetric) {
    df <- df[as.numeric(df$Var1) >= as.numeric(df$Var2), ]
  }
  
  ggplot(df, aes(x = Var1, y = Var2, fill = Accuracy)) +
    geom_tile() +
    geom_text(aes(label = ifelse(is.na(Accuracy), "", 
                                 round(Accuracy, digits))),
              color = "white", size = 2) +
    scale_fill_gradient(
      low = "blue", high = "red",
      limits = c(min, 1),
      na.value = "white"
    ) +
    coord_equal() +
    theme_minimal() +
    labs(
      title = title,
      x = "",
      y = "",
      fill = leg_title
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}


# --------------------------------------------
# --------- Summary Validation Results -------
# --------------------------------------------

Eval <- function(data, new=TRUE, decimals=2) {
  
  # Subset
  if(new) data <- data[data$question %in% unlist(l_indP_lab), ]
  
  out <- c(
    round(mean(data$accuracy), decimals),
    round(mean(data$dice), decimals),
    round(mean(data$dice/data$edice), decimals),
    round(mean(data$baserate), decimals),
    round(mean(data$baserate_ai), decimals),
    round(data$baserate_ai[data$question=="causes_human"], decimals)
  )
  names(out) <- c("accuracy", "dice", "dice/edice", "baserate_hum", "baserate_ai", "baserate_hc")
  
  return(out)
  
} # eof

# --------------------------------------------
# --------- Plot one Panel of Barplot Figure -
# --------------------------------------------

Plot1Barplot <- function(data, 
                         ind_qu, 
                         labels_qu, 
                         cols, 
                         outlets,
                         legend=FALSE, 
                         title=NULL, 
                         ymax=65, 
                         grid=TRUE, 
                         SEs=TRUE) {
  
  # --- Compute proportions ---
  n_col_i <- length(ind_qu)
  tab_i <- matrix(NA, nrow=length(outlets), ncol=n_col_i)
  rownames(tab_i) <- outlets
  colnames(tab_i) <- l_labels_P[[i]]
  n_outlets <- length(outlets)
  for(j in 1:n_outlets) {
    df_wide_j <- data[data$newspaper == outlets[j], ]
    tab_i[j, ] <- colMeans(df_wide_j[, ind_qu]) # / colMeans(df_wide_j_ds[, l_indP_lab[[i]]])
  }
  
  # --- Plotting ---
  par(mar=c(4,4,1,1))
  barplot(tab_i*100, beside=TRUE, las=2, ylim=c(0, ymax),  # 0.65
          col=cols[row(tab_i)], xaxt = "n", axes=F)
  if(grid) {
    abline(h=seq(0, ymax, length=9), lty=3, col="grey")
  }
  
  
  
  bp <- barplot(tab_i*100, beside=TRUE, las=2, ylim=c(0, ymax),
                col=cols[row(tab_i)], add=TRUE, xaxt = "n", axes=F)
  axis(2, las=2, at=seq(0, ymax, by=10), labels=paste0(seq(0, ymax, by=10), "%"))
  
  # browser()
  
  # Add SEs
  if(SEs == TRUE) {
    N <- nrow(data)
    bar <- 0.1
    tab_i_SE <- sqrt(tab_i*(1-tab_i) / N)
    segments(bp, 
             (tab_i-tab_i_SE)*100, 
             bp, 
             (tab_i+tab_i_SE)*100, 
             col="grey")
    segments(bp-bar, 
             (tab_i-tab_i_SE)*100, 
             bp+bar, 
             (tab_i-tab_i_SE)*100, 
             col="grey")
    segments(bp-bar, 
             (tab_i+tab_i_SE)*100, 
             bp+bar, 
             (tab_i+tab_i_SE)*100, 
             col="grey")
  }
  
  
  
  
  # ## Version Double Line
  # par(mgp = c(3, 1.5, 0)) 
  # axis(1, at=colMeans(bp), labels=labels_qu, las=1, cex.axis=0.75)
  # par(mgp = c(3, 1, 0)) 
  
  ## Version Doube line Force
  axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  text(
    x = colMeans(bp),
    y = - 7,
    labels = labels_qu,
    xpd = NA,
    srt = 0,           # rotate if needed
    adj = 0.5,
    cex = 0.90
  )
  
  title(ylab="% Articles Mainly about Climate Change")
  
  # ## Version 45 degree Single Line
  # axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  # text(
  #   x = colMeans(bp), y = par("usr")[3]-2,
  #   labels = labels_qu,
  #   srt = 45, adj = 1, xpd = NA, cex = 0.75
  # )
  
  
  if(legend) legend("topright", legend=outlets, text.col=cols, bty="n", cex=1.25)
  if(is.null(title)) {
    title(main=v_cats[i], font.main=1, cex.main=1.5, line=-1.2) 
  } else {
    title(title, font.main=1, cex.main=1.5, line=-1.5)
  }
  
} # EoF



# --------------------------------------------
# --------- Plot one Panel of Barplot Figure -
# --------------------------------------------
# Alternative version that takes already the final means/data as input

Plot1Barplot_Tab <- function(tab_i, 
                             ind_qu, 
                             labels_qu, 
                             cols, 
                             outlets,
                             legend=FALSE, 
                             title=NULL, 
                             ymax=65, 
                             grid=TRUE, 
                             SEs=TRUE) {
  
  # browser()
  
  # --- Plotting ---
  par(mar=c(4,4,1,1))
  barplot(tab_i*100, beside=TRUE, las=2, ylim=c(0, ymax),  # 0.65
          col=cols[row(tab_i)], xaxt = "n", axes=F)
  if(grid) {
    abline(h=seq(0, ymax, length=9), lty=3, col="grey")
  }
  
  
  
  bp <- barplot(tab_i*100, beside=TRUE, las=2, ylim=c(0, ymax),
                col=cols[row(tab_i)], add=TRUE, xaxt = "n", axes=F)
  axis(2, las=2, at=seq(0, ymax, by=10), labels=paste0(seq(0, ymax, by=10), "%"))
  
  # browser()
  
  # Add SEs
  if(SEs == TRUE) {
    N <- nrow(data)
    bar <- 0.1
    tab_i_SE <- sqrt(tab_i*(1-tab_i) / N)
    segments(bp, 
             (tab_i-tab_i_SE)*100, 
             bp, 
             (tab_i+tab_i_SE)*100, 
             col="grey")
    segments(bp-bar, 
             (tab_i-tab_i_SE)*100, 
             bp+bar, 
             (tab_i-tab_i_SE)*100, 
             col="grey")
    segments(bp-bar, 
             (tab_i+tab_i_SE)*100, 
             bp+bar, 
             (tab_i+tab_i_SE)*100, 
             col="grey")
  }
  
  
  
  
  # ## Version Double Line
  # par(mgp = c(3, 1.5, 0)) 
  # axis(1, at=colMeans(bp), labels=labels_qu, las=1, cex.axis=0.75)
  # par(mgp = c(3, 1, 0)) 
  
  ## Version Doube line Force
  axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  text(
    x = colMeans(bp),
    y = - 7,
    labels = labels_qu,
    xpd = NA,
    srt = 0,           # rotate if needed
    adj = 0.5,
    cex = 0.90
  )
  
  title(ylab="% Articles Mainly about Climate Change")
  
  # ## Version 45 degree Single Line
  # axis(1, at=colMeans(bp), labels=FALSE, las=1, cex.axis=0.75)
  # text(
  #   x = colMeans(bp), y = par("usr")[3]-2,
  #   labels = labels_qu,
  #   srt = 45, adj = 1, xpd = NA, cex = 0.75
  # )
  
  
  if(legend) legend("right", legend=outlets, text.col=cols, bty="n", cex=1.5)
  if(is.null(title)) {
    title(main=v_cats[i], font.main=1, cex.main=1.5, line=-1.2) 
  } else {
    title(title, font.main=1, cex.main=1.5, line=-1.5)
  }
  
} # EoF





# --------------------------------------------
# --------- Parse JSON -----------------------
# --------------------------------------------

fix_json <- function(json_text) {
  # Parse as a list
  parsed <- fromJSON(json_text)
  
  # Detect "bad" structure: explanation keys that are long sentences, values repeating short answer
  keys <- names(parsed)
  new_list <- list()
  skip_next <- FALSE
  
  for (i in seq_along(keys)) {
    if (skip_next) {
      skip_next <- FALSE
      next
    }
    
    key <- keys[i]
    value <- parsed[[i]]
    
    # If this is a "good" style entry: array length >= 2, just keep it
    if (is.vector(value) && length(value) >= 2) {
      new_list[[key]] <- value
      next
    }
    
    # If "bad" style: next key is an explanation and has same value
    if (i < length(keys)) {
      next_key <- keys[i + 1]
      next_value <- parsed[[i + 1]]
      
      # Check if next key looks like an explanation (longer than 20 chars and contains spaces)
      if (nchar(next_key) > 20 && grepl(" ", next_key) &&
          identical(as.character(value), as.character(next_value))) {
        new_list[[key]] <- c(value, next_key)
        skip_next <- TRUE
      } else {
        # Just keep as single element (no explanation found)
        new_list[[key]] <- c(value, NA)
      }
    } else {
      # Last key: keep as is
      new_list[[key]] <- c(value, NA)
    }
  }
  
  return(new_list)
}



# --------------------------------------------
# --------- Compute Jaccard for 2 vectors ----
# --------------------------------------------

jaccard_index <- function(vec1, vec2) {
  # Ensure both vectors are binary (0/1)
  if (!all(vec1 %in% c(0, 1)) | !all(vec2 %in% c(0, 1))) {
    stop("Both vectors must be binary (0 or 1).")
  }
  
  # Compute intersection and union
  intersection <- sum(vec1 & vec2)
  union <- sum(vec1 | vec2)
  
  # Handle case where both vectors are all zeros
  if (union == 0) {
    return(NA)
  }
  
  # Compute Jaccard index
  jaccard <- intersection / union
  return(jaccard)
}


# --------------------------------------------
# --------- Little JSON fix ------------------
# --------------------------------------------

LittleJSONfix <- function(x) {
  
  new_string <- gsub("```", "", x)
  new_string <- gsub("json", "", new_string)
  new_string
}

# --------------------------------------------
# --------- Line Break For Labels ------------
# --------------------------------------------

# Function to split labels into two lines if needed
split_labels <- function(labels) {
  sapply(labels, function(lbl) {
    words <- unlist(strsplit(lbl, " "))
    if (length(words) > 1) {
      paste(words, collapse = "\n")  # Insert a line break
    } else {
      lbl  # Keep as is
    }
  })
}



# --------------------------------------------
# --------- Stuff Adapted from Fabian --------
# --------------------------------------------

map_yesno <- function(answer_vec) {
  x <- tolower(answer_vec)
  x[x == 'no'] <- 0
  x[x == 'yes'] <- 1
  
  # Get the other questions, remove % sign of the percentage climate change question
  x[x != 'yes' & x != 'no'] <- str_sub(x[x != 'yes' & x != 'no'], 0, 1)
  as.numeric(x)
}

get_newspaper <- function(url) {
  newspapers <- c(
    'taz.de', 'sueddeutsche.de', 'spiegel.de', 'zeit.de', 'news.google',
    'faz.net', 'welt.de', 'bild.de', 'tagesschau.de', 'theguardian.com'
  )
  
  newspapers[str_detect(url, newspapers)]
}

# get_wide_df2 <- function(df) {
#   
#   df_wide <- c()
#   question_names <- names(parse_json(df$response[1]))
#   
#   for (i in seq(nrow(df))) {
#     url <- df$url[i]
#     newspaper <- df$newspaper[i]
#     
#     response <- tryCatch(parse_json(df$response[i]),
#                          error = function(e) {
#                            print(e)
#                            NULL
#                          }
#     )
#     
#     if (!is.null(response)) {
#       
#       response <- map_yesno(sapply(response, `[`, 1))
#       
#       
#       df_wide <- rbind(
#         df_wide,
#         c(url, newspaper, response)
#       )
#     }
#   }
#   
#   colnames(df_wide) <- c('url', 'newspaper', question_names)
#   data.frame(df_wide) %>% 
#     mutate(across(-c(url, newspaper), as.numeric))
# } # eoF


get_wide_df <- function(df) {
  
  df_wide <- c()
  question_names <- names(parse_json(df$response[1]))
  nr_questions <- length(question_names)
  
  for (i in seq(nrow(df))) {
    url <- df$url[i]
    newspaper <- get_newspaper(url)
    
    response <- tryCatch(parse_json(df$response[i]),
                         error = function(e) {
                           print(e)
                           NULL
                         }
    )
    
    if (!is.null(response)) {
      
      response <- map_yesno(sapply(response, `[`, 1))
      df_wide <- rbind(df_wide,
                       c(newspaper, response[seq(nr_questions)], url))
    } # end if: isnull
    
    # print(i)
  } # end for
  
  colnames(df_wide) <- c('url', 'newspaper', question_names)
  df_final <- data.frame(df_wide) %>% 
    mutate(across(-c(url, newspaper), as.numeric))
  
  return(df_final)
}




# ------------------------------------------
# -------- Plot Labels ---------------------
# ------------------------------------------

PlotLabel <- function(text, srt=0, cex=1.5,
                      xpos=0.5, ypos=0.5) {
  
  par(mar=rep(0, 4))
  
  plot.new()
  plot.window(xlim=c(0, 1), ylim=c(0,1))
  text(x=xpos, y=ypos, labels=text, srt=srt, cex=cex, adj=0.4)
  
}


# --------------------------------------------
# ------- Compute Quarterly Means of Items ---
# --------------------------------------------

# ----- For Filters: Broadly / Mainly -----
ComputeFilter <- function(subset) {
  # Prepare vector
  df_uf_date <-  as.POSIXct(subset$publication_date, tz = "UTC")
  df_uf_date <- paste0(format(df_uf_date, "%Y"), "-", quarters(df_uf_date))
  
  # Storage
  m_qu_filter <- matrix(NA, tot_qu, 2)
  for(i in 1:tot_qu) {
    data_bin_ss_ssy <- subset[df_uf_date == df_counts_qu$date[i], ]
    for(j in 1:2) {
      m_qu_filter[i, j] <- mean(as.numeric(data_bin_ss_ssy[, c("broadly_climate_change", "mainly_climate_change")[j] ])) 
    }
  }
  
  # Normalize
  m_qu_filters_norm <- as.data.frame(matrix(NA, tot_qu, 2))
  colnames(m_qu_filters_norm) <- c("broadly", "mainly")
  for(i in 1:tot_qu) for(j in 1:2) m_qu_filters_norm[i, j] <- df_counts_qu$n_climate[i]* m_qu_filter[i,j] / df_counts_qu$n_total[i]
  
  # Return
  outlist <- list("m_qu_filters_norm" = m_qu_filters_norm, # normalized: percentage of total articles
                  "m_qu_filter" = m_qu_filter) # percentage of climate articles that are broadly/mainly
  
  return(outlist)
} # eoF

# ------ OR Within topics -----
ComputeOR <- function(data_bin_ss) {
  df_date <-  as.POSIXct(data_bin_ss$publication_date, tz = "UTC")
  df_date <- paste0(format(df_date, "%Y"), "-", quarters(df_date))
  
  a_qu_OR <- array(NA, dim=c(tot_qu, 4)) 
  for(i in 1:tot_qu) {
    data_bin_ss_ssy <- data_bin_ss[df_date == df_counts_qu$date[i], ]
    for(j in 1:4) {
      a_qu_OR[i, j] <- mean((rowSums(as.matrix(data_bin_ss_ssy[, l_labels_P[[j]]])) > 0))
    }
  }
  
  # Normalize
  a_qu_OR_norm <- matrix(NA, tot_qu, 4)
  for(i in 1:tot_qu) for(j in 1:4) a_qu_OR_norm[i, j] <- m_qu_filter[i, 2]*df_counts_qu$n_climate[i]* a_qu_OR[i,j] / df_counts_qu$n_total[i]
  
  # Return
  return(a_qu_OR_norm)
} # eoF


# ----- Individual Items -----
ComputeItems <- function(data_bin_ss) {
  
  a_qu_items <- array(NA, dim=c(tot_qu, 28)) 
  df_ai_pub_qunthyear <-  as.POSIXct(data_bin_ss$publication_date, tz = "UTC")
  df_ai_pub_qunthyear <- paste0(format(df_ai_pub_qunthyear, "%Y"), "-", quarters(df_ai_pub_qunthyear))
  
  for(i in 1:tot_qu) {
    data_bin_ss_ssy <- data_bin_ss[df_ai_pub_qunthyear == df_counts_qu$date[i], ]
    for(j in 1:28) {
      a_qu_items[i, j] <- mean(as.matrix(data_bin_ss_ssy[, unlist(l_labels_P)[j] ])) 
    }
  }
  a_qu_items <- as.data.frame(a_qu_items)
  colnames(a_qu_items) <- unlist(l_labels_P)
  
  # Normalize
  m_qu_items_norm <- as.data.frame(matrix(NA, tot_qu, 28))
  for(i in 1:tot_qu) for(j in 1:28) m_qu_items_norm[i, j] <- m_qu_filter[i, 2]*df_counts_qu$n_climate[i]* a_qu_items[i,j] / df_counts_qu$n_total[i]
  colnames(m_qu_items_norm) <- unlist(l_labels_P)
  
  # Return
  return(m_qu_items_norm)
} # eoF


# ------------------------------------------
# -------- Plot Figure 1 (Time Series) -----
# ------------------------------------------

plotFig1 <- function(df) {

  df$year    <- 2010 + (df$time - 1) %/% 4
  df$quarter <- ((df$time - 1) %% 4) + 1
  
  # quarter end months: 3, 6, 9, 12
  end_month <- df$quarter * 3
  
  # first day of quarter-end month, then jump to last day of that month
  df$date <- as.Date(paste(df$year, end_month, "01", sep = "-"))
  df$date <- as.Date(cut(df$date, "month")) + months(1) - 1
  
  climate_events <- list(
    
    # --- COPs ---
    "COP21 Paris" = as.Date("2015-11-30"),
    "COP26 Glasgow" = as.Date("2021-10-31"),
    # "COP27 Sharm el-Sheikh" = as.Date("2022-11-06"),
    "COP28 Dubai" = as.Date("2023-11-30"),
    "COP29 Baku" = as.Date("2024-11-11"),
    "COP30 Belem" = as.Date("2025-11-10"),
    
    # "The Guardian Climate Pledge" = as.Date("2019-10-15"),
    
    # --- IPCC (summarized: WG I only + 1.5°C SR) ---
    "IPCC AR5 WG I" = as.Date("2013-09-27"),
    "IPCC Special Report on 1.5°C" = as.Date("2018-10-08"),
    "IPCC AR6 WG I" = as.Date("2021-08-09"),
    
    # --- Climate movement ---
    "Global Fridays for Future Strike" = as.Date("2019-03-15"),
    # "Largest Global Climate Strike" = as.Date("2019-09-20"),
    # "Extinction Rebellion Actions" = as.Date("2019-10-07"),
    
    "XR Actions & Climate Pledge" = as.Date("2019-10-10"),
    "Just Stop Oil Actions" = as.Date("2022-10-01"),
    
    # --- Extreme weather ---
    # "Pacific Northwest heat dome" = as.Date("2021-06-25"),
    "Pakistan monsoon floods" = as.Date("2022-06-14"),
    
    # --- Global shocks ---
    "WHO declares global pandemic" = as.Date("2020-03-11"),
    "Russia invades Ukraine" = as.Date("2022-02-24")
  )
  
  y_max  <- 12
  y_text <- y_max * 0.85
  eps    <- y_max * 0.06
  
  events_df <- data.frame(
    event = names(climate_events),
    date  = as.Date(unlist(climate_events)),
    y_text = y_text,
    y_end  = y_text - eps
  )
  
  title <- "Climate Reporting as % of Total Reporting 2010-2024"
  lt <- setNames(rep(1, nlevels(df$variable)), levels(df$variable))
  lt[levels(df$variable)[1:2]] <- 2
  
  # --- year scaffolding for centered labels + brackets ---
  years <- 2010:2025
  
  year_df <- data.frame(
    year  = years,
    start = as.Date(paste0(years, "-01-01")),
    end   = as.Date(paste0(years, "-12-31")),
    mid   = as.Date(paste0(years, "-07-01"))
  )
  
  year_df <- tibble(
    year  = years,
    start = as.Date(paste0(years, "-03-31")),  # Q1 end
    end   = as.Date(paste0(years, "-12-31")),  # Q4 end
    mid   = start + (end - start) / 2
  )
  
  # where to draw the brackets (below 0)
  y_bracket <- -0.7
  y_tick    <- -0.3
  
  p <- ggplot(df, aes(x = date, y = estimate,
                      color = variable, fill = variable,
                      linetype = variable, group = variable)) +
    
    # --- Year boundary guides (Jan 1 each year) ---
    # geom_vline(
    #   data = data.frame(x = as.Date(paste0(2010:2026, "-01-01"))),
    #   aes(xintercept = x),
    #   inherit.aes = FALSE,
    #   linewidth = 0.3,
    #   linetype = "dotted",
    #   color = "grey70"
    # ) +
    # 
    # --- Year brackets under the x-axis ---
    geom_segment(
      data = year_df,
      aes(x = start, xend = end, y = y_bracket, yend = y_bracket),
      inherit.aes = FALSE,
      linewidth = 0.5,
      color = "grey50"
    ) +
    geom_segment(
      data = year_df,
      aes(x = start, xend = start, y = y_bracket, yend = y_tick),
      inherit.aes = FALSE,
      linewidth = 0.5,
      color = "grey50"
    ) +
    geom_segment(
      data = year_df,
      aes(x = end, xend = end, y = y_bracket, yend = y_tick),
      inherit.aes = FALSE,
      linewidth = 0.5,
      color = "grey50"
    ) +
    
    # --- climate event vertical line segments (stop near label) ---
    geom_segment(
      data = events_df, aes(x = date, xend = date, y = 0, yend = y_text - eps),
      color = "grey50", linewidth = 0.4, linetype = "dotted", inherit.aes = FALSE
    ) + # --- event labels ---
    geom_text(
      data = events_df, aes(x = date + 25, y = y_text - eps/2, label = event),
      angle = 90, vjust = 0, hjust = 0, size = 4, color = "grey40", inherit.aes = FALSE
    ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    # geom_point() +
    scale_linetype_manual(values = lt) +
    
    labs(
      x = "", y = "Percentage of Climate Reporting",
      color = NULL, fill = NULL, linetype = NULL, title = title
    ) +
    
    geom_segment(
      aes(x = as.Date("2010-01-01"),
          xend = as.Date("2010-01-01"),
          y = 0,
          yend = 16),
      inherit.aes = FALSE,
      color = "grey50",
      linewidth = 0.50
    ) +
    theme_minimal() +
    theme(
      # axis.line.y = element_line(color = "grey40", linewidth = 0.6),
      axis.ticks.y = element_line(color = "grey50"),
      legend.text = element_text(size = 10),
      legend.position = c(0.005, 0.975),
      legend.justification = c("left", "top"),
      legend.background = element_rect(
        fill = scales::alpha("white", 0.7),
        color = NA
      ),
      axis.text.x = element_text(margin = margin(t = -14)),
      plot.margin = margin(t = 10, r = 10, b = 0, l = 0),
      plot.title = element_text(hjust = 0.50, size = 16, margin = margin(b = 10)),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    
    # --- X axis: centered year labels ---
    scale_x_date(
      breaks = year_df$mid,          # July 1 each year
      labels = year_df$year,         # show just the year
      limits = c(as.Date("2010-01-01"), as.Date("2025-12-31")),
      expand = expansion(mult = c(0.0, 0.0))
    ) +
    
    # --- Y axis (no hard limits here; set via coord_cartesian to allow brackets below 0) ---
    scale_y_continuous(
      breaks = seq(0, 16, 2),
      limits = c(-1, 16),
      labels = function(x) paste0(x, "%")
    ) +
    # coord_cartesian(clip = "off")
    coord_cartesian(ylim = c(-0.80, 16), clip = "off")
  
  p
  
} # eoF