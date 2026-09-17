library(dplyr)
library(tidyr)
library(xtable)
library(stringr)
library(ggplot2)
library(jsonlite)

dice <- function(x, y) {
  1 - proxy::dist(rbind(x, y), method = 'Dice')[1]
}

edice <- function(x1, x2) {
  p1 <- mean(x1, na.rm = TRUE)
  p2 <- mean(x2, na.rm = TRUE)
  2 * p1 * p2 / (p1 + p2)
}

permutation_test <- function(x1, x2) {
  dices <- replicate(1000, {
    dice(x1, sample(x2))
  })
  pval <- mean(dices > dice(x1, x2))
  pval
}

get_mode <- function(x) {
  ifelse(sum(x == 1) > 1, 1, 0)
}


plot_fig <- function(df, variable1, variable2, title, y2lab = 'Dice similarity') {
  variable1_sym <- sym(variable1)  # Convert string to symbol
  variable2_sym <- sym(variable2)  # Convert string to symbol
  
  p <- ggplot(df, aes(x = factor(question))) +
    geom_bar(aes(y = !!variable1_sym),  # Use !! to unquote the symbol
             stat = "identity", position = "dodge", alpha = 0.6) +
    geom_point(aes(y = !!variable2_sym * max(df[[variable1]])),
               size = 3, position = position_dodge(width = 0.9)) +
    scale_y_continuous(
      name = "Proportion difference",
      sec.axis = sec_axis(~ . / max(df[[variable1]]), name = y2lab)  # Corrected max() usage
    ) +
    labs(x = "Question", title = "Base Difference and Accuracy per Rater") +
    theme_minimal() +
    ggtitle(title) +
    theme(
      legend.position = "top",
      plot.title = element_text(size = 16, hjust = 0.50),
      axis.text.x = element_text(angle = 90)
    )
  
  p
}

##########################################
### THIS ORDER NEEDS TO BE FIXED AS BELOW
### TO BE IN LINE WITH THE GOOGLE FORM
##########################################
causes <- c("causes_human",
            "causes_fossil_fuels",
            "causes_agriculture",
            "causes_overconsumption",
            "causes_dev_countries_contrib",
            "causes_economic_system",
            "causes_affluent_footprint")

impact <- c("impact_adverse",
            "impact_extreme_weather",
            "impact_sea_level_rise",
            "impact_biodiversity_loss",
            "impact_food_water_security",
            "impact_migration",
            "impact_science_threat")

mitigation <- c("mitigation_reduction_fossil",
                "mitigation_renewable_energy",
                "mitigation_diets_livestock",
                "mitigation_electrification",
                "mitigation_carbon_tax",
                "mitigation_reduce_flying",
                "mitigation_net_zero")

adaptation <- c("adaptation_infrastructure_resilience",
                "adaptation_ecosystem_restoration",
                "adaptation_agriculture_resilience",
                "adaptation_human_migration",
                "adaptation_public_health",
                "adaptation_countries",
                "adaptation_disaster_preparedness")

selected_questions <- c(causes, impact, mitigation, adaptation)

df_human <- read.csv('Files/human_validation.csv') %>% 
  select(-Timestamp, -Any.comments.)

colnames(df_human) <- c('rater', 'url', 'mainly_climate', selected_questions)

df_human <- df_human %>% 
  mutate(
    url = gsub('^\\d+\\.\\s+', '', url),
    across(
      # Include Mentioned for df_human$mitigation_carbon_tax
      .cols = where(~ all(.x %in% c("None", "Weakly Imply", "Strongly Imply", "Mention", "Mentioned"))),
      .fns = ~ recode(.x,
                      "None" = 0,
                      "Weakly Imply" = 1,
                      "Strongly Imply" = 2,
                      "Mention" = 3,
                      "Mentioned" = 3)
    )
  ) %>% select(-mainly_climate)


df_ai <- readRDS('Files/G_Df_bin_Filtered_mainly_7.0.2-Mistral-Large-.RDS') %>%
  select(url, all_of(selected_questions)) %>%
  filter(url %in% df_human$url) %>%
  mutate(rater = 'gpt') %>%
  select(rater, url, all_of(selected_questions))

# Create the human_caused-OR question
# df_human$causes_human_or <- as.numeric(apply(df_human[, causes], 1, function(x) any(x > 0)))
# df_ai$causes_human_or <- as.numeric(apply(df_ai[, causes], 1, function(x) any(x > 0)))

df <- rbind(df_human, df_ai) %>% 
  pivot_longer(cols = -c(rater, url), names_to = 'question', values_to = 'value')


#############################
### Let's look at differences in proportions of NONE and (MENTION OR IMPLY)
#############################
df_wide <- df %>%
  pivot_wider(names_from = rater, values_from = value) %>% 
  mutate(
    gpt_bin = as.numeric(gpt > 0),
    Fabian_bin = as.numeric(Fabian > 0),
    Simon_bin = as.numeric(Simon > 0),
    Jonas_bin = as.numeric(Jonas > 0),
    Consensus_bin = as.numeric(Consensus > 0)
  )

df_wide$url <- factor(df_wide$url, levels = df_ai$url)
stopifnot(levels(df_wide$url) == df_ai$url)

# Number the URLs
number_map <- list()
for (i in seq(49)) {
  number_map[df_ai$url[i]] <- as.character(i)
}

df_wide <- df_wide %>%
  arrange(url) %>% 
  mutate(number = unname(unlist(number_map[url])))


#############################################
### COMPUTE INTER-RATER RELIABILITY OF HUMANS
#############################################
library(irr)

df_mat <- df_wide %>%
  dplyr::select(Fabian_bin, Jonas_bin, Simon_bin)

# Fabian vs Jonas
irr::kripp.alpha(
  t(df_wide[, c("Fabian_bin", "Jonas_bin")]),
  method = "nominal"
)

# Jonas vs Simon
irr::kripp.alpha(
  t(df_wide[, c("Jonas_bin", "Simon_bin")]),
  method = "nominal"
)

# Fabian vs Simon
irr::kripp.alpha(
  t(df_wide[, c("Fabian_bin", "Simon_bin")]),
  method = "nominal"
)

irr::kripp.alpha(t(df_mat), method = "nominal")

df_sum <- df_wide %>%
  group_by(question) %>%
  summarize(
    
    baserate_human = mean(Consensus_bin),
    baserate_ai = mean(gpt_bin),
    accuracy_consensus_gpt = mean(Consensus_bin == gpt_bin, na.rm = TRUE),
    eaccuracy_consensus_gpt = (
      mean(Consensus_bin, na.rm = TRUE) * mean(gpt_bin, na.rm = TRUE) +
        (1 - mean(Consensus_bin, na.rm = TRUE)) * (1 - mean(gpt_bin, na.rm = TRUE))
    ),
    dice_consensus_gpt = dice(Consensus_bin, gpt_bin),
    edice_consensus_gpt = edice(Consensus_bin, gpt_bin),
    krippendorf_consensus_gpt = irr::kripp.alpha(
      rbind(Consensus_bin, gpt_bin),
      method = "nominal"
    )$value,
    
    # Permutation test p-value
    pval = permutation_test(Consensus_bin, gpt_bin)
    
  ) %>%
  mutate(
    question = factor(question, levels = selected_questions)
  )

df_wide$category <- sapply(
  strsplit(as.character(df_wide$question), '_'), function(x) {
    x[1]
  })

set.seed(1)
nr_boot <- 1000

# Bootstrap estimates
boot_res <- lapply(seq_len(nr_boot), function(i) {
  df_wide %>%
    dplyr::filter(question %in% selected_questions) %>%
    dplyr::group_by(question) %>%
    dplyr::slice_sample(n = nrow(.), replace = TRUE) %>%
    dplyr::summarize(
      accuracy = mean(Consensus_bin == gpt_bin, na.rm = TRUE),
      dice = dice(Consensus_bin, gpt_bin),
      edice = edice(Consensus_bin, gpt_bin),
      # krippendorf_consensus_gpt = irr::kripp.alpha(
      #   rbind(Consensus_bin, gpt_bin),
      #   method = "nominal"
      # )$value,
      .groups = "drop"
    ) %>%
    dplyr::mutate(iter = i)
})

# Bootstrap percentile intervals
boot_ci <- dplyr::bind_rows(boot_res) %>%
  dplyr::group_by(question) %>%
  dplyr::summarize(
    accuracy_lo  = quantile(accuracy, 0.025, na.rm = TRUE),
    accuracy_hi = quantile(accuracy, 0.975, na.rm = TRUE),
    dice_lo      = quantile(dice, 0.025, na.rm = TRUE),
    dice_hi     = quantile(dice, 0.975, na.rm = TRUE),
    edice_lo     = quantile(edice, 0.025, na.rm = TRUE),
    edice_hi    = quantile(edice, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

df_sum <- df_sum %>%
  dplyr::left_join(boot_ci, by = "question")

write.csv(df_sum, 'Files/validation_results.csv', row.names = FALSE)
