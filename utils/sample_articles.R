library(dplyr)
# Sample articles to validate


# Only use articles that are mainly about climate change
df <- readRDS('Files/G_Df_bin_Filtered_mainly_7.0.2-Mistral-Large-.RDS')
  # # remove questions we don't care about
  # select(-seq(14))

# Remove articles larger than 80% or all articles
length_quantiles <- quantile(df$wordcount, seq(0, 1, 0.1))
df <- df %>% filter(wordcount <= length_quantiles[9])

# Function to select articles to fulfill the x-constraint
select_articles <- function(A, x) {
  # Input:
  # matrix: n x m binary matrix (articles x questions)
  # x: number of 1s and 0s needed per question

  # Number of articles and questions
  n <- nrow(A)
  m <- ncol(A)

  number_of_ones <- apply(A, 2, sum)
  not_fulfilled <- number_of_ones < x

  if (any(not_fulfilled)) {
    print(paste0('Not possible to fulfill constrained for ', sum(not_fulfilled), ' number of questions'))
    print(paste0('The questions where the constrained cannot be fulfilled are: ', paste0(which(not_fulfilled), collapse = ', ')))
  }

  # Initialize the set of selected articles
  selected_articles <- c()
  question_status <- data.frame(question = 1:m, ones = 0, zeros = 0)
  question_status_cum <- c()

  # Function to calculate the score for an article
  calculate_score <- function(article, question_status) {
    ones_needed <- pmax(0, x - question_status$ones)
    zeros_needed <- pmax(0, x - question_status$zeros)

    # Article contribution to ones and zeros
    article_ones <- colSums(A[article, , drop = FALSE])
    article_zeros <- 1 - article_ones

    # Compute score based on remaining needs
    score <- sum(pmin(article_ones, ones_needed)) + sum(pmin(article_zeros, zeros_needed))
    return(score)
  }

  all_articles <- seq(n)

  # While loop to select articles until the constraint is satisfied or we selected all articles
  while (length(selected_articles) <= n) {

    # Check if the x-constraint is fulfilled
    if (all(question_status$ones >= x & question_status$zeros >= x)) {
      break
    }

    # Calculate scores for all remaining articles
    if (length(selected_articles) > 0) {
      remaining_articles <- all_articles[-selected_articles]
    } else {
      remaining_articles <- all_articles
    }

    scores <- sapply(remaining_articles, function(i) calculate_score(i, question_status))

    # Adding more questions does not improve the situation anymore
    if (all(scores == 0)) {
      break
    }

    # Select the article with the highest score
    best_article_ix <- which.max(scores)
    best_article <- remaining_articles[best_article_ix]

    # Update the selected articles set
    selected_articles <- c(selected_articles, best_article)

    # Update the question_status
    question_status$ones <- question_status$ones + A[best_article, ]
    question_status$zeros <- question_status$zeros + (1 - A[best_article, ])

    question_status_cum
  }

  # Return the results
  return(list(selected_articles = selected_articles,
              question_status = question_status))
}

causes <- c("causes_human",
            "causes_fossil_fuels",
            "causes_agriculture",
            "causes_overconsumption",
            "causes_dev_countries_contrib",
            "causes_economic_system",
            "causes_consumerism",
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
               "mitigation_transport",
               "mitigation_reduce_flying",
               "mitigation_net_zero")

adaptation <- c("adaptation_infrastructure_resilience",
               "adaptation_ecosystem_restoration",
               "adaptation_agriculture_resilience",
               "adaptation_human_migration",
               "adaptation_public_health",
               "adaptation_countries",
               "adaptation_disaster_preparedness",
               "adaptation_urban_climate_resilience")

selected_questions <- c(causes, impact, mitigation, adaptation)

A <- df %>%
  select(all_of(selected_questions)) %>% 
  mutate(
    across(
      everything(),
      ~ as.numeric(. > 0)  # TRUE becomes 1, FALSE becomes 0
  )) %>% 
  as.matrix()

table(as.matrix(A))
apply(as.matrix(A), 2, table)
apply(A, 2, sum)

A <- apply(A, 2, as.numeric)
ix <- order((unname(apply(A, 2, sum))))
rev(apply(A, 2, sum)[ix])

res <- select_articles(A, 10)

counts <- apply(A, 2, sum)

df_sel <- df[res$selected_articles, ]
apply(df_sel %>% select(selected_questions), 2, function(x) sum(x > 0))
apply(df_sel %>% select(selected_questions), 1, function(x) sum(x > 0))

df_sel$url_numbered <- paste0(seq(nrow(df_sel)), '. ', df_sel$url)
write.csv(df_sel, 'validation_guardian_articles.csv', row.names = FALSE)
