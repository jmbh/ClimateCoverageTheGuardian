library(dplyr)

## Here we look at the articles that have a 'Yes' on the various aspects
df <- readRDS('Files/Guard_Df_binarized_rdy_7.0.2-Mistral-Large-.RDS')

# Read two articles for each aspect

# Causes
causes <- c("causes_human",
            "causes_fossil_fuels",
            "causes_agriculture",
            "causes_overconsumption",
            "causes_economic_system",
            "causes_affluent_footprint",
            "causes_dev_countries_contrib")

# Impact
impact <- c("impact_adverse",
            "impact_extreme_weather",
            "impact_sea_level_rise",
            "impact_biodiversity_loss",
            "impact_food_water_security",
            "impact_migration",
            "impact_science_threat")

# Mitigation
mitigation <- c("mitigation_reduction_fossil",
                "mitigation_renewable_energy",
                "mitigation_electrification",
                "mitigation_diets_livestock",
                "mitigation_reduce_flying",
                "mitigation_carbon_tax",
                "mitigation_net_zero")

# Adaptation
adaptation <- c("adaptation_infrastructure_resilience",
                "adaptation_agriculture_resilience",
                "adaptation_ecosystem_restoration",
                "adaptation_public_health",
                "adaptation_disaster_preparedness",
                "adaptation_human_migration",
                "adaptation_countries")


# Only for mainly
get_top <- function(df, aspect, top = 3) {
  set.seed(1)
  df_sel <- df[df$mainly_climate_change == 1 & df[[aspect]] == 1, ]
  df_sel %>% sample_n(top) %>%
    select(url, year, wordcount, mainly_climate_change, !!aspect, summarize_hundred_words)
}

get_top(df, causes[[1]])
get_top(df, causes[[2]])
get_top(df, causes[[3]])
get_top(df, causes[[4]])
get_top(df, causes[[5]])
get_top(df, causes[[6]])
get_top(df, causes[[7]])

get_top(df, impact[[1]])
get_top(df, impact[[2]])
get_top(df, impact[[3]])
get_top(df, impact[[4]])
get_top(df, impact[[5]])
get_top(df, impact[[6]])
get_top(df, impact[[7]])

get_top(df, mitigation[[1]])
get_top(df, mitigation[[2]])
get_top(df, mitigation[[3]])
get_top(df, mitigation[[4]])
get_top(df, mitigation[[5]])
get_top(df, mitigation[[6]])
get_top(df, mitigation[[7]])

get_top(df, adaptation[[1]])
get_top(df, adaptation[[2]])
get_top(df, adaptation[[3]])
get_top(df, adaptation[[4]], top = 5)
get_top(df, adaptation[[5]])
get_top(df, adaptation[[6]])
# First article was already mentioned (for disaster preparedness)
get_top(df, adaptation[[7]])
