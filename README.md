## Reproducibility Archive

This archive contains files and code to reproduce the results in our paper.

The first set of scripts is contained in the Python file `guardian_get_daily_counts.py` which queries both the total number of articles published monthly from 2010-2025 and all articles in this time period which include the keyword "climate". The former counts, before and after filtering (see paper), can be found in `Files/guardian_monthly_counts_unfiltered.csv` and `Files/guardian_monthly_counts.csv`. The latter dataset is available at `Files/articles.csv` and contains 92,221 articles. The latter dataset is also available

We took a stratified (by year) sample from these 92,221 articles and sent them through our AI pipeline. We are legally not allowed to share all full texts. We therefore share our analysis pipeline only after downloading it from our database, adding the word count of articles, and deleting the full text. From this point forward, which includes still the raw output of the AI pipeline, everything is fully reproducible. We are happy to share the full texts for non-commercial research purposes upon request.

- `0_Helpers` contains various helper functions for data processing and plotting
- `1_Process.R` Applies various processing steps to the AI data, including parsing the JSON output and binarizing the responses. This file takes the file `Files/Guard_RawData_With_WordCount7.0.2-Mistral-Large-.RDS` as input. We had to omit the earlier preprocessing steps producing this `.RDS` file from this reproducibility archive (database query and computing word count), because it would require us to share the full texts of articles, which we are legally not permitted to share openly.
- `2_Filter_and_Subsetting.R` Takes the processed data and applies the different data quality and "narrowness" filters we report in the paper
- `3_Plotting_Meta` contains some settings, labels, and colors we use throughout for plotting
- `4_Analysis_Time_Quarterly.R` aggregates the AI responses to the quarterly level and produdes Figure 1 in the paper and Figure S1 in the appendix. This script also normalizes the proportions of articles reporting on a given aspect using the total number of articles with the keyword "climate" (accounting for the fact that we ran the AI only on a subset of all available data) and using the total number of articles published in The Guardian in a given quarter. The former is provided in the RDS file `Guard_Total_noAI.RDS`. This file contains all the articles in `Files/articles.csv`, but with irrelevant articles filtered out (see paper).The latter information is provided via the CSV `guardian_monthly_counts.csv` in the folder `Files`.
- `5_Analysis_Topic_Newspaper.R` aggregates the AI responses to the newspaper level and produces Figure 2 in the paper and various appendix figures. This script also compares the results from The Guardian to a recent paper analyzing the same variables in Germany. The German data is provided with the file `Datasum_prop_2x2_mainly_7.0.2-Mistral-Large-.RDS` in folder `Files`
- `6_Analysis_Correlational.R` produces the correlational analysis shown in the appendix

The script `utils/guardian_get_daily_counts.py` queries the total number
of articles published per day/month.
