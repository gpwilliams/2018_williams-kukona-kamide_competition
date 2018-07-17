Experiment 2 (Visual Competition)
================
Williams, G.P., Kukona, A., & Kamide, Y.

This document provides a reproducible analysis for Experiment 2 of the paper *Spatial narrative context modulates semantic (but not visual) competition during discourse processing* by Glenn P. Williams, Anuenue Kukona, & Yuki Kamide.

Packages, Functions, and Data
=============================

``` r
load("../data/visual_data.Rdata")
visual_demo <- read.csv("../data/visual_demographics.csv")
```

Sample Demographics
===================

``` r
# gender count
visual_gender_n <- visual_demo %>% 
  filter(included == "yes") %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  ungroup()

# age averages
visual_ages <- visual_demo %>%
  filter(included == "yes") %>%
  summarise(min_age = min(age),
            max_age = max(age),
            mean_age = mean(age),
            sd_age = sd(age)
            )
```

60 (16 male) native speakers of English from the University of Dundee community (aged 18- 42, *M* = 19.85, *SD* = 3.51) took part in this study for partial course credit. All participants had uncorrected vision, wore soft contact lenses, or wore spectacles, and had no known auditory, visual, or language disorders.

Define Variables for Both Analyses
==================================

``` r
# define time window
t_window <- c("crit_noun_on","crit_noun_off")

# reset interest area labels for later subsetting
ias <- c("c", "d")

# establish conditions for later summaries
conds <- c("condition", "IA2")

# define variables for centering
centering_list <- list(factors = c("condition", "IA2"),
                       levels = c("together", "c")
                       )

# define main model formula for frequentist and Bayesian analyses
full_formula <- as.formula("asin ~ condition_c * IA2_c + 
                           (1 | by) + (1 | by: condition_c) + (1 | by: IA2_c)"
                           )
```

Noun + 200ms
============

Data Preparation
----------------

We subsetted the data to the time window of the critical noun (e.g. "bat") + 200ms. Within this time window, we further subsetted our data to fixations on the competitor and the average fixations on the two distractors (henceforth, distractor).

``` r
# shift window by how much?
window_shift <- 200

# tidy data before analysis & make distractor looks the average across the two
tidy_data <- visual_data %>%
  mutate(item = as.factor(item),
         d = (d1 + d2) / 2,
         time_0 = time - UQ(as.name(t_window[1]))
         ) %>%
  select(-c(t, d1, d2))

# subset data to time window
sub_data <- subset_to_window(tidy_data, "time_0", timeM, t_window, window_shift)
long_data <- make_data_long(sub_data, "IA2", ias, "fix")
```

Data Aggregation
----------------

Prior to running our analyses, we aggregated the data first by subjects, and then by items, across the entire time window. This aggregation was conducted in order to account for a heavy bias in fixation probabilities within trials for each subject, whereby within subjects and items several trials could consist of 0% or 100% looking towards one interest area.

Additionally, we transformed our dependent variable, the proportion of fixations on a given interest area, into arcsin square root transformed proportions. This transformation attempts to account for the bounded nature of the underlying binomial response data that makes up a proportion. This transformation was used to account for the lack of homogeneity in the variance across the range of possible outcomes, with a larger variance towards the endpoints (i.e. 0 or 1) (Mirman, 2014). In effect, the arcsin square root transformation pulls out the distribution of proportions around the tails of the data.

``` r
# aggregate data by subjects, centre variables, and make new dvs
by_subj <- aggregate_data(long_data, "subject", conds, "fix")
by_subj <- centre_many(by_subj, centering_list)
by_subj <- make_dvs(by_subj, "y", "N")

# aggregate data by items, centre variables, and make new dvs
by_items <- aggregate_data(long_data, "item", conds, "fix")
by_items <- centre_many(by_items, centering_list)
by_items <- make_dvs(by_items, "y", "N")
```

Model Structure
---------------

We conducted one main analysis which looked at the main effects and interactions between the condition and interest area variables. To perform this analysis, we used a hierarchical mixed-effects model with an arcsin square root transformation. Our models contained fixed effects of condition and interest area (centred) and their interaction. Additionally, our random effects structure took the form of random intercepts by subjects/items, and random intercepts of subjects/items nested within condition, and subjects/items nested within interest area.

In all models, we used the maximal converging random effects structure (Barr et al., 2013).

``` r
# by subjects main model: testing for main effects and interactions
by_subj_model <- tidy_model(lmer(full_formula, data = by_subj))

# by items main model
by_items_model <- tidy_model(lmer(full_formula, data = by_items))
```

Descriptive Statistics
----------------------

Below, we show a table of the means and standard deviations for the proportion of fixations and arcsin squareroot transformed fixations on each interest area within each condition aggregated by subjects.

``` r
# generate descriptives
descriptives_table <- by_subj %>%
  group_by(IA2, condition) %>%
  summarise_at(vars(prop, asin),
               funs(mean, sd)) %>%
  round_three()

# print output
kable(descriptives_table)
```

| IA2 | condition |  prop\_mean|  asin\_mean|  prop\_sd|  asin\_sd|
|:----|:----------|-----------:|-----------:|---------:|---------:|
| c   | apart     |       0.167|       0.404|     0.099|     0.138|
| c   | together  |       0.164|       0.408|     0.071|     0.098|
| d   | apart     |       0.157|       0.399|     0.069|     0.098|
| d   | together  |       0.159|       0.403|     0.062|     0.089|

Main model
----------

For both by-subjects and by-items analyses, we found no signficant differences between the levels of either factor and their interaction.

``` r
merge_tables(by_subj_model, by_items_model) %>% kable()
```

| aggregate | term                |  estimate|  std.error|  statistic|  p\_value|
|:----------|:--------------------|---------:|----------:|----------:|---------:|
| Subjects  | (Intercept)         |     0.403|      0.010|     40.321|     0.000|
| Subjects  | condition\_c        |     0.004|      0.010|      0.353|     0.724|
| Subjects  | IA2\_c              |     0.005|      0.013|      0.417|     0.677|
| Subjects  | condition\_c:IA2\_c |     0.000|      0.021|     -0.003|     0.997|
| Items     | (Intercept)         |     0.409|      0.008|     54.204|     0.000|
| Items     | condition\_c        |     0.000|      0.011|     -0.012|     0.991|
| Items     | IA2\_c              |     0.008|      0.015|      0.505|     0.613|
| Items     | condition\_c:IA2\_c |    -0.006|      0.022|     -0.291|     0.771|

Evaluating Evidence for the Null Hypothesis
-------------------------------------------

The effect of condition had a non-significant effect of fixations to the distractor across conditions. Moreover, the effect of interest area had a non-significant effect on fixations in the Apart condition. However, we cannot take this as evidence of the absence of any effect in these cases. One way to evaluate support for the null hypothesis is through using Bayes Factors. Here, we use the Bayesian Information Criterion (BIC) approximation to the Bayes factor, which affords easy computation using the same model specifications in lme4 used to evaluate support for the alternative hypothesis under the NHST procedure. A discussion of how to compute this approximation, as well as the strengths and weaknesses of this method are outlined in Wagenmakers (2007).

``` r
# by-subjects: evaluating each effect in isolation
all_subj_max <- lmer(full_formula, data = by_subj, REML = F)
all_subj_cond <- update(all_subj_max, . ~ . - condition_c)
all_subj_IA <- update(all_subj_max, . ~ . - IA2_c)
all_subj_int <- update(all_subj_max, . ~ . - condition_c : IA2_c)

# by_subjects: BFs against model H1 (full model)
by_subj_bf_vs_H1 <- 
  data.frame(
    Aggregate = rep("by-subjects", 4),
    Model = c("H1: full model", 
              "H2: no condition", 
              "H3: no interest area", 
              "H4: no interaction"
              ),
    BIC = c(BIC(all_subj_max), 
            BIC(all_subj_cond), 
            BIC(all_subj_IA),
            BIC(all_subj_int)
            ),
    BF_comparison = c("-", "BF_21", "BF_31", "BF_41"),
    BF = c(NA,
           exp((BIC(all_subj_max) - BIC(all_subj_cond))/2),
           exp((BIC(all_subj_max) - BIC(all_subj_IA))/2),
           exp((BIC(all_subj_max) - BIC(all_subj_int))/2)
           )
    )

# by-items: evaluating each effect in isolation
all_items_max <- lmer(full_formula, data = by_items, REML = F)
all_items_cond <- update(all_items_max, . ~ . - condition_c)
all_items_IA <- update(all_items_max, . ~ . - IA2_c)
all_items_int <- update(all_items_max, . ~ . - condition_c : IA2_c)

# by-items: BFs against model H1 (full model)
by_items_bf_vs_H1 <- 
  data.frame(
    Aggregate = rep("by-items", 4),
    Model = c("H1: full model", 
              "H2: no condition", 
              "H3: no interest area", 
              "H4: no interaction"
              ),
    BIC = c(BIC(all_items_max), 
            BIC(all_items_cond), 
            BIC(all_items_IA),
            BIC(all_items_int)
            ),
    BF_comparison = c("-", "BF_21", "BF_31", "BF_41"),
    BF = c(NA,
           exp((BIC(all_items_max) - BIC(all_items_cond))/2),
           exp((BIC(all_items_max) - BIC(all_items_IA))/2),
           exp((BIC(all_items_max) - BIC(all_items_int))/2)
           )
    )
```

The BIC for all models is displayed in the table below, along with the BIC approximation to the Bayes factor for all models in relation to the maximal model used to fit the data (*H*1; i.e. including main effects and interactions by both factors). The approximation to the Bayes factors compare evidence against the maximal model in relation to models containing all other factors except the one of interest; specifically, these are models without the main effect of condition (*H*2), without the main effect of interest area (*H*3), and without the interaction between interest area and condition (*H*4).

``` r
rbind(by_subj_bf_vs_H1, by_items_bf_vs_H1) %>%
  round_three() %>%
  kable()
```

| Aggregate   | Model                |       BIC| BF\_comparison |      BF|
|:------------|:---------------------|---------:|:---------------|-------:|
| by-subjects | H1: full model       |  -387.703| -              |      NA|
| by-subjects | H2: no condition     |  -393.057| BF\_21         |  14.541|
| by-subjects | H3: no interest area |  -393.007| BF\_31         |  14.185|
| by-subjects | H4: no interaction   |  -393.184| BF\_41         |  15.492|
| by-items    | H1: full model       |  -273.269| -              |      NA|
| by-items    | H2: no condition     |  -278.121| BF\_21         |  11.313|
| by-items    | H3: no interest area |  -277.858| BF\_31         |   9.919|
| by-items    | H4: no interaction   |  -278.034| BF\_41         |  10.831|

In sum, the maximal model was dispreferred in all cases, suggesting that these terms play no role in guiding fixations during the time window of the noun + 200ms.

Noun + 400ms
============

Data Preparation
----------------

We conducted the same analysis as used in the region of the noun + 200ms for the region of the noun + 400ms. This time window was selected as previous research has shown that, given long preview times (e.g. 1000ms) of the visual scene prior to noun onset, visual competition occurs approximately 100ms later than semantic competition. As our semantic competition experiment showed effects during the region of the noun + 300ms, this later region of the noun + 400ms was deemed appropriate for analysis.

``` r
# shift window by how much?
window_shift <- 400

# tidy data before analysis (make distractor looks the average across the two)
tidy_data <- visual_data %>%
  mutate(item = as.factor(item),
         d = (d1 + d2) / 2,
         time_0 = time - UQ(as.name(t_window[1]))
         ) %>%
  select(-c(t, d1, d2))

# subset data to time window
sub_data <- subset_to_window(tidy_data, "time_0", timeM, t_window, window_shift)
long_data <- make_data_long(sub_data, "IA2", ias, "fix")
```

Data Aggregation
----------------

``` r
# aggregate data by subjects, centre variables and make new dvs
by_subj <- aggregate_data(long_data, "subject", conds, "fix")
by_subj <- centre_many(by_subj, centering_list)
by_subj <- make_dvs(by_subj, "y", "N")

# aggregate data by items, centre variables and make new dvs
by_items <- aggregate_data(long_data, "item", conds, "fix")
by_items <- centre_many(by_items, centering_list)
by_items <- make_dvs(by_items, "y", "N")
```

Model Structure
---------------

``` r
# by-subjects main model: testing for main effects and interactions
by_subj_model <- tidy_model(lmer(full_formula, data = by_subj))

# by-items main model: testing for main effects and interactions
by_items_model <- tidy_model(lmer(full_formula, data = by_items))
```

Descriptive Statistics
----------------------

Below, we show a table of the means and standard deviations for the proportion of fixations and arcsin squareroot transformed fixations on each interest area within each condition aggregated by subjects.

``` r
descriptives_table <- by_subj %>%
  group_by(IA2, condition) %>%
  summarise_at(vars(prop, asin),
               funs(mean, sd)) %>%
  round_three()

# print output
kable(descriptives_table)
```

| IA2 | condition |  prop\_mean|  asin\_mean|  prop\_sd|  asin\_sd|
|:----|:----------|-----------:|-----------:|---------:|---------:|
| c   | apart     |       0.145|       0.372|     0.093|     0.136|
| c   | together  |       0.145|       0.375|     0.077|     0.120|
| d   | apart     |       0.118|       0.338|     0.067|     0.105|
| d   | together  |       0.117|       0.337|     0.061|     0.100|

Main model
----------

For both by-subjects and by-items analyses, we found a significant main effect of interest area across both conditions, with a larger proportion of transformed fixations on the competitor than the distractor. However, we found no significant main effect of, or interaction with, condition.

``` r
merge_tables(by_subj_model, by_items_model) %>% kable()
```

| aggregate | term                |  estimate|  std.error|  statistic|  p\_value|
|:----------|:--------------------|---------:|----------:|----------:|---------:|
| Subjects  | (Intercept)         |     0.356|      0.011|     32.804|     0.000|
| Subjects  | condition\_c        |     0.001|      0.011|      0.107|     0.915|
| Subjects  | IA2\_c              |     0.036|      0.014|      2.593|     0.010|
| Subjects  | condition\_c:IA2\_c |     0.004|      0.022|      0.182|     0.856|
| Items     | (Intercept)         |     0.364|      0.009|     42.106|     0.000|
| Items     | condition\_c        |    -0.002|      0.011|     -0.170|     0.865|
| Items     | IA2\_c              |     0.041|      0.017|      2.363|     0.018|
| Items     | condition\_c:IA2\_c |     0.002|      0.022|      0.084|     0.933|

Evaluating Evidence for the Null Hypothesis
-------------------------------------------

``` r
# by-subjects: evidence of main model vs. interaction (BF_12)
max_subj <- lmer(full_formula, data = by_subj, REML = F) # H2
main_subj <- update(max_subj, . ~ . - condition_c: IA2_c) # H1
bf_against_int_subj <- exp((BIC(max_subj) - BIC(main_subj))/2) # BF_12

# by-items: evidence of main model vs. interaction (BF_12)
max_items <- lmer(full_formula, data = by_items, REML = F) # H2
main_items <- update(max_items, . ~ . - condition_c: IA2_c) # H1
bf_against_int_items <- exp((BIC(max_items) - BIC(main_items))/2) # BF_12
```

The BIC approximation to the Bayes Factor shows that the the data are more likely under the model of main effects with no interaction than the model including the interaction term for both by-subjects and by-items analyses (by-subjects: *B**I**C*(*H*<sub>1</sub>) = -360.001, *B**I**C*(*H*<sub>2</sub>) = -354.554, *B**F*<sub>12</sub>≈ 15.234; by-items: *B**I**C*(*H*<sub>1</sub>) = -258.729, *B**I**C*(*H*<sub>2</sub>) = -253.884, *B**F*<sub>12</sub>≈ 11.273). This suggests that the discourse condition plays no role in modulating visual competition during the critical noun region + 400ms.

References
==========

Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). *Random effects structure for confirmatory hypothesis testing: Keep it maximal. Journal of Memory and Language, 68*(3), 255-278. <https://doi.org/10.1016/j.jml.2012.11.001>

Mirman, D. (2014). *Growth Curve Analysis and Visualization Using R*. Boca Ranton, FL.: Chapman and Hall/CRC Press.

Wagenmakers, E.-J. (2007). A practical solution to the pervasive problems of p values. *Psychonomic Bulletin & Review, 14*(5), 779-804. <https://doi.org/10.3758/BF03194105>
