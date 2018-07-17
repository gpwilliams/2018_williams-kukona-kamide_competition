Experiment 1 (Semantic Competition)
================
Williams, G.P., Kukona, A., & Kamide, Y.

This document provides a reproducible analysis for Experiment 1 of the paper *Spatial narrative context modulates semantic (but not visual) competition during discourse processing* by Glenn P. Williams, Anuenue Kukona, & Yuki Kamide.

Packages, Functions, and Data
=============================

``` r
load("../data/semantic_data.Rdata")
semantic_demo <- read.csv("../data/semantic_demographics.csv")
```

Sample Demographics
===================

``` r
# gender count
semantic_gender_n <- semantic_demo %>% 
  filter(included == "yes") %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  ungroup()

# age averages
semantic_ages <- semantic_demo %>%
  filter(included == "yes") %>%
  summarise(min_age = min(age),
            max_age = max(age),
            mean_age = mean(age),
            sd_age = sd(age)
            )
```

60 (11 male) native speakers of English from the University of Dundee community (aged 17- 33, *M* = 20.62, *SD* = 3.66) took part in this study for partial course credit. All participants had uncorrected vision, wore soft contact lenses, or wore spectacles, and had no known auditory, visual, or language disorders.

Data Preparation
================

We subsetted the data to the time window of the critical noun (e.g. "piano") + 300ms to allow for eye-movements to be driven by semantic competition. Within this time window, we further subsetted our data to fixations on the competitor and the average fixations on the two distractors (henceforth, distractor).

``` r
# define time window
t_window <- c("crit_noun_on","crit_noun_off")

# shift window by how much?
window_shift <- 300

# reset interest area labels for later subsetting
ias <- c("c", "d")

# establish conditions for later summaries
conds <- c("condition", "IA2")

# define variables for centering
centering_list <- list(factors = c("condition", "IA2"),
                       levels = c("together", "c")
                       )

# tidy data before analysis (make distractor looks the average across the two)
tidy_data <- semantic_data %>%
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
================

Prior to running our analyses, we aggregated the data first by subjects, and then by items, across the entire time window. This aggregation was conducted in order to account for a heavy bias in fixation probabilities within trials for each subject, whereby within subjects and items several trials could consist of 0% or 100% looking towards one interest area.

Additionally, we transformed our dependent variable, the proportion of fixations on a given interest area, into arcsin square root transformed proportions. This transformation attempts to account for the bounded nature of the underlying binomial response data that makes up a proportion. This transformation was used to account for the lack of homogeneity in the variance across the range of possible outcomes, with a larger variance towards the endpoints (i.e. 0 or 1) (Mirman, 2014).

``` r
# aggregate data by subjects, center variables, and make new dvs
by_subj <- aggregate_data(long_data, "subject", conds, "fix")
by_subj <- centre_many(by_subj, centering_list)
by_subj <- make_dvs(by_subj, "y", "N")

# aggregate data by items, center variables, and make new dvs
by_items <- aggregate_data(long_data, "item", conds, "fix")
by_items <- centre_many(by_items, centering_list)
by_items <- make_dvs(by_items, "y", "N")
```

Model Structure
===============

We conducted one main analysis which looked at the main effects and interactions between the condition and interest area variables. To perform this analysis, we used a hierarchical mixed-effects model with an arcsin squareroot transformation. Our models contained fixed effects of condition and interest area (centred) and their interaction. Additionally, our random effects structure took the form of random intercepts by subjects/items, and random intercepts of subjects/items nested within condition, and subjects/items nested within interest area.

Where interactions between our fixed effects were present, we conducted pairwise comparisons to explore the simple effects of each fixed effect.

To explore the main effect of condition within each interest area, we first subsetted our data to each interest area before submitting the data to a hierarchical mixed effects model with a fixed effect of condition, and random intercepts by subjects/items.

We used a similar procedure to explore the fixed effect of interest area within each condition, but this time subsetting our data to each condition, and fitting models with a fixed effect of interest area, and random intercepts by subjects/items.

In all models, we used the maximal converging random effects structure (Barr et al., 2013). For follow-up tests, all p-values are reported using the Bonferroni correction.

``` r
# define model details ----

# main_model_formula
main_formula <- as.formula("asin ~ condition_c * IA2_c + 
                           (1 | by) + (1 | by: condition_c) + (1 | by: IA2_c)"
                           )

# condition by IA formula, IA by condition formula
cond_by_IA_formula <- as.formula("asin ~ condition + (1 | by)")
IA_by_cond_formula <- as.formula("asin ~ IA2 + (1 | by)")

# define number of comparisons for p-value correction
n_comparisons <- 4

# by-subjects ----

# main model: testing for main effects and interactions
by_subj_model <- tidy_model(lmer(main_formula, data = by_subj))

# analyse condition split by IA, then adjust p-values
by_subj_cond_IA <- test_interactions(by_subj, IA2, cond_by_IA_formula)
by_subj_cond_IA$p_value <- adjust_multiple(by_subj_cond_IA, n_comparisons)

# analyse effect of interest area split by condition, then adjust p-values
by_subj_IA_cond <- test_interactions(by_subj, condition, IA_by_cond_formula)
by_subj_IA_cond$p_value <- adjust_multiple(by_subj_IA_cond, n_comparisons)

# by-items ----

# main model: testing for main effects and interactions
by_items_model <- tidy_model(lmer(main_formula, data = by_items))

# analyse condition split by IA, then adjust p-values
by_items_cond_IA <- test_interactions(by_items, IA2, cond_by_IA_formula)
by_items_cond_IA$p_value <- adjust_multiple(by_items_cond_IA, n_comparisons)

# analyse effect of interest area split by condition, then adjust p-values
by_items_IA_cond <- test_interactions(by_items, condition, IA_by_cond_formula)
by_items_IA_cond$p_value <- adjust_multiple(by_items_IA_cond, n_comparisons)
```

Descriptive Statistics
======================

Below, we show a table of the means and standard deviations for the proportion of fixations and arcsin squareroot transformed fixations on each interest area within each condition aggregated by subjects.

``` r
# descriptives
descriptives_table <- by_subj %>%
  group_by(IA2, condition) %>%
  summarise_at(vars(prop, asin),
               funs(mean, sd)) %>%
  round_three()

# print output
descriptives_table %>% kable()
```

| IA2 | condition |  prop\_mean|  asin\_mean|  prop\_sd|  asin\_sd|
|:----|:----------|-----------:|-----------:|---------:|---------:|
| c   | apart     |       0.150|       0.386|     0.072|     0.109|
| c   | together  |       0.201|       0.451|     0.093|     0.131|
| d   | apart     |       0.153|       0.396|     0.054|     0.078|
| d   | together  |       0.149|       0.389|     0.055|     0.081|

Main model
==========

For both by-subjects and by-items analyses, we found a significant interaction between condition and interest area. As such, we performed follow-up tests to explore the direction of effect.

``` r
merge_tables(by_subj_model, by_items_model) %>% kable()
```

| aggregate | term                |  estimate|  std.error|  statistic|  p\_value|
|:----------|:--------------------|---------:|----------:|----------:|---------:|
| Subjects  | (Intercept)         |     0.406|      0.007|     56.244|     0.000|
| Subjects  | condition\_c        |     0.029|      0.013|      2.279|     0.023|
| Subjects  | IA2\_c              |     0.025|      0.013|      1.990|     0.047|
| Subjects  | condition\_c:IA2\_c |     0.072|      0.025|      2.826|     0.005|
| Items     | (Intercept)         |     0.409|      0.009|     45.378|     0.000|
| Items     | condition\_c        |     0.030|      0.010|      3.008|     0.003|
| Items     | IA2\_c              |     0.029|      0.018|      1.611|     0.107|
| Items     | condition\_c:IA2\_c |     0.073|      0.020|      3.728|     0.000|

Interactions
============

Condition by Interest Area
--------------------------

In both by-subjects and by-items analyses we found a significant difference between the two conditions (Together and Apart) on the (transformed) proportion of fixations on the competitor, with a larger proportion of fixations on the competitor in the Together (Mean = 0.201) condition than the Apart condition (Mean = 0.15). We found no significant difference between the Together (Mean = 0.149) and Apart (Mean = 0.153) conditions on the transformed proportion of fixations on the distractor.

``` r
merge_tables(by_subj_cond_IA, by_items_cond_IA) %>% kable()
```

| aggregate | group | term              |  estimate|  std.error|  statistic| p\_value |
|:----------|:------|:------------------|---------:|----------:|----------:|:---------|
| Subjects  | c     | (Intercept)       |     0.386|      0.016|     24.830| 0.000    |
| Subjects  | c     | conditiontogether |     0.065|      0.022|      2.957| 0.012    |
| Subjects  | d     | (Intercept)       |     0.396|      0.010|     38.569| 0.000    |
| Subjects  | d     | conditiontogether |    -0.007|      0.013|     -0.552| 1.000    |
| Items     | c     | (Intercept)       |     0.391|      0.017|     23.433| 0.000    |
| Items     | c     | conditiontogether |     0.066|      0.017|      3.828| 0.000    |
| Items     | d     | (Intercept)       |     0.398|      0.012|     33.159| 0.000    |
| Items     | d     | conditiontogether |    -0.007|      0.009|     -0.757| 1.000    |

Interest Area by Condition
--------------------------

In both by-subjects and by-items analyses we found a significant difference between the (transformed) proportion of fixations on the two interest areas (Competitor and Distractor) in the Together condition only. Here, we found a larger proportion of fixations on the competitor (Mean = 0.201) than the distractor (Mean = 0.149). In the Apart condition, we found no significant difference between the proportions of fixations on the competitor (Mean = 0.15) and distractor (Mean = 0.153).

``` r
merge_tables(by_subj_IA_cond, by_items_IA_cond) %>% kable()
```

| aggregate | group    | term        |  estimate|  std.error|  statistic| p\_value |
|:----------|:---------|:------------|---------:|----------:|----------:|:---------|
| Subjects  | apart    | (Intercept) |     0.386|      0.012|     31.606| 0.000    |
| Subjects  | apart    | IA2d        |     0.011|      0.017|      0.616| 1.000    |
| Subjects  | together | (Intercept) |     0.451|      0.014|     32.030| 0.000    |
| Subjects  | together | IA2d        |    -0.061|      0.020|     -3.080| 0.008    |
| Items     | apart    | (Intercept) |     0.391|      0.013|     29.327| 0.000    |
| Items     | apart    | IA2d        |     0.008|      0.019|      0.405| 1.000    |
| Items     | together | (Intercept) |     0.457|      0.016|     29.208| 0.000    |
| Items     | together | IA2d        |    -0.066|      0.022|     -2.971| 0.012    |

Evaluating Evidence for the Null Hypothesis
===========================================

The effect of condition had a non-significant effect of fixations to the distractor across conditions. Moreover, the effect of interest area had a non-significant effect on fixations in the Apart condition. However, we cannot take this as evidence of the absence of any effect in these cases. One way to evaluate support for the null hypothesis is through using Bayes Factors. Here, we use the Bayesian Information Criterion (BIC) approximation to the Bayes factor, which affords easy computation using the same model specifications in lme4 used to evaluate support for the alternative hypothesis under the NHST procedure. A discussion of how to compute this approximation, as well as the strengths and weaknesses of this method are outlined in Wagenmakers (2007).

The Effect of Condition on the Distractor
-----------------------------------------

``` r
# by-subjects (evaluate alternative against null - H01)
cond_dist_subj_max <- lmer(cond_by_IA_formula, 
                           data = by_subj %>% filter(IA2 == "d"), 
                           REML = F
                           )
cond_dist_subj_min <- update(cond_dist_subj_max, . ~ . - condition)

# BF_01
cond_dist_bf_subj <- exp((BIC(cond_dist_subj_max) - BIC(cond_dist_subj_min))/2)

# by-items
cond_dist_items_max <- lmer(cond_by_IA_formula, 
                           data = by_items %>% filter(IA2 == "d"), 
                           REML = F
                           )

cond_dist_items_min <- update(cond_dist_items_max, . ~ . - condition)

# BF_01
cond_dist_bf_items <- exp((BIC(cond_dist_items_max) - BIC(cond_dist_items_min))/2)
```

The BIC approximation to the Bayes Factor shows that the the data are more likely under the null than the alternative hypothesis for both by-subjects and by-items analyses (by-subjects: *B**I**C*(*H*<sub>0</sub>) = -257.899 *B**I**C*(*H*<sub>1</sub>) = -253.42, *B**F*<sub>01</sub>≈ 9.388; by-items: *B**I**C*(*H*<sub>0</sub>) = -172.803 *B**I**C*(*H*<sub>1</sub>) = -169.231, *B**F*<sub>01</sub>≈ 5.967). This suggests that fixations on the distractor are not influenced by the discourse condition.

The Effect of Interest Area in the Apart condition
--------------------------------------------------

``` r
# by-subjects (evaluate alternative against null - H01)
ia_apart_subj_max <- lmer(IA_by_cond_formula, 
                          data = by_subj %>% filter(condition == "apart"), 
                          REML = F
                          )
ia_apart_subj_min <- update(ia_apart_subj_max, . ~ . - IA2)

# BF_01
ia_apart_bf_subj <- exp((BIC(ia_apart_subj_max) - BIC(ia_apart_subj_min))/2) 

# by-items
ia_apart_items_max <- lmer(IA_by_cond_formula, 
                           data = by_items %>% filter(condition == "apart"), 
                           REML = F
                           )
ia_apart_items_min <- update(ia_apart_items_max, . ~ . - IA2)

# BF_01
ia_apart_bf_items <- exp((BIC(ia_apart_items_max) - BIC(ia_apart_items_min))/2) 
```

The BIC approximation to the Bayes Factor shows that the the data are more likely under the null than the alternative hypothesis for both by-subjects and by-items analyses (by-subjects: *B**I**C*(*H*<sub>0</sub>) = -212.838 *B**I**C*(*H*<sub>1</sub>) = -208.436, *B**F*<sub>01</sub>≈ 9.035; by-items: *B**I**C*(*H*<sub>0</sub>) = -138.69 *B**I**C*(*H*<sub>1</sub>) = -134.7, *B**F*<sub>01</sub>≈ 7.352). This suggests that there is no difference in the fixations on the competitor and distractor in the Apart condition.

References
==========

Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). *Random effects structure for confirmatory hypothesis testing: Keep it maximal. Journal of Memory and Language, 68*(3), 255-278. <https://doi.org/10.1016/j.jml.2012.11.001>

Mirman, D. (2014). *Growth Curve Analysis and Visualization Using R*. Boca Ranton, FL.: Chapman and Hall/CRC Press.

Wagenmakers, E.-J. (2007). A practical solution to the pervasive problems of p values. *Psychonomic Bulletin & Review, 14*(5), 779-804. <https://doi.org/10.3758/BF03194105>
