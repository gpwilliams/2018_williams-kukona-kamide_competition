Additional Analyses (Semantic Competition)
================
Williams, G.P., Kukona, A., & Kamide, Y.

This document provides a reproducible analysis for the additional analyses for Experiment 1 (semantic competition) of the paper *Spatial narrative context modulates semantic (but not visual) competition during discourse processing* by Glenn P. Williams, Anuenue Kukona, & Yuki Kamide.

This document addresses two concerns raised by an anonymous reviewer about the effects described for the semantic competition experiment:

1.  Perhaps the modulation of semantic competition when the target and competitor are specified in the same (vs. different) narrative location(s) arises due to differences in the temporal proximity of mention for the target and competitor across the two conditions. For example, in "The piano and the trumpet are in the bar. The carrot and the lantern are in the gallery." the target (piano) and competitor (trumpet) are mentioned together in a short space of time. However, in "The carrot and the lantern are in the bar. The piano and the trumpet are in the gallery.", and other variations, there can be between 0 and 2 objects mentioned between the target and competitor.

2.  If the grouping of objects in the same narrative location is enough to modulate semantic representations for these objects (i.e. increasing similarity for them via a shared event/location) then the distractor should become a competitor for the target when it is in the same narrative location as the target (but not when it is separate the the target).

In an attempt to address these concerns, here we conduct further analyses to explore the effect of (1) temporal proximity on accessibility for the competitor, and (2) grouping of distractors with (or apart from) the target on accessibility for the distractor. We will establish whether accessibility for the competitor decreases as the temporal proximity from the target (by the number of objects mentioned between the target and competitor) increases. This analysis can only be conducted on the Apart condition as only this condition varies the temporal proximity of mention for the target and competitor. If the (transformed) proportion of fixations on the competitor does not differ as a measure of temporal proximity then this provides compelling evidence that this factor alone cannot drive the effects reported in our main analyses.

Additionally, we will address whether grouping of the target and distractor in the same (vs. separate) narrative location increases accessibility for the distractor. As above, this necessitates conducting analyses on the data for the Apart condition only as this condiiton provides the only instance in which the distractor can be in the same location or a different location to the target. This analysis addresses whether or not the grouping of semantically unrelated objects in the same narrative location makes these objects more semantically similar to one another, thus driving competition on access for the target. If the (transformed) proportion of fixations on the distractor does not differ depending upon the spatial narrative location of the distractor, this suggests that (i) semantic overlap is only affected for objects that initially share some overlap, or (ii) grouping by narrative location does not increase semantic overlap, but instead, separation makes objects less accessible through foregrounding/backgrounding of each object (i.e. compatible with an event model account).

Options, Packages, Functions, and Data
======================================

``` r
options(scipen = 1, digits = 3)
load(here("data", "semantic_data.Rdata"))
semantic_demo <- read.csv(here("data", "semantic_demographics.csv"))
semantic_item_checks <- read.csv(here("data", "semantic_item_checks.csv"))
```

Data Preparation
================

For the Apart condition we determined which distractors were grouped with the target across each item and list, and calculated the number of objects mentioned between the target and competitor.

``` r
subject_lists <- merge(
  filter(semantic_demo, included == "yes"), semantic_item_checks) %>%
  dplyr::select(c(
    subject,
    item,
    list,
    objects_between_mention,
    distractor_with_target
  )) %>%
  mutate(
    subject = as.factor(str_pad(subject, 2, pad = "0")),
    item = as.factor(item)
  )
```

As with our main analyses, we restrict our analyses here to the critical noun region + 300ms.

``` r
# define time window
t_window <- c("crit_noun_on", "crit_noun_off")

# shift window by how much?
window_shift <- 300
```

Proximity
=========

As described above, here we assessed the influence of temporal proximity of mention for the target and competitor on accessibility for the competitor; directly assessing whether proximity of mention between the target and competitor modulates accessibility for the competitor.

Data Preparation
----------------

As in our main analyses, we subsetted the data to the time window of the critical noun (e.g. "piano") + 300ms. Within this time window, we further subsetted our data to fixations on the competitor to explore temporal proximity effects on accessibility for the competitor.

``` r
# tidy data before analysis, restricting to the competitor only
prox_tidy_data <- semantic_data %>%
  mutate(
    item = as.factor(item),
    time_0 = time - UQ(as.name(t_window[1]))
  ) %>%
  dplyr::select(-c(t, d1, d2))

# subset data to time window (already in long format)
prox_sub_data <- subset_to_window(
  prox_tidy_data, 
  "time_0", 
  timeM, 
  t_window, 
  window_shift
) %>% 
  rename(fix = c)
```

Data Aggregation
----------------

As in our main analyses, we aggregated the data by subjects and items prior to conducting our analyses to account for the bias in fixation proportions towards extreme values (0 or 100% looking) within the subject by items data set (Mirman, 2014).

``` r
# join data sets, dropping together conditions
prox_long_data <- inner_join(
  prox_sub_data, 
  subject_lists, 
  by = c("subject", "item")
) %>%
  rename(prox = objects_between_mention) %>%
  mutate(prox = as.factor(prox))

# aggregate data by subjects, center variables, and make new dvs
prox_by_subj <- aggregate_data(prox_long_data, "subject", "prox", "fix")
prox_by_subj <- make_dvs(prox_by_subj, "y", "N")

# aggregate data by items, center variables, and make new dvs
prox_by_item <- aggregate_data(prox_long_data, "item", "prox", "fix")
prox_by_item <- make_dvs(prox_by_item, "y", "N")
```

Model Structure
---------------

We fitted an interaction model assessing the main effect of proximity of mention. As we only have one observation per condition for each subject, and fewer for each item (i.e. each item has only 2 condition levels of 0, 1, and 2 items mentioned between the target and competitor) the most complex model afforded by the data is that including random intercepts only by subjects/items. As such, our models took this form, using the maximal converging random effects structure given the data (Barr et al., 2013). Since the proximity factor has three levels, we first fit the full model to our data prior to conducting pairwise comparisons (with Bonferroni corrections) between each level of the factor using the `glht()` function from the `multcomp` package.

``` r
# main_model_formula
prox_main_formula <- as.formula("asin ~ prox + (1 | by)")

# interaction model: testing for main effects and interactions
prox_by_subj_model <- lmer(prox_main_formula, data = prox_by_subj)
prox_by_item_model <- lmer(prox_main_formula, data = prox_by_item)

# establish contrast matrix of planned comparisons
contrast_matrix <- rbind(
  "0 vs. 1" = c(0, 1, 0),
  "0 vs. 2" = c(0, 0, 1),
  "2 vs. 3" = c(0, -1, 1)
)

# fit contrasts and manually adjust p-values 
# for all tests (i.e. accounting for doing split by-subject/item analysis)
prox_by_subj_comparisons <- test_many_levels(prox_by_subj_model, contrast_matrix)
prox_by_item_comparisons <- test_many_levels(prox_by_item_model, contrast_matrix)
```

We found no significant difference between each level of proximity in both the by-subjects and by-items models.

``` r
merge_tables(prox_by_subj_comparisons, prox_by_item_comparisons) %>%
  pretty_confint(., "conf.low", "conf.high") %>%
  rename(p_value = p.value) %>%
  kable()
```

| aggregate | comparison |  estimate|  std.error|  statistic|  p\_value| ci                |
|:----------|:-----------|---------:|----------:|----------:|---------:|:------------------|
| Subjects  | 0 vs. 1    |     0.062|      0.038|      1.612|     0.321| \[-0.028; 0.152\] |
| Subjects  | 0 vs. 2    |     0.014|      0.038|      0.359|     1.000| \[-0.076; 0.104\] |
| Subjects  | 2 vs. 3    |    -0.048|      0.038|     -1.253|     0.631| \[-0.138; 0.042\] |
| Items     | 0 vs. 1    |     0.026|      0.042|      0.618|     1.000| \[-0.073; 0.126\] |
| Items     | 0 vs. 2    |    -0.002|      0.042|     -0.038|     1.000| \[-0.101; 0.098\] |
| Items     | 2 vs. 3    |    -0.028|      0.042|     -0.656|     1.000| \[-0.127; 0.072\] |

Descriptive Statistics
----------------------

Below, we show a table of the means, standard deviations, and confidence intervals for the proportion of fixations and arcsine square root transformed fixations on each interest area within each condition of proximity aggregated by subjects.

``` r
prox_by_subj %>%
  group_by(prox) %>%
  summarise(
    n = length(unique(by)),
    prop_mean = mean(prop),
    prop_sd = sd(prop),
    prop_ci = ci_of_mean(prop_mean, prop_sd, n),
    asin_mean = mean(asin),
    asin_sd = sd(asin),
    asin_ci = ci_of_mean(asin_mean, asin_sd, n)
    ) %>%
  select(-n) %>%
  rename(proximity = prox) %>%
  kable()
```

| proximity |  prop\_mean|  prop\_sd| prop\_ci         |  asin\_mean|  asin\_sd| asin\_ci         |
|:----------|-----------:|---------:|:-----------------|-----------:|---------:|:-----------------|
| 0         |       0.140|     0.131| \[0.106; 0.174\] |       0.319|     0.232| \[0.259; 0.379\] |
| 1         |       0.155|     0.096| \[0.130; 0.180\] |       0.381|     0.153| \[0.341; 0.420\] |
| 2         |       0.149|     0.143| \[0.112; 0.186\] |       0.333|     0.240| \[0.271; 0.395\] |

Evaluating Evidence in Support of the Null Hypothesis
-----------------------------------------------------

We further addressed the evidence in support of the null hypothesis for these comparisons using the BIC approximation to the Bayes factor (Wagenmakers, 2007). While the by-subjects data contains one observation of each condition for each subject, the by-items data contains between two and one observations. As a result, the models for the by-subjects data are fitted as a mixed effects model (as above), with random intercepts by subjects. However, the by-items models are instead fitted with a general linear model, given that no random intercept can be fitted by items. In each case, the BIC approximation to the Bayes factor is calculated in the same way as in our main analyses. The data is first subsetted to the levels of interest within each contrast, before a model is fitted with the fixed effect of proximity. Following this, a second model is fitted with only a fixed intercept. In the by-subjects analyses, both models contains the same random effects structure. In the by-items analyses, no random effect is modelled during this process.

``` r
prox_data_list <- list(
  subj_zero_vs_one = prox_by_subj %>% filter(prox != 2), 
  subj_zero_vs_two = prox_by_subj %>% filter(prox != 1),
  subj_one_vs_two = prox_by_subj %>% filter(prox != 0),
  item_zero_vs_one = prox_by_item %>% filter(prox != 2), 
  item_zero_vs_two = prox_by_item %>% filter(prox != 1),
  item_one_vs_two = prox_by_item %>% filter(prox != 0)
  )

prox_bf <- data.frame(
  model = names(prox_data_list), 
  BIC_H0 = vector("numeric", 6),
  BIC_H1 = vector("numeric", 6),
  BF_01 = vector("numeric", 6)
)

for (i in seq_along(names(prox_data_list))) {
  
  if(names(prox_data_list)[i] %>% stringr::str_detect("subj")) {
    maximal <- lmer(asin ~ prox + (1 | by), data = prox_data_list[[i]])
    reduced <- lmer(asin ~ 1 + (1 | by), data = prox_data_list[[i]])
  } else {
    maximal <- lm(asin ~ prox, data = prox_data_list[[i]])
    reduced <- lm(asin ~ 1, data = prox_data_list[[i]])
  }
  # save output
  prox_bf$BIC_H0[i] <- BIC(reduced)
  prox_bf$BIC_H1[i] <- BIC(maximal)
  prox_bf$BF_01[i] <- exp((BIC(maximal) - BIC(reduced)) / 2)
}
```

The results of these analyses, showing evidence in support of the null hypothesis for each comparison, is displayed in the table below.

``` r
prox_bf %>% 
  mutate(
    aggregate = c(rep("Subjects", 3), rep("Items", 3)),
    model = rep(c("Zero vs. One", "Zero vs. Two", "One vs. Two"), 2)
  ) %>%
  rename(
    group = model,
    "*BF~01~*" = BF_01,
    "*BIC*(H~0~)" = BIC_H0,
    "*BIC*(H~1~)" = BIC_H1
    ) %>%
  select(aggregate, everything()) %>%
  kable(escape = FALSE)
```

| aggregate | group        |  *BIC*(H<sub>0</sub>)|  *BIC*(H<sub>1</sub>)|  *BF<sub>01</sub>*|
|:----------|:-------------|---------------------:|---------------------:|------------------:|
| Subjects  | Zero vs. One |                 -28.1|                 -21.4|              27.96|
| Subjects  | Zero vs. Two |                  12.5|                  21.6|              96.48|
| Subjects  | One vs. Two  |                 -26.2|                 -18.5|              46.03|
| Items     | Zero vs. One |                 -54.8|                 -52.0|               4.08|
| Items     | Zero vs. Two |                 -32.5|                 -29.1|               5.65|
| Items     | One vs. Two  |                 -36.0|                 -32.9|               4.62|

Together, the Bayes factors for all comparisons show complling evidence in support of the null hypothesis (all *BF<sub>01</sub>* ⪆ 3). Thus, we can conclude that the proximity of mention between the target and competitor plays no role in the accessibility for the competitor. Thus, grouping by the spatial narrative location is still a likely candidate for the effects reported in our main analyses.

Distractor-Target Grouping
==========================

We explored the notion that distractors should attract more attention when mentioned in the same location as the target if spatial grouping alone causes competition. Specifically, we tested whether fixations on distractors differs depending upon whether they are in the same or a separate location to the target.

Data Preparation
----------------

As in our main analyses, we subsetted the data to the time window of the critical noun (e.g. "piano") + 300ms. Within this time window, we further subsetted our data to fixations on the two distractors to establish the grouping conditions for the distractor.

``` r
# reset interest area labels for later subsetting
target_group_ias <- c("d1", "d2")

# tidy data before analysis (make distractor looks the average across the two)
target_group_tidy_data <- semantic_data %>%
  mutate(
    item = as.factor(item),
    time_0 = time - UQ(as.name(t_window[1]))
  ) %>%
  dplyr::select(-t)

# subset data to time window
target_group_sub_data <- subset_to_window(
  target_group_tidy_data, 
  "time_0", 
  timeM, 
  t_window, 
  window_shift
)

target_group_long_data <- make_data_long(
  target_group_sub_data, 
  "IA2", 
  target_group_ias, 
  "fix"
) 

# merge with information about subject conditions 
# keep only apart conditions where these checks are appropriate
target_group_long_data <- inner_join(
  target_group_long_data,
  subject_lists,
  by = c("subject", "item")
) %>%
  rename(prox = objects_between_mention) %>%
  mutate(
    prox = as.factor(prox),
    grouping = ifelse(
      as.character(distractor_with_target) == as.character(IA2),
      "together",
      "apart"
      )
  )
```

Data Aggregation
----------------

We aggregated the data by subject and items separately as in all previous analyses. However here we included whether or not the a distractor was located in the same narrative location as the target (termed Grouping with Together (i.e. with the target) and Apart (i.e. separate from the target) as levels of this factor).

``` r
# aggregate data by subjects
dist_by_subj <- aggregate_data(
  target_group_long_data,
  "subject",
  "grouping",
  "fix"
)

# aggregate by items
dist_by_item <- aggregate_data(
  target_group_long_data,
  "item",
  "grouping",
  "fix"
)

# establish centering list
target_group_centering_list <- list(
  factors = c("grouping", "IA2"),
  levels = c("together", "d1")
)

# make new dvs
dist_by_subj <- make_dvs(dist_by_subj, "y", "N")
dist_by_item <- make_dvs(dist_by_item, "y", "N")
```

Model Structure
---------------

Here the by-subjects model included random intercepts by subject and by Grouping nested within subject. The by-items model could not accommodate this structure as each item only has one distractor in the same location as the target (while subjects see both the first-mentioned (d1) and second-mentioned (d2) distractor in each condition). As such, the by-items model contains only random intercepts by item.

``` r
# interaction model: testing for main effects and interactions
dist_by_subj_model <- tidy_model(
  lmer(asin ~ grouping + (1 | by), data = dist_by_subj)
)
dist_by_item_model <- tidy_model(
  lmer(asin ~ grouping + (1 | by), data = dist_by_item)
)
```

We found no significant differences in the proportion of fixations on a given distractor regardless of whether they were in the same or separate location to the target. Thus, having a distractor in the same location as the target does not make this object compete with the target.

``` r
merge_tables(dist_by_subj_model, dist_by_item_model) %>%
  pretty_confint(., "2.5 %", "97.5 %") %>%
  kable()
```

| aggregate | term             |  estimate|  std.error|  statistic|  p\_value| ci                |
|:----------|:-----------------|---------:|----------:|----------:|---------:|:------------------|
| Subjects  | (Intercept)      |     0.379|      0.016|     24.289|     0.000| \[0.349; 0.410\]  |
| Subjects  | groupingtogether |     0.017|      0.022|      0.748|     0.454| \[-0.027; 0.060\] |
| Items     | (Intercept)      |     0.382|      0.018|     21.022|     0.000| \[0.346; 0.417\]  |
| Items     | groupingtogether |     0.022|      0.026|      0.874|     0.382| \[-0.028; 0.073\] |

This pattern of results is reflected in the descriptive statistics for this analysis, below.

Descriptive Statistics
----------------------

``` r
dist_by_subj %>%
  group_by(grouping) %>%
  summarise(
    n = length(unique(by)),
    prop_mean = mean(prop),
    prop_sd = sd(prop),
    prop_ci = ci_of_mean(prop_mean, prop_sd, n),
    asin_mean = mean(asin),
    asin_sd = sd(asin),
    asin_ci = ci_of_mean(asin_mean, asin_sd, n)
  ) %>%
  select(-n) %>%
  kable()
```

| grouping |  prop\_mean|  prop\_sd| prop\_ci         |  asin\_mean|  asin\_sd| asin\_ci         |
|:---------|-----------:|---------:|:-----------------|-----------:|---------:|:-----------------|
| apart    |       0.146|     0.077| \[0.127; 0.166\] |       0.379|     0.114| \[0.350; 0.409\] |
| together |       0.160|     0.083| \[0.139; 0.182\] |       0.396|     0.127| \[0.363; 0.429\] |

Evaluating Evidence in Support of the Null Hypothesis
-----------------------------------------------------

We further addressed the evidence in support of the null hypothesis for these comparisons using the BIC approximation to the Bayes factor using a similar method to that described above. Here, both the by-subjects and by-item aggregated data contains an observation for each level of the grouping factor for each subject/item. As such, we fitted the data using mixed effects models with a fixed effect of grouping and random intercepts by subjects/items.

``` r
dist_bf <- data.frame(
  model = c("by_subj", "by_items"), 
  BF_01 = vector("numeric", 2)
)

# by-subjects
dist_maximal_subj <- lmer(asin ~ grouping + (1 | by), data = dist_by_subj)
dist_reduced_subj <- lmer(asin ~ 1 + (1 | by), data = dist_by_subj)
dist_bf$BF_01[1] <- exp((BIC(dist_maximal_subj) - BIC(dist_reduced_subj)) / 2)

# by-items
dist_maximal_items <- lmer(asin ~ grouping + (1 | by), data = dist_by_item)
dist_reduced_items <- lmer(asin ~ 1 + (1 | by), data = dist_by_item)
dist_bf$BF_01[2] <- exp((BIC(dist_maximal_items) - BIC(dist_reduced_items)) / 2)
```

The BIC approximation to the Bayes factor shows that the the data are more likely under the null than the alternative hypothesis for both by-subjects and by-items analyses (by-subjects: *BIC(H<sub>0</sub>)* = -146.347, *BIC(H<sub>1</sub>)* = -136.33, *BF<sub>01</sub>* ≈ 149.655; by-items: *BIC(H<sub>0</sub>)* = -91.54, *BIC(H<sub>1</sub>)* = -82.659, *BF<sub>01</sub>* ≈ 84.807). This suggests that there is no difference in the (transformed) proportion of fixations on the distractor regardless of whether it is in the same or a separate location to the target.

These findings suggest that the association of objects with the same narrative location of the target is not enough to drive competition, and instead suggests that competition is only modulated for semantic competitors. As a result, we can conclude that the narrative location either (i) modulates accessibility for information in the same/separate location as the target through foregrounding/backgrounding, making the competitor less accessible (and thus less likely to compete) when located in a different narrative location to the target, or (ii) only impacts accessibility for objects that already share some semantic overlap.

References
==========

Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). *Random effects structure for confirmatory hypothesis testing: Keep it maximal. Journal of Memory and Language, 68*(3), 255-278. <https://doi.org/10.1016/j.jml.2012.11.001>

Mirman, D. (2014). *Growth Curve Analysis and Visualization Using R*. Boca Ranton, FL.: Chapman and Hall/CRC Press.

Wagenmakers, E.-J. (2007). A practical solution to the pervasive problems of p values. *Psychonomic Bulletin & Review, 14*(5), 779-804. <https://doi.org/10.3758/BF03194105>

Session Information
===================

``` r
devtools::session_info()
```

    ## Session info -------------------------------------------------------------

    ##  setting  value                       
    ##  version  R version 3.5.1 (2018-07-02)
    ##  system   x86_64, mingw32             
    ##  ui       RStudio (1.1.414)           
    ##  language (EN)                        
    ##  collate  English_United Kingdom.1252 
    ##  tz       Europe/London               
    ##  date     2019-01-08

    ## Packages -----------------------------------------------------------------

    ##  package    * version  date       source        
    ##  assertthat   0.2.0    2017-04-11 CRAN (R 3.5.1)
    ##  backports    1.1.2    2017-12-13 CRAN (R 3.5.0)
    ##  base       * 3.5.1    2018-07-02 local         
    ##  bindr        0.1.1    2018-03-13 CRAN (R 3.5.1)
    ##  bindrcpp   * 0.2.2    2018-03-29 CRAN (R 3.5.1)
    ##  broom      * 0.5.0    2018-07-17 CRAN (R 3.5.1)
    ##  cli          1.0.0    2017-11-05 CRAN (R 3.5.1)
    ##  codetools    0.2-15   2016-10-05 CRAN (R 3.5.1)
    ##  colorspace   1.3-2    2016-12-14 CRAN (R 3.5.1)
    ##  compiler     3.5.1    2018-07-02 local         
    ##  crayon       1.3.4    2017-09-16 CRAN (R 3.5.1)
    ##  datasets   * 3.5.1    2018-07-02 local         
    ##  devtools     1.13.6   2018-06-27 CRAN (R 3.5.1)
    ##  digest       0.6.16   2018-08-22 CRAN (R 3.5.1)
    ##  dplyr      * 0.7.7    2018-10-16 CRAN (R 3.5.1)
    ##  evaluate     0.11     2018-07-17 CRAN (R 3.5.1)
    ##  fansi        0.3.0    2018-08-13 CRAN (R 3.5.1)
    ##  ggplot2    * 3.0.0    2018-07-03 CRAN (R 3.5.1)
    ##  glue         1.3.0    2018-07-17 CRAN (R 3.5.1)
    ##  graphics   * 3.5.1    2018-07-02 local         
    ##  grDevices  * 3.5.1    2018-07-02 local         
    ##  grid         3.5.1    2018-07-02 local         
    ##  gtable       0.2.0    2016-02-26 CRAN (R 3.5.1)
    ##  here       * 0.1      2017-05-28 CRAN (R 3.5.1)
    ##  highr        0.7      2018-06-09 CRAN (R 3.5.1)
    ##  htmltools    0.3.6    2017-04-28 CRAN (R 3.5.1)
    ##  knitr      * 1.20     2018-02-20 CRAN (R 3.5.1)
    ##  lattice      0.20-35  2017-03-25 CRAN (R 3.5.1)
    ##  lazyeval     0.2.1    2017-10-29 CRAN (R 3.5.1)
    ##  lme4       * 1.1-18-1 2018-08-17 CRAN (R 3.5.1)
    ##  magrittr     1.5      2014-11-22 CRAN (R 3.5.1)
    ##  MASS       * 7.3-50   2018-04-30 CRAN (R 3.5.1)
    ##  Matrix     * 1.2-14   2018-04-13 CRAN (R 3.5.1)
    ##  memoise      1.1.0    2017-04-21 CRAN (R 3.5.1)
    ##  methods    * 3.5.1    2018-07-02 local         
    ##  minqa        1.2.4    2014-10-09 CRAN (R 3.5.1)
    ##  multcomp   * 1.4-8    2017-11-08 CRAN (R 3.5.1)
    ##  munsell      0.5.0    2018-06-12 CRAN (R 3.5.1)
    ##  mvtnorm    * 1.0-8    2018-05-31 CRAN (R 3.5.0)
    ##  nlme         3.1-137  2018-04-07 CRAN (R 3.5.1)
    ##  nloptr       1.0.4    2017-08-22 CRAN (R 3.5.1)
    ##  pillar       1.3.0    2018-07-14 CRAN (R 3.5.1)
    ##  pkgconfig    2.0.2    2018-08-16 CRAN (R 3.5.1)
    ##  plyr         1.8.4    2016-06-08 CRAN (R 3.5.1)
    ##  purrr        0.2.5    2018-05-29 CRAN (R 3.5.1)
    ##  R6           2.2.2    2017-06-17 CRAN (R 3.5.1)
    ##  Rcpp         0.12.18  2018-07-23 CRAN (R 3.5.1)
    ##  rlang        0.3.0    2018-10-22 CRAN (R 3.5.1)
    ##  rmarkdown  * 1.10     2018-06-11 CRAN (R 3.5.1)
    ##  rprojroot    1.3-2    2018-01-03 CRAN (R 3.5.1)
    ##  rstudioapi   0.7      2017-09-07 CRAN (R 3.5.1)
    ##  sandwich     2.5-0    2018-08-17 CRAN (R 3.5.1)
    ##  scales       1.0.0    2018-08-09 CRAN (R 3.5.1)
    ##  splines      3.5.1    2018-07-02 local         
    ##  stats      * 3.5.1    2018-07-02 local         
    ##  stringi      1.1.7    2018-03-12 CRAN (R 3.5.0)
    ##  stringr    * 1.3.1    2018-05-10 CRAN (R 3.5.1)
    ##  survival   * 2.42-3   2018-04-16 CRAN (R 3.5.1)
    ##  TH.data    * 1.0-9    2018-07-10 CRAN (R 3.5.1)
    ##  tibble       1.4.2    2018-01-22 CRAN (R 3.5.1)
    ##  tidyr      * 0.8.1    2018-05-18 CRAN (R 3.5.1)
    ##  tidyselect   0.2.4    2018-02-26 CRAN (R 3.5.1)
    ##  tools        3.5.1    2018-07-02 local         
    ##  utf8         1.1.4    2018-05-24 CRAN (R 3.5.1)
    ##  utils      * 3.5.1    2018-07-02 local         
    ##  withr        2.1.2    2018-03-15 CRAN (R 3.5.1)
    ##  yaml         2.2.0    2018-07-25 CRAN (R 3.5.1)
    ##  zoo          1.8-3    2018-07-16 CRAN (R 3.5.1)
