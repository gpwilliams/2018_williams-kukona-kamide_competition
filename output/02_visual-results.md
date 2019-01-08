---
title: "Experiment 2 (Visual Competition)"
author: Williams, G.P., Kukona, A., & Kamide, Y.
output:
  html_document: 
    toc: true
    toc_float: true
    keep_md: true
---

This document provides a reproducible analysis for Experiment 2 of the paper *Spatial narrative context modulates semantic (but not visual) competition during discourse processing* by Glenn P. Williams, Anuenue Kukona, & Yuki Kamide. 

# Options, Packages, Functions, and Data


```r
options(scipen = 1, digits = 3)
load(here("data", "visual_data.Rdata"))
visual_demo <- read.csv(here("data", "visual_demographics.csv"))
```

# Sample Demographics


```r
# gender count
visual_gender_n <- visual_demo %>%
  filter(included == "yes") %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  ungroup()

# age averages
visual_ages <- visual_demo %>%
  filter(included == "yes") %>%
  summarise(
    min_age = min(age),
    max_age = max(age),
    mean_age = mean(age),
    sd_age = sd(age)
  )
```

60 (16 male) native speakers of English from the University of Dundee community (aged 18- 42, *M* = 19.85, *SD* = 3.507) took part in this study for partial course credit. All participants had uncorrected vision, wore soft contact lenses, or wore spectacles, and had no known auditory, visual, or language disorders.

# Define Variables for Both Analyses


```r
# define time window
t_window <- c("crit_noun_on", "crit_noun_off")

# reset interest area labels for later subsetting
ias <- c("c", "d")

# establish conditions for later summaries
conds <- c("condition", "IA2")

# define variables for centering
centering_list <- list(
  factors = c("condition", "IA2"),
  levels = c("together", "c")
)

# define interaction model formula for frequentist and Bayesian analyses
full_formula <- as.formula(
  "asin ~ condition_c * IA2_c + 
  (1 | by) + (1 | by: condition_c) + (1 | by: IA2_c)"
)
```

# Noun + 200ms

## Data Preparation

We subsetted the data to the time window of the critical noun (e.g. "bat") + 200ms. Within this time window, we further subsetted our data to fixations on the competitor and the average fixations on the two distractors (henceforth, distractor).


```r
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

## Data Aggregation

Prior to running our analyses, we aggregated the data first by subjects, and then by items, across the entire time window. This aggregation was conducted in order to account for a heavy bias in fixation probabilities within trials for each subject, whereby within subjects and items several trials could consist of 0% or 100% looking towards one interest area. Additionally, models fitted to the subject-by-item data violated the assumption of homoscedasticity for residuals.

Additionally, we transformed our dependent variable, the proportion of fixations on a given interest area, into arcsin square root transformed proportions. This transformation attempts to account for the bounded nature of the underlying binomial response data that makes up a proportion. This transformation was used to account for the lack of homogeneity in the variance across the range of possible outcomes, with a larger variance towards the endpoints (i.e. 0 or 1) (Mirman, 2014). In effect, the arcsin square root transformation pulls out the distribution of proportions around the tails of the data. 


```r
# aggregate data by subjects, centre variables, and make new dvs
by_subj <- aggregate_data(long_data, "subject", conds, "fix")
by_subj <- centre_many(by_subj, centering_list)
by_subj <- make_dvs(by_subj, "y", "N")

# aggregate data by items, centre variables, and make new dvs
by_items <- aggregate_data(long_data, "item", conds, "fix")
by_items <- centre_many(by_items, centering_list)
by_items <- make_dvs(by_items, "y", "N")
```

## Model Structure

We conducted one main analysis which looked at the main effects and interactions between the condition and interest area variables. To perform this analysis, we used a hierarchical mixed-effects model with an arcsin square root transformation. Our models contained fixed effects of condition and interest area (centred) and their interaction. Additionally, our random effects structure took the form of random intercepts by subjects/items, and random intercepts of subjects/items nested within condition, and subjects/items nested within interest area.

In all models, we used the maximal converging random effects structure (Barr et al., 2013).


```r
# by subjects interaction model: testing for main effects and interactions
by_subj_model <- tidy_model(lmer(full_formula, data = by_subj))

# by items interaction model
by_items_model <- tidy_model(lmer(full_formula, data = by_items))
```

## Descriptive Statistics

Below, we show a table of the means, standard deviations, and confidence intervals for the proportion of fixations and arcsin squareroot transformed fixations on each interest area within each condition aggregated by subjects.


```r
# generate descriptives
descriptives_table <- by_subj %>%
  group_by(IA2, condition) %>%
  summarise(
    n = length(unique(by)),
    prop_mean = mean(prop),
    prop_sd = sd(prop),
    prop_ci = ci_of_mean(prop_mean, prop_sd, n),
    asin_mean = mean(asin),
    asin_sd = sd(asin),
    asin_ci = ci_of_mean(asin_mean, asin_sd, n)
    ) %>%
  select(-n)

# print output
kable(descriptives_table)
```



IA2   condition    prop_mean   prop_sd  prop_ci           asin_mean   asin_sd  asin_ci        
----  ----------  ----------  --------  ---------------  ----------  --------  ---------------
c     apart            0.167     0.099  [0.142; 0.193]        0.404     0.138  [0.368; 0.440] 
c     together         0.164     0.071  [0.145; 0.182]        0.408     0.098  [0.382; 0.433] 
d     apart            0.157     0.069  [0.140; 0.175]        0.399     0.098  [0.374; 0.424] 
d     together         0.159     0.062  [0.143; 0.175]        0.403     0.089  [0.380; 0.426] 

## Interaction Model

For both by-subjects and by-items analyses, we found no significant differences between the levels of either factor and their interaction.


```r
merge_tables(by_subj_model, by_items_model) %>% 
  pretty_confint(., "2.5 %", "97.5 %") %>%
  kable()
```



aggregate   term                 estimate   std.error   statistic   p_value  ci              
----------  ------------------  ---------  ----------  ----------  --------  ----------------
Subjects    (Intercept)             0.403       0.010      40.321     0.000  [0.384; 0.423]  
Subjects    condition_c             0.004       0.010       0.353     0.724  [-0.017; 0.024] 
Subjects    condition_c:IA2_c       0.000       0.021      -0.003     0.997  [-0.041; 0.041] 
Subjects    IA2_c                   0.005       0.013       0.417     0.677  [-0.019; 0.030] 
Items       (Intercept)             0.409       0.008      54.204     0.000  [0.394; 0.424]  
Items       condition_c             0.000       0.011      -0.012     0.991  [-0.022; 0.021] 
Items       condition_c:IA2_c      -0.006       0.022      -0.291     0.771  [-0.049; 0.036] 
Items       IA2_c                   0.008       0.015       0.505     0.613  [-0.022; 0.037] 

## Evaluating Evidence for the Null Hypothesis

The effect of condition had a non-significant effect of fixations to the distractor across conditions. Moreover, the effect of interest area had a non-significant effect on fixations in the Apart condition. However, we cannot take this as evidence of the absence of any effect in these cases. One way to evaluate support for the null hypothesis is through using Bayes Factors. Here, we use the Bayesian Information Criterion (BIC) approximation to the Bayes factor, which affords easy computation using the same model specifications in lme4 used to evaluate support for the alternative hypothesis under the NHST procedure. A discussion of how to compute this approximation, as well as the strengths and weaknesses of this method are outlined in Wagenmakers (2007).


```r
# by-subjects: evaluating each effect in isolation
all_subj_max <- lmer(full_formula, data = by_subj, REML = F)
all_subj_cond <- update(all_subj_max, . ~ . - condition_c)
all_subj_IA <- update(all_subj_max, . ~ . - IA2_c)
all_subj_int <- update(all_subj_max, . ~ . - condition_c : IA2_c)

# by_subjects: BFs against model H1 (full model)
by_subj_bf_vs_H1 <-
  data.frame(
    Aggregate = rep("Subjects", 4),
    Model = c(
      "H1: full model",
      "H2: no condition",
      "H3: no interest area",
      "H4: no interaction"
    ),
    BIC = c(
      BIC(all_subj_max),
      BIC(all_subj_cond),
      BIC(all_subj_IA),
      BIC(all_subj_int)
    ),
    BF_comparison = c("-", "BF_21", "BF_31", "BF_41"),
    BF = c(
      NA,
      exp((BIC(all_subj_max) - BIC(all_subj_cond)) / 2),
      exp((BIC(all_subj_max) - BIC(all_subj_IA)) / 2),
      exp((BIC(all_subj_max) - BIC(all_subj_int)) / 2)
    )
  )

# by-items: evaluating each effect in isolation
all_items_max <- lmer(full_formula, data = by_items, REML = F)
all_items_cond <- update(all_items_max, . ~ . - condition_c)
all_items_IA <- update(all_items_max, . ~ . - IA2_c)
all_items_int <- update(all_items_max, . ~ . - condition_c:IA2_c)

# by-items: BFs against model H1 (full model)
by_items_bf_vs_H1 <-
  data.frame(
    Aggregate = rep("Items", 4),
    Model = c(
      "H1: full model",
      "H2: no condition",
      "H3: no interest area",
      "H4: no interaction"
    ),
    BIC = c(
      BIC(all_items_max),
      BIC(all_items_cond),
      BIC(all_items_IA),
      BIC(all_items_int)
    ),
    BF_comparison = c("-", "BF_21", "BF_31", "BF_41"),
    BF = c(
      NA,
      exp((BIC(all_items_max) - BIC(all_items_cond)) / 2),
      exp((BIC(all_items_max) - BIC(all_items_IA)) / 2),
      exp((BIC(all_items_max) - BIC(all_items_int)) / 2)
    )
  )
```

The BIC for all models is displayed in the table below, along with the BIC approximation to the Bayes factor for all models in relation to the maximal model used to fit the data ($H1$; i.e. including main effects and interactions by both factors). The approximation to the Bayes factors compare evidence against the maximal model in relation to models containing all other factors except the one of interest; specifically, these are models without the main effect of condition ($H2$), without the main effect of interest area ($H3$), and without the interaction between interest area and condition ($H4$). 


```r
rbind(by_subj_bf_vs_H1, by_items_bf_vs_H1) %>%
  rename(
    "BF comparison" = BF_comparison,
    "approximate BF" = BF
  ) %>%
  kable()
```



Aggregate   Model                    BIC  BF comparison    approximate BF
----------  ---------------------  -----  --------------  ---------------
Subjects    H1: full model          -388  -                            NA
Subjects    H2: no condition        -393  BF_21                     14.54
Subjects    H3: no interest area    -393  BF_31                     14.19
Subjects    H4: no interaction      -393  BF_41                     15.49
Items       H1: full model          -273  -                            NA
Items       H2: no condition        -278  BF_21                     11.31
Items       H3: no interest area    -278  BF_31                      9.92
Items       H4: no interaction      -278  BF_41                     10.83

In sum, the maximal model was dispreferred in all cases, suggesting that these terms play no role in guiding fixations during the time window of the noun + 200ms.

# Noun + 400ms

## Data Preparation

We conducted the same analysis as used in the region of the noun + 200ms for the region of the noun + 400ms. This time window was selected as previous research has shown that, given long preview times (e.g. 1000ms) of the visual scene prior to noun onset, visual competition occurs approximately 100ms later than semantic competition. As our semantic competition experiment showed effects during the region of the noun + 300ms, this later region of the noun + 400ms was deemed appropriate for analysis.


```r
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

## Data Aggregation


```r
# aggregate data by subjects, centre variables and make new dvs
by_subj <- aggregate_data(long_data, "subject", conds, "fix")
by_subj <- centre_many(by_subj, centering_list)
by_subj <- make_dvs(by_subj, "y", "N")

# aggregate data by items, centre variables and make new dvs
by_items <- aggregate_data(long_data, "item", conds, "fix")
by_items <- centre_many(by_items, centering_list)
by_items <- make_dvs(by_items, "y", "N")
```

## Model Structure


```r
# by-subjects interaction model: testing for main effects and interactions
by_subj_model <- tidy_model(lmer(full_formula, data = by_subj))

# by-items interaction model: testing for main effects and interactions
by_items_model <- tidy_model(lmer(full_formula, data = by_items))
```

## Descriptive Statistics

Below, we show a table of the means, standard deviations, and confidence intervals for the proportion of fixations and arcsin squareroot transformed fixations on each interest area within each condition aggregated by subjects.


```r
descriptives_table <- by_subj %>%
  group_by(IA2, condition) %>%
  summarise(
    n = length(unique(by)),
    prop_mean = mean(prop),
    prop_sd = sd(prop),
    prop_ci = ci_of_mean(prop_mean, prop_sd, n),
    asin_mean = mean(asin),
    asin_sd = sd(asin),
    asin_ci = ci_of_mean(asin_mean, asin_sd, n)
    ) %>%
  select(-n)

# print output
kable(descriptives_table)
```



IA2   condition    prop_mean   prop_sd  prop_ci           asin_mean   asin_sd  asin_ci        
----  ----------  ----------  --------  ---------------  ----------  --------  ---------------
c     apart            0.145     0.093  [0.121; 0.169]        0.372     0.136  [0.337; 0.407] 
c     together         0.145     0.077  [0.125; 0.165]        0.375     0.120  [0.344; 0.406] 
d     apart            0.118     0.067  [0.101; 0.136]        0.338     0.105  [0.311; 0.365] 
d     together         0.117     0.061  [0.101; 0.133]        0.337     0.100  [0.312; 0.363] 

## Interaction Model

For both by-subjects and by-items analyses, we found a significant main effect of interest area across both conditions, with a larger proportion of transformed fixations on the competitor than the distractor. However, we found no significant main effect of, or interaction with, condition.


```r
merge_tables(by_subj_model, by_items_model) %>%
  pretty_confint(., "2.5 %", "97.5 %") %>%
  kable()
```



aggregate   term                 estimate   std.error   statistic   p_value  ci              
----------  ------------------  ---------  ----------  ----------  --------  ----------------
Subjects    (Intercept)             0.356       0.011      32.804     0.000  [0.334; 0.377]  
Subjects    condition_c             0.001       0.011       0.107     0.915  [-0.020; 0.022] 
Subjects    condition_c:IA2_c       0.004       0.022       0.182     0.856  [-0.039; 0.047] 
Subjects    IA2_c                   0.036       0.014       2.593     0.010  [0.009; 0.063]  
Items       (Intercept)             0.364       0.009      42.106     0.000  [0.347; 0.381]  
Items       condition_c            -0.002       0.011      -0.170     0.865  [-0.024; 0.020] 
Items       condition_c:IA2_c       0.002       0.022       0.084     0.933  [-0.042; 0.045] 
Items       IA2_c                   0.041       0.017       2.363     0.018  [0.007; 0.075]  

## Evaluating Evidence for the Null Hypothesis


```r
# by-subjects: evidence of interaction (BF_12)
max_subj <- lmer(full_formula, data = by_subj, REML = F) # H2
main_subj <- update(max_subj, . ~ . - condition_c: IA2_c) # H1
bf_against_int_subj <- exp((BIC(max_subj) - BIC(main_subj))/2) # BF_12

# by-items: evidence of interaction (BF_12)
max_items <- lmer(full_formula, data = by_items, REML = F) # H2
main_items <- update(max_items, . ~ . - condition_c: IA2_c) # H1
bf_against_int_items <- exp((BIC(max_items) - BIC(main_items))/2) # BF_12
```

The BIC approximation to the Bayes Factor shows that the the data are more likely under the model of main effects with no interaction than the model including the interaction term for both by-subjects and by-items analyses (by-subjects: *BIC(H~1~)* = -360.001, *BIC(H~2~)* = -354.554, *BF~12~* $\approx$ 15.234; by-items: *BIC(H~1~)* = -258.729, *BIC(H~2~)* = -253.884, *BF~12~* $\approx$ 11.273). This suggests that the discourse condition plays no role in modulating visual competition during the critical noun region + 400ms.

# References

Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). *Random effects structure for confirmatory hypothesis testing: Keep it maximal. Journal of Memory and Language, 68*(3), 255-278. https://doi.org/10.1016/j.jml.2012.11.001

Mirman, D. (2014). *Growth Curve Analysis and Visualization Using R*. Boca Ranton, FL.: Chapman and Hall/CRC Press.

Wagenmakers, E.-J. (2007). A practical solution to the pervasive problems of p values. *Psychonomic Bulletin & Review, 14*(5), 779-804. https://doi.org/10.3758/BF03194105

# Session Information


```r
devtools::session_info()
```

```
## Session info -------------------------------------------------------------
```

```
##  setting  value                       
##  version  R version 3.5.1 (2018-07-02)
##  system   x86_64, mingw32             
##  ui       RStudio (1.1.414)           
##  language (EN)                        
##  collate  English_United Kingdom.1252 
##  tz       Europe/London               
##  date     2019-01-08
```

```
## Packages -----------------------------------------------------------------
```

```
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
```
