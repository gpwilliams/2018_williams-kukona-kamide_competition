###############################################################################
# Competition Paper Analyses - User Defined Functions
#   Glenn Williams - 2018/07/17
###############################################################################

round_pad <- function(num, digits = 3) {
  # Rounds any number to specified digits but forces padding of zeroes
  # Defaults to 3 digits.
  # e.g. 0.0101 with round(0.0101, digits = 3) = 0.01
  # while round_three(0.0101, digits = 3) = 0.010
  # used for matching of CI in tables.
  # Inputs: num = numeric variable to round
  # Outputs: character vector of numeric variables, rounded to
  #           specified digits (defaults to 3).
  formatC(round(num, digits = 3), format = "f", digits = 3)
}

round_all_three <- function(data, digits = 3) {
  # Round any numeric columns in a data frame to 3 decimal places,
  #   and forces padding of zeroes.
  # Relies upon the round_pad() function.
  # Inputs: data = data.frame object containing numeric columns.
  # Ouputs: data = data.frame object with numeric columns rounded to 3 dp.
  data %>% mutate_if(is.numeric, round_pad, digits = digits)
}

subset_to_window <- function(data, time_col, times, time_window, time_shift) {
  # Subsets data to the beginning and end of a time window.
  # Inputs: data = data.frame from which to subset data
  #         time_col = column identifying time from the beginning of the trial
  #         times = named numbers of mean times for each window
  #         time_window = names of window boundaries by which to subset
  #         time_shift = value (in ms) by which to shift the window
  #             e.g. 300ms after the onset/offset = 300
  # Returns: data.frame subsetted to only the provided time-window
  data %>% filter(UQ(as.name(time_col)) >= time_shift & 
                    UQ(as.name(time_col)) < (diff(times[time_window]) + 
                                               time_shift
                    )
  )
}

make_data_long <- function(data, ia_col, ia_list, dv_col) {
  # Gathers data into a long format.
  # Inputs: data = data.frame from which to gather
  #         ia_col = column to produce which labels interest areas
  #         ia_list = character containing the interest area column names
  #         dv_col = column which indicates a fixation (or not)
  # Outputs: data.frame gathered by the provided parameters.
  data %>%
    gather(key = UQ(as.name(ia_col)), 
           value = UQ(as.name(dv_col)), 
           UQ(ia_list)
    )
}

aggregate_data <- function(data, by_id, conditions, dv_col) {
  # Aggregates data by subjects or items.
  # Inputs: data = data.frame by which to aggregate
  #         by_id = name of column for the group by which to aggregate
  #         conditions = conditions by which to calcualte DV
  #         dv_col = name of column containing the dependent variable
  # Outputs: data.frame aggregated by the provided parameters.
  groups <- c(by_id, conditions)
  data %>%
    group_by_at(groups) %>%
    summarise(y = sum(UQ(as.name(dv_col))),
              N = length(UQ(as.name(dv_col)))
    ) %>%
    rename_(by = names(.)[1]) %>%
    as.data.frame() %>%
    mutate_if(is.character, as.factor)
}

# centre variables
centre_var <- function(data, factor, level_one) {
  # Centres two level factors
  # Inputs: data = data.frame containing the relevant factor
  #         factor = column name (in quotes) for the factor to be centred
  #                   e.g. "task"
  #         level_one = the level to receive the positive coding
  #                       (in quotes), e.g. "R" (for reading)
  # Outputs: data.frame with the provided factor centred by the provided level.
  (data[[factor]] == level_one) - mean(data[[factor]] == level_one, na.rm = T)
}

centre_many <- function(data, list) {
  # Centres data using the centre_var function according to a list of IDs
  # Inputs: data = data.frame containing the factors to be centred
  #           list = list containing two levels
  #             the first containing the factor IDs
  #             the second containing the level IDs
  #             e.g. list(factors = c("cond_one", "cond_two"),
  #                        levels = c("level_one", "level_two"))
  # Outputs: data.frame with appended centred factors.
  for(i in seq(list[[1]])) {
    data[[paste0(list[[1]][i], "_c")]] <- 
      centre_var(data, list[[1]][i], list[[2]][i])
  }
  return(data)
}

make_dvs <- function(data, successes, bins) {
  # Makes proportions and arsin square root transformed proportions
  # Inputs: data from which proportions should be calculated
  #         successes = column containing a track of successes (e.g. fixation)
  #         bins = column of bins from which successes are evaluated
  #           both successes and bins need to be quotes text strings
  # Outputs: data.frame with DV columns 
  #   (proportions and arcsin square root transformed proportions)
  data %>%
    mutate(prop = UQ(as.name(successes))/UQ(as.name(bins)),
           asin = asin(sqrt(UQ(as.name(successes))/
                              UQ(as.name(bins))))
    )
}

make_confint <- function(merMod, confint_method = "Wald") {
  merMod %>%
    confint(., method = confint_method) %>%
    as.data.frame() %>%
    na.omit() %>%
    tibble::rownames_to_column(., "term") %>%
    mutate_if(is.numeric, funs(round(., 3)))
}

tidy_model <- function(merMod, confint = TRUE, ...) {
  # Inputs: merMod = a saved model from lme4
  # Outputs: data.frame of tidied fixed effects and p-values
  #           Note: p-values calculated via the normal approximation
  if (confint == TRUE) {
    confints <- make_confint(merMod, ...)
  }
  tidied_model <- merMod %>% 
    broom::tidy("fixed") %>% 
    mutate(p_value = 2*(1 - pnorm(abs(statistic)))) %>%
    mutate_if(is.numeric, funs(round(., 3)))
  if (confint == TRUE) {
    merge(tidied_model, confints) 
  } else {
    tidied_model
  }
}

test_interactions <- function(
  data, 
  group, 
  formula, 
  confint = TRUE, 
  ...
) {
  # Runs lme on a factor within groups (for interactions)
  # Inputs: data = data.frame from which comparisons will be made
  #         group = group to split by
  #         formula = string outlining fixed and random effects
  #                     to test for a remaining factor
  #                   e.g. "asin ~ factor_one + (1 | subjects)"
  # Outputs: 
  #   data.frame containing tidied model outputs for pairwise comparisons.
  group_var <- enquo(group)
  data %>% 
    group_by(!!group_var) %>%
    do(
      results = tidy_model(
        lmer(formula, data = .), 
        confint, 
        ...
      )
    ) %>%
    unnest()
}

test_many_levels <- function(merMod, contrast_matrix, adjusted = "bonferroni") {
  # extract pairwise comparisons (difference score estimates)
  model_summary <- summary(
    multcomp::glht(merMod, contrast_matrix),
    test = adjusted(adjusted)
  ) %>% 
    tidy()
  
  # estimate confidence intervals
  intervals <- confint(multcomp::glht(merMod, contrast_matrix)) %>% 
    tidy()
  
  # merge together and tidy columns
  merge(model_summary, intervals) %>%
    rename(comparison = lhs) %>%
    select(-rhs)
}

merge_tables <- function(subjects_table, item_table) {
  # Merges two tables of tidied model outputs
  #  and creates an ID column (note, order is fixed)
  # Inputs: subjects_table = data.frame of tidied by_subjects model
  #        item_table = data.frame of tidied by_items model
  # Outputs: combined table with ID column for tests
  output_table <- bind_rows(subjects_table, item_table) 
  output_table %>%
    mutate(aggregate = rep(c("Subjects", "Items"), 
                           each = nrow(output_table)/2)
    ) %>%
    dplyr::select(aggregate, everything())
}

adjust_multiple <- function(data, n_comparisons) {
  # Adjusts p-values for multiple comparisons, returns to 3dp.
  # Inputs: data = the data frame containing statistics to adjust
  #           data must contain a column called p_value to adjust
  #         n_comparisons = number of comparisons for which to adjust
  # Outputs: data.frame with p_value column of adjusted p-values to 3dp
  format(
    p.adjust(
      data[["p_value"]],
      method = "bonferroni",
      n = n_comparisons
    ), 
    nsmall = 3
  )
}

ci_of_mean <- function(mean, sd, n, rounding = 3) {
  # Calculates the 95% confidence interval around the mean
  # Inputs: mean = numeric variable of the mean
  #         sd = standard deviation around the mean
  #         n = number of observations that make up the mean
  #         rounding = decimals by which to round the output (defaults to 3)
  # Outputs: character vector containing both lower and upper bounds of the 
  #           confidence interval around the mean in square brackets.
  paste0(
    "[", 
    round_pad(mean - qt(0.975, df = n-1)*sd/sqrt(n), rounding), 
    "; ", 
    round_pad(mean + qt(0.975, df = n-1)*sd/sqrt(n), rounding),
    "]"
  )
}

pretty_confint <- function(data, lower, upper, colname = "ci", rounding = 3) {
  # Creates a character column of two numeric columns inside square brackets.
  # Used for merging confidence intervals together from two columns
  # Inputs: data = data.frame containing two numeric columns, lower and upper.
  #         lower = character string of the lower CI value column
  #         upper = character string of the upper CI value column
  #         colname = name of column following merge (defaults to "ci")
  #         rounding = rounding to apply to column values (defaults to 3)
  output <- data
  
  output[[colname]] <- 
    paste0(
      "[", 
      round_pad(data[[lower]], rounding),
      "; ", 
      round_pad(data[[upper]], rounding),
      "]"
    )
  output[[lower]] <- NULL
  output[[upper]] <- NULL
  output
}