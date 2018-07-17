###############################################################################
# Competition Paper Analyses - User Defined Functions
#   Glenn Williams - 2018/07/17
###############################################################################

round_three <- function(data) {
  # Round any numeric columns in a data frame to 2 decimal places.
  # Inputs: data = data.frame object containing numeric columns.
  # Ouputs: data = data.frame object with numeric columns rounded to 3 dp.
  data %>% mutate_if(is.numeric, round, digits = 3)
}

bind_pairwise <- function(data) {
  # Saves a list of data frame outputs from multiple test statistics
  #   and binds them together as one output with a group ID.
  # Input: data = list containing IDs by which pairwise stats are calculated
  #                 and a data frame of results consisting of test
  #                 statistics for each comparison.
  # Output: data frame containing binded results with group ID col.
  output <- NULL
  for (ii in 1: nrow(data[1])) {
    # extract results for each pairwise comparison
    group_dat <- as.data.frame(data[["results"]][ii]) %>%
      filter(group == "fixed") %>% 
      dplyr::select(-group)
    
    # check for groups, add as group ID col, or warn if too many
    if (ncol(data) == 2) {
      group_dat$group <- data[[1]][ii]
    } else if (ncol(data) == 3) {
      group_dat$group <- paste(data[[1]][ii], "and", data[[2]][ii])
    } else if (ncol(data) > 3) {
      print("warning, too many groups selected")
    }
    
    # bind results together, print with group as first col
    output <- rbind(output, group_dat)
    output <- output %>% select(group, 1:(ncol(output)))
  }
  round_three(output) # round to 3dp
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

tidy_model <- function(merMod) {
  # Inputs: merMod = a saved model from lme4
  # Outputs: data.frame of tidied fixed effects and p-values
  #           Note: p-values calculated via the normal approximation
  merMod %>% 
    tidy("fixed") %>% 
    mutate(p_value = 2*(1 - pnorm(abs(statistic)))) %>%
    mutate_if(is.numeric, funs(round(., 3)))
}

test_interactions <- function(data, group, formula) {
  # Runs lme on a factor within groups (for interactions)
  # Inputs: data = data.frame from which comparisons will be made
  #         group = group to split by
  #         formula = string outlining fixed and random effects
  #                     to test for a remaining factor
  #                   e.g. "asin ~ factor_one + (1 | subjects)"
  # Outputs: 
  #   data.frame containing tidied model outputs for pairwise comparisons.
  group_var <- enquo(group)
  output <- data %>% 
    group_by(!!group_var) %>%
    do(results = tidy(lmer(formula, data = .)))
  # store output, calculate p-values, round to 3dp
  bind_pairwise(output) %>%
    mutate(p_value = 2*(1-pnorm(abs(statistic)))) %>%
    mutate_if(is.numeric, funs(round(., 3)))
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
    select(aggregate, everything())
}

adjust_multiple <- function(data, n_comparisons) {
  # Adjusts p-values for multiple comparisons, returns to 3dp.
  # Inputs: data = the data frame containing statistics to adjust
  #           data must contain a column called p_value to adjust
  #        n_comparisons = number of comparisons for which to adjust
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