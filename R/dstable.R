#' Calculate descriptive statistics for a data frame
#'
#' This function takes a data frame and specifications of grouping and stats as input and
#' outputs a list which have two fixed components: label and depth, and several custom components which
#' are stores the stats specified in stats_settings.
#'
#' @param data Data frame that contains the data 
#' @param group_by_cols Character object that holds the column names to be grouped
#' @param value_col Column name of the numeric data where the stats will be calculated on
#' @param stats_settings List of stats settings
#' @return A list that contains the stats result requested with labels and depths for each group labels
#' @export
get_descriptive_stats <- function(data, group_by_cols, value_col, stats_settings)
  descriptive_stats_rec(data, list(), group_by_cols, value_col, stats_settings, 0)

descriptive_stats_rec <- function(data, stats, group_by_cols, value_col, stats_settings, depth) {
  group_col <- group_by_cols[1]

  if (length(group_by_cols) == 1) {
    return(calc_stats(data, group_col, value_col, stats_settings, depth))
  }
  
  for (group in levels(factor(data[[group_col]]))) {
    stats <- concat_list(
      stats, 
      calc_stats(
        data[data[[group_col]] == group,], 
        group_col, 
        value_col, 
        stats_settings, 
        depth
      )
    )

    stats <- concat_list(
      stats, 
      descriptive_stats_rec(
        data[data[[group_col]] == group,],
        list(), 
        group_by_cols[-1], 
        value_col, 
        stats_settings, 
        depth + 1
      )
    )
  }
  return(stats)
}

calc_stats <- function(data, label_col, value_col, stats_settings, depth) {
  # Initialize the list that stores the calculation result with its label and depth
  factor_vec <- factor(data[[label_col]])

  result <- list(
    label = levels(factor_vec),
    depth = rep(depth, times=length(levels(factor_vec)))
  )
  for (i in 1:length(stats_settings)) {
    result[[names(stats_settings)[i]]] <- as.vector(
      tapply(
        data[[value_col]], 
        factor_vec,
        stats_settings[[i]]
      )
    )
  }
  return(result)
}

concat_list <- function(lst_a, lst_b) {
  lst_new <- list()
  for (comp in names(lst_b)) {
    lst_new[[comp]] <- unlist(
      lapply(list(lst_a, lst_b), function(lst) lst[[comp]]))
  }
  return(lst_new)
}