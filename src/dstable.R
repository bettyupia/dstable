get_descriptive_stats_df <- function(data, group_by_cols, value_col, stats_settings)
  data.frame(get_descriptive_stats(data, group_by_cols, value_col, stats_settings))

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
        stats, 
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