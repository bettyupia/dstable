INDENT_STR = "\\hspace{1cm}"

GetDescriptiveStatsDataFrame = function(data, group_by_cols, value_col, stats_settings, level)
  GetDescriptiveStats(data, group_by_cols, value_col, stats_settings, level) %>% data.frame()

GetDescriptiveStats = function(data, group_by_cols, value_col, stats_settings, level)
  GetDescriptiveStatsRec(data, list(), group_by_cols, value_col, stats_settings, 0)

GetDescriptiveStatsRec = function(data, stats, group_by_cols, value_col, stats_settings, level) {
  group_col = group_by_cols[1]
  if (length(group_by_cols) == 1) {
    return(GetStats(data, group_col, value_col, stats_settings))
  }
  
  for (group_value in data[[group_col]] %>% 
       factor() %>% 
       levels()) {
    stats = Concat(stats, GetStats(data[data[[group_col]] == group_value,], group_col, value_col, stats_settings, level))
    stats = Concat(stats, GetDescriptiveStatsRec(data[data[[group_col]] == group_value,], stats, group_by_cols[-1], value_col, stats_settings, level + 1))
  }
  return(stats)
}

GetStats = function(data, group_by_col, value_col, stats_settings, level) {
  stats = list(
    label = paste(
      paste(rep(INDENT_STR, times = level), collapse = ""),
      data[[group_by_col]] %>% 
        factor() %>% 
        levels(),
      sep = ""
    )
  )
  for (i in 1:length(stats_settings)) {
    stats[[names(stats_settings)[i]]] = tapply(
      data[[value_col]], 
      data[[group_by_col]] %>% factor(),
      stats_settings[[i]]
      ) %>% as.vector()
  }
  return(stats)
}

Concat = function(lst_a, lst_b) {
  new_list = list()
  for (comp_name in lst_b %>% names()) {
    new_list[[comp_name]] = lapply(list(lst_a, lst_b), function(lst) lst[[comp_name]]) %>% unlist()
  }
  return(new_list)
}