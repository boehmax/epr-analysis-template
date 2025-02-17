find_closest_value <- function(x, target) {
  x[which.min(abs(x - target))]
}

compute_power_saturation <- function(data, sample_name, power_levels = c(20, 40, 80, 320, 640, 1280), selection_points) {
  power_saturation <- map_df(power_levels, function(p) {
    map_df(selection_points, function(y) {
      data_filtered <- data %>%
        filter(sample == sample_name, power_uW == p)
      
      data_filtered %>%
        filter(Field_G == find_closest_value(data_filtered$Field_G, y))
    })
  })
  
  return(power_saturation)
}