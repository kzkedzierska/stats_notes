create_a_line <- function(input_data) {
  center <- apply(input_data, 2, mean)
  dists <- apply(input_data,1, function(point) euc.dist(center, point))
  
  # random_point <- input_data[sample(which(dists >= quantile(dists, 0.75)), 1), ] 
  # random_point <- apply(input_data, 2, function(vec) rnorm(1, mean(vec), sd(vec)))
  # random_point <- c(3, 8)
  # find the equation of the line going through the center and random point
  a1 = (random_point[2] - center[2]) / (random_point[1] - center[1])
  b1 = center[2] - a1 * center[1]
  a1 <- 1
  b1 <- 3
  
  # get the projection coordinates
  b2 = input_data[,2] + (1 / a1) * input_data[,1]
  projected_x = (b2 - b1) / (a1 + 1 / a1)
  projected_y = a1 * projected_x + b1
  projected_data <- cbind(projected_x, projected_y)
  
  # get the distance between the points and coordinates
  squared_distance <- sum(sqrt(apply((input_data - projected_data)^2, 1, sum)))
  
  # plot
  plot_data <- 
    cbind(input_data, projected_data) %>%
    as_tibble() %>%
    rename(x = 1, y = 2, x_projected = 3, y_projected = 4) 
  
  plot_data_wide <-
    rbind(cbind(input_data, 1:nrow(input_data)),
          cbind(projected_data, 1:nrow(projected_data))) %>%
    as_tibble() %>%
    rename(x = 1, y = 2, data_point = 3)
  
  overhead <- 0.2
  min_x <- min(min(input_data[, 1]), min(projected_data[, 1]))
  max_x <- max(max(input_data[, 1]), max(projected_data[, 1]))
  projected_line <-
    tibble(x = seq((1 - (sign(min_x) * overhead)) * min_x, 
                  (1 + overhead) * max_x, 
                   length.out = 10)) %>%
    mutate(y = a1 * x + b1)
  
  plot_data %>%
    ggplot() +
    geom_point(aes(x, y))+ 
    geom_line(data = projected_line, aes(x, y), linetype = "dotdash") +
    geom_line(data = plot_data_wide, aes(x = x, y = y, group = data_point),
              color = "red") +
    coord_equal(expand = TRUE) +
    expand_limits(x = 0, y = 0) +
    labs(title = paste0("Squared distance: ", round(squared_distance, 2))) +
    # coord_fixed(xlim = c(0,10), ylim = c(0,10)) +
    NULL
}

create_a_line(Y)

