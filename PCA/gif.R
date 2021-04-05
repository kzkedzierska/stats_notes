library(tidyverse)
library(animation)

saveGIF(movie.name = "PCA.gif", {
  input_data <- Y
  for (i in 1:1000) {
    if (i == 1) {
      a1 <- rnorm(1)
      b1 <- rnorm(1)
      squared_distance_old <- Inf
    } else {
      a1_old <- a1
      b1_old <- b1
      squared_distance_old <- squared_distance
      a1 = rnorm(1, mean = a1, sd = 0.05)
      b1 = rnorm(1, mean = b1, sd = 0.05)
    }
    
    # get the projection coordinates
    b2 = input_data[,2] + (1 / a1) * input_data[,1]
    projected_x = (b2 - b1) / (a1 + 1 / a1)
    projected_y = a1 * projected_x + b1
    projected_data <- cbind(projected_x, projected_y)
    
    # get the distance between the points and coordinates
    squared_distance <- sum(sqrt(apply((input_data - projected_data)^2, 1, sum)))
    
    if (squared_distance > squared_distance_old) {
      a1 <- a1_old
      b1 <- b1_old
      squared_distance <- squared_distance_old
      next()
    }
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
    
    plt <-
      plot_data %>%
      ggplot() +
      geom_point(aes(x, y))+ 
      geom_line(data = projected_line, aes(x, y), linetype = "dotdash") +
      geom_line(data = plot_data_wide, aes(x = x, y = y, group = data_point),
                color = "red") +
      coord_equal(expand = TRUE, xlim = c(-4, 4), ylim = c(-4, 4)) +
      expand_limits(x = 0, y = 0) +
      labs(title = paste0("Sum of squared distance: ", round(squared_distance, 2))) + 
      # coord_cartesian() +
      NULL
    if (i %% 8 == 0) { print(plt) }
    
  }
}, fps = 10)
