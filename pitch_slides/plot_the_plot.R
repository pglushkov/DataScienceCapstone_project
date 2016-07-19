plot_ze_plot <- function() {

  model_size = c(1.5, 3.5, 8, 15, 20);
  search_time = c(0.015, 0.026, 0.041, 0.054, 0.13);

  plot(model_size, search_time, pch = '.', xlab = 'model size (mb)',
    ylab = 'search time (sec)');

  lines(model_size, search_time, col = 'purple', lty = 1, lwd = 2);

}
