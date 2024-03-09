prices <- c(1, 1, 5, 5, 5, 5, 5, 8, 8, 10, 10, 10, 10, 12, 14, 14, 14, 15, 15, 15, 15, 15, 15, 18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 25, 25, 25, 25, 25, 28, 28, 30, 30, 30)
bins <- cut(prices, breaks = quantile(prices, probs = seq(0, 1, length.out = 4), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
bin_means <- tapply(prices, bins, mean)
smoothed_means <- bin_means[bins]
apply_boundary_smoothing <- function(price) {
  bin_boundaries <- quantile(prices, probs = seq(0, 1, length.out = 4), na.rm = TRUE)
  lower <- bin_boundaries[which.min(abs(bin_boundaries - price))]
  upper <- bin_boundaries[which.min(abs(bin_boundaries - price)) + 1]
  if (is.na(upper)) { 
    upper <- max(prices)
  }
  if(abs(price - lower) < abs(upper - price)) {
    return(lower)
  } else {
    return(upper)
  }
}
smoothed_boundaries <- sapply(prices, apply_boundary_smoothing)
hist(prices, breaks = quantile(prices, probs = seq(0, 1, length.out = 4), na.rm = TRUE), col = "lightblue", main = "Histogram of Prices with Equal-Frequency Binning", xlab = "Price", ylab = "Frequency")
