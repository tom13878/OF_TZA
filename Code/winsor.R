
# Winsoring using fraction
winsor <- function(x, fraction=0.05, lower=TRUE, upper=TRUE){
  # Winsor functions at a quantile chosen by fraction
  if (length(fraction) != 1 || fraction < 0 || fraction > 0.1){
    stop('bad choice of fraction!!')
  }
  q <- quantile(x, probs = c(fraction, 1-fraction), na.rm = TRUE)
  ll <- q[1]
  ul <- q[2]
  if(lower){
    x[x < ll] <- ll
  }
  if(upper){
    x[x > ul] <- ul
  }
  return(x)
}

# Winsoring using median
winsor2 <- function (x, multiple=3){
  # winsor function by so many multiples from median average
  # deviation
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x, na.rm = TRUE)
  y <- x - med
  sc <- mad(y, center=0, na.rm = TRUE) * multiple
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  y + med
}

# Trimming data using fraction
trim <- function(x, fraction=0.05, lower=TRUE, upper=TRUE){
  # set values below or above a certain quantile to NA
  if (length(fraction) != 1 || fraction < 0 || fraction > 0.1){
    stop('bad choice of fraction!!')
  }
  q <- quantile(x, probs = c(fraction, 1-fraction), na.rm = TRUE)
  ll <- q[1]
  ul <- q[2]
  if(lower){
    x[x < ll] <- NA
  }
  if(upper){
    x[x > ul] <- NA
  }
  return(x)
}