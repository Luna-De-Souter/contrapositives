# This script defines a function for calculating contrapositive consistency
# and contrapositive coverage based on a cna output and a dataset.
# The dataset should be in the form of a dataframe.
# Currently, the function only calculates contrapositive consistency and
# contrapositive coverage for atomic solution formulas (asfs), not for
# complex solution formulas (csfs).
# The function returns a dataframe of asfs, as is standardly returned by the
# asfs() function, with two additional columns, one containing the contrapositive
# consistency of the asfs and one containing the contrapositive coverage of the
# asfs in the dataframe.

### defining the function
contrapositives <- function(x, df){
  asfs <- asf(x)
  asfs2 <- asfs
  N <- nrow(df)
  asfs2$Y <- sapply(asfs2$outcome, function(y) sum(df[,y]))
  asfs2$XY <- asfs2$coverage * asfs2$Y
  asfs2$xY <- asfs2$Y - asfs2$XY
  asfs2$X <- asfs2$coverage/asfs2$consistency*asfs2$Y
  asfs2$x_small <- N - asfs2$X
  asfs2$y_small <- N - asfs2$Y
  asfs2$xy <- asfs2$x_small - asfs2$xY
  asfs$contrapositive_consistency <- asfs2$xy / asfs2$y_small
  asfs$contrapositive_coverage <- asfs2$xy / asfs2$x_small
  return(asfs)
}

### example
library(cna)

# Run a cna analysis.
ana <- cna(d.irrigate, con = 0.8, cov = 0.8)

# Calculate contrapositive consistency and contrapositive coverage, using the 
# calc_contr function.
asfs_contr <- contrapositives(ana, d.irrigate)
asfs_contr


