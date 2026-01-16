# MATH163 – Week 05: TRY SOLUTIONS (base R script)
# -------------------------------------------------
# Answers for the "Try it yourself" items in week 5.
# No external packages or files are required.

set.seed(163)

#### [5-TY-1] ############################################################
# Build a 50×300 matrix of Geom(p = 0.2) using replicate().
# Show side-by-side histograms of column MEANS and MEDIANS.

m_ty <- replicate(300, rgeom(50, prob = 0.2))  # 50 rows × 300 cols
means_ty   <- colMeans(m_ty)
medians_ty <- apply(m_ty, 2, median)

op <- par(mfrow = c(1, 2))
hist(means_ty,
     main = "Means (50-sample, Geom p=0.2)",
     xlab = "sample mean")
hist(medians_ty,
     main = "Medians (50-sample, Geom p=0.2)",
     xlab = "sample median")
par(op)

# Notes:
# • Means tend to look more bell-shaped by CLT, even for skewed data.
# • Medians preserve discreteness/skew more visibly for Geometric data.


#### [5-TY-2] ############################################################
# Using m1 (100×150, Geom p=0.1), draw histograms of column MINIMA and MAXIMA.

m1 <- matrix(rgeom(100 * 150, prob = 0.1), nrow = 100, ncol = 150)

mins_m1 <- apply(m1, 2, min)
maxs_m1 <- apply(m1, 2, max)

op <- par(mfrow = c(1, 2))
hist(mins_m1,
     main = "Column minima (Geom p=0.1, n=100)",
     xlab = "min per column")
hist(maxs_m1,
     main = "Column maxima (Geom p=0.1, n=100)",
     xlab = "max per column")
par(op)

# Notes:
# • Minima: many columns contain a 0 (failures before first success), so mass near 0.
# • Maxima: long right tail—occasional large runs inflate the maximum.


#### [5-TY-3] ############################################################
# For n = 100, run the Binomial→Poisson visual check for p ∈ {0.01, 0.05, 0.1}.
# For each p:
#   (a) simulate many Binomial draws,
#   (b) estimate empirical PMF by relative frequencies,
#   (c) overlay Poisson(λ = n p).

overlay_poisson <- function(n = 100, p = 0.01, B = 200000) {
  x   <- rbinom(B, size = n, prob = p)
  tb  <- table(x)
  k   <- as.integer(names(tb))      # support values with nonzero counts
  emp <- as.numeric(tb) / sum(tb)   # empirical mass at each k
  
  # Base plot as vertical lines for empirical PMF
  plot(k, emp, type = "h", lwd = 3,
       main = sprintf("Bin(n=%d, p=%.3f) vs Poisson(λ=%.2f)", n, p, n*p),
       xlab = "k", ylab = "Probability")
  
  # Overlay Poisson PMF at same support
  lines(k, dpois(k, lambda = n * p), lwd = 2)
  legend("topright",
         legend = c("Empirical Binomial", sprintf("Poisson(λ=%.2f)", n*p)),
         lwd = c(3, 2), bty = "n")
}

op <- par(mfrow = c(1, 3))
overlay_poisson(p = 0.01)
overlay_poisson(p = 0.05)
overlay_poisson(p = 0.10)
par(op)

# Notes:
# • The Poisson approximation is best for small p with moderate n (λ = n p fixed).
# • As p grows, the Binomial shape departs more from Poisson.
