# MATH163 – Week 05: Generating multiple samples (base R script)
# --------------------------------------------------------------
# This week we’ll:
#  • generate many random samples from a known distribution (Geometric),
#  • store them as columns of a matrix (so each column is one sample),
#  • compute column-wise summaries (means, medians, minima, maxima),
#  • and see how these summaries vary from sample to sample.
# We’ll also make a quick visual check of the approximation
#     Binomial(n, p) ≈ Poisson(λ = n p)
# for small p.
#
# No external data or packages needed this week.
# Use this file as your “lab book”: read the comments, run the blocks,
# and annotate with your own notes as you go.

set.seed(163)

#### INTRODUCTION ####
# In all tasks below, one COLUMN = one sample. For example, a 100×150 matrix
# holds 150 separate samples of size 100 each.

#### WORKED EXAMPLE (WITH ANSWERS INLINE) ####

# 1) Build the same 100×150 dataset three ways (Geometric with p = 0.1)
#    rgeom(n, p) returns the number of failures before first success (0,1,2,...).

# Method A: generate one long vector then reshape it into 100 rows × 150 cols
m1 <- rgeom(100 * 150, prob = 0.1)  # 15,000 numbers
dim(m1) <- c(100, 150)

# Method B: pre-allocate a matrix and fill it row-by-row in a loop
m2 <- matrix(NA_integer_, nrow = 100, ncol = 150)
for (i in 1:100) {
  # each iteration fills the i-th row with 150 geometric draws
  m2[i, ] <- rgeom(150, prob = 0.1)
}

# Method C: replicate(): make 150 columns; each column is length-100 geom draws
m3 <- replicate(150, rgeom(100, prob = 0.1))  # 100×150

# 2) Compare distributions of sample MEANS and MEDIANS across columns
#    colMeans() is a fast column-mean; apply(..., 2, fun) works for general fun.

means_m1   <- colMeans(m1)
medians_m1 <- apply(m1, 2, median)

op <- par(mfrow = c(1, 2))  # 1 row, 2 panels (remember to restore later)
hist(means_m1,
     main = "Means of 150 samples (n=100, Geom p=0.1)",
     xlab = "sample mean")
hist(medians_m1,
     main = "Medians of 150 samples (n=100, Geom p=0.1)",
     xlab = "sample median")
par(op)

# Notes:
#  • Means often look more bell-shaped (CLT effect).
#  • Medians show discreteness/skew more strongly for right-skewed distributions.

# 3) Extremes: minima and maxima across samples (intuition check)
#    With geometric draws, many columns will include 0; maxima have long right tails.

mins_m1 <- apply(m1,  2, min)
maxs_m1 <- apply(m1,  2, max)

op <- par(mfrow = c(1, 2))
hist(mins_m1, main = "Column minima (Geom p=0.1, n=100)", xlab = "min per column")
hist(maxs_m1, main = "Column maxima (Geom p=0.1, n=100)", xlab = "max per column")
par(op)

# 4) Mini demo: Binomial → Poisson overlay (one case)
#    We simulate many Binomial(n=100, p=0.05) draws, estimate its PMF by frequency,
#    and overlay Poisson(λ = n p = 5) at the same support.

n   <- 100
p   <- 0.05
lam <- n * p

B   <- rbinom(2e5, size = n, prob = p)   # many draws → smooth empirical PMF
tab <- table(B)
k   <- as.integer(names(tab))            # support values with at least one count
emp <- as.numeric(tab) / sum(tab)        # empirical probability mass at each k

pois_pmf <- dpois(k, lambda = lam)

plot(k, emp, type = "h", lwd = 3,
     main = "Binomial(n=100, p=0.05) vs Poisson(λ=5)",
     xlab = "k", ylab = "Probability")
lines(k, pois_pmf, lwd = 2)
legend("topright",
       legend = c("Empirical Binomial", "Poisson(λ=5)"),
       lwd = c(3, 2), bty = "n")

#### TRY IT YOURSELF (NO ANSWERS BELOW THIS LINE) ####
# [5-TY-1] Build a 50×300 matrix of Geom(p = 0.2) using replicate().
#          Make side-by-side histograms of column MEANS and MEDIANS; comment.

# [5-TY-2] Using m1 (the 100×150, p=0.1 case), draw histograms of column MINIMA
#          and column MAXIMA. Explain the shape of each, in terms of geometric
#          right-skew and sample size per column.

# [5-TY-3] For n = 100, repeat the Binomial→Poisson visual check for
#          p in {0.01, 0.05, 0.1}. For each p:
#            (a) generate many Binomial draws,
#            (b) compute the empirical PMF (table / total),
#            (c) overlay Poisson(λ = n p).
#          A visual assessment is fine — note when/why the approximation looks best.
