# MATH163 – Week 09: Goodness-of-fit (Chi-squared)
# Data needed:
#   - data/lottery.csv  (columns N1..N6 among others; each row is a draw)
# Optional for extension:
#   - Roulette.Rds  (integer vector of 0..36 outcomes from ~1000 spins; European wheel)
#
# Packages:
suppressPackageStartupMessages({
  library(here)  # tidy file paths
  library(vcd)   # goodfit() helper for discrete GOF (Poisson/NegBin)
})
set.seed(163)

# --- INTRODUCTION ----------------------------------------------------
# Goal: Compare an observed discrete distribution to an expected (proposed) distribution.
# The chi-squared statistic:
#    X^2 = sum_over_categories ( (Observed - Expected)^2 / Expected )
# Under H0 (model fits well) and with adequate expected counts,
#    X^2 ~ χ^2_(df) where df = (number of categories − 1 − number of fitted parameters).
# Today:
#   (1) Pool all lottery balls drawn and test if numbers 1..59 look uniform.
#   (2) Compare a simple 2-level piecewise model (1–49 vs 50–59).
#   (3) Look at the first column N1 and illustrate Poisson vs NegBin fit via goodfit().

# --- WORKED EXAMPLE (WITH ANSWERS INLINE) ----------------------------
# 0) Load data and build pooled counts across all 6 numbers per draw
lotDF <- read.csv(here("data", "lottery.csv"))

# Combine all six columns N1..N6 into one long vector of ball values:
allnum <- c(lotDF$N1, lotDF$N2, lotDF$N3, lotDF$N4, lotDF$N5, lotDF$N6)

# Turn that into counts per ball number (Observed):
tab <- table(allnum)

# Quick picture of the empirical frequencies:
barplot(tab,
        main = "Lottery ball frequencies (all positions pooled)",
        xlab = "Ball number", ylab = "Count")

# 1) Test for uniformity over the observed categories
#    chisq.test(table) with no p= argument uses equal probabilities across names(tab).
#    H0: Each number is equally likely; H1: not all equal.
gof_unif <- chisq.test(tab)
gof_unif
# Interpretation guide:
#  - If p-value is small (< 0.05), the deviations from uniform look too large to be chance.
#  - If p-value is not small, the data are consistent with uniformity.

# 2) Compare to a simple 2-level piecewise model: numbers 1–49 vs 50–59
#    Motivation: Suppose you hypothesise fewer high numbers are drawn (or vice versa).
#    We translate that into a model with one probability mass for 1–49 and another for 50–59.
freq_1_49  <- mean(allnum <= 49)    # empirical proportion of balls in 1..49
freq_50_59 <- 1 - freq_1_49         # empirical proportion in 50..59

# Build an expected probability vector over each category name(allnum):
#  - the first 49 numbers share freq_1_49/49 each
#  - the last 10 numbers share freq_50_59/10 each
p_vec <- c(rep(freq_1_49 / 49, 49), rep(freq_50_59 / 10, 10))

# Compare Observed (tab) to these expected probabilities:
gof_piecewise <- chisq.test(tab, p = p_vec, rescale.p = TRUE)
gof_piecewise
# Note: rescale.p=TRUE rescales p to sum to 1 if small rounding error exists.

# 3) Distributional fit for the first drawn number (column N1)
#    We’ll use vcd::goodfit() to fit Poisson and Negative Binomial to the *observed frequency
#    of values* in N1. First, put N1 values into a vector and shift to start at 0 for Poisson/NB.
n1 <- lotDF$N1
n1_shift0 <- n1 - min(n1)  # typically min is 1 → shift to 0..58 (example)

# Try Poisson fit:
gfp <- goodfit(n1_shift0, type = "poisson")
# Try NegBin fit:
gfn <- goodfit(n1_shift0, type = "nbinomial")

# Standing plots compare observed vs fitted frequencies visually:
par(mfrow = c(1, 2))
plot(gfp, type = "standing", main = "Poisson fit to N1 (shifted to 0)")
plot(gfn, type = "standing", main = "NegBin fit to N1 (shifted to 0)")
par(mfrow = c(1, 1))

# Formal chi-squared comparisons (goodfit stores observed & fitted):
chisq.test(gfp$observed, p = gfp$fitted, rescale.p = TRUE)
chisq.test(gfn$observed, p = gfn$fitted, rescale.p = TRUE)
# In practice for lottery *values*, Poisson is rarely appropriate; NegBin often fits
# “overdispersed” counts better than Poisson. If both fits are poor (tiny p-values),
# it signals the model family is not capturing the mechanism (which is fine—that’s a finding).

# --- TRY IT YOURSELF (NO ANSWERS HERE) -------------------------------
# [9-TY-1] Re-do the uniform GOF for N2 alone:
#          Hints:
#            tab2 <- table(lotDF$N2)
#            chisq.test(tab2)
#          Briefly comment on whether N2 looks uniform (use the p-value and the barplot).

# [9-TY-2] Fit Poisson to N1 (shifted to 0) using goodfit(..., type="poisson") and
#          compare its standing plot against the NegBin plot above. Which fits better,
#          and how can you tell?

# [9-TY-3] (Extended) Roulette fairness — European wheel (0..36)
#          You are given Roulette.Rds with ~1000 outcomes. Test fairness under:
#            (a) Uniform 0..36
#            (b) Low (1–18) vs High (19–36)   [exclude 0]
#            (c) Red vs Black                 [exclude 0]
#            (d) Even vs Odd                  [exclude 0]
#          Write short (2–3 line) conclusions: H0, stat/df, p-value, plain-English takeaway.

## ---------- SKELETON FOR [9-TY-3] (uncomment and complete) ----------
# spins <- readRDS(here("data", "Roulette.Rds"))
#
# ## (a) Uniform 0..36
# tab_all <- table(spins)                     # counts for each outcome 0..36
# gof_all <- chisq.test(tab_all, p = rep(1/37, 37), rescale.p = TRUE)
# barplot(tab_all, main = "Roulette: outcomes (all)", xlab = "Number", ylab = "Count")
# gof_all
#
# ## Helper: drop zeros (green) for (b)–(d)
# spins_1_36 <- spins[spins %in% 1:36]
#
# ## (b) Low vs High
# band <- factor(ifelse(spins_1_36 <= 18, "Low", "High"), levels = c("Low", "High"))
# tab_lh <- table(band)
# chisq.test(tab_lh, p = c(0.5, 0.5))
#
# ## (c) Red vs Black (European red numbers)
# reds <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
# rb <- factor(ifelse(spins_1_36 %in% reds, "Red", "Black"), levels = c("Red", "Black"))
# tab_rb <- table(rb)
# chisq.test(tab_rb, p = c(0.5, 0.5))
#
# ## (d) Even vs Odd
# eo <- factor(ifelse(spins_1_36 %% 2 == 0, "Even", "Odd"), levels = c("Even", "Odd"))
# tab_eo <- table(eo)
# chisq.test(tab_eo, p = c(0.5, 0.5))
#
# ## Notes to include in your write-up:
# # - Always check that expected counts are not too small (rule-of-thumb ≥ 5 per cell).
# # - Chi-squared GOF assumes independent spins/draws and a fixed model under H0.
# # - A small p-value suggests evidence against the specified “fair” model; a large p-value
# #   means the data are consistent with that model (not proof of fairness, just no strong
# #   evidence against it).