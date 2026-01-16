# MATH163 – Week 10: t-tests and CIs (σ unknown)
# Data: data/Eastbourne.csv (columns year, month, sun.hours), data/bikerides.csv (one column)

suppressPackageStartupMessages(library(here))
set.seed(163)

# --- INTRODUCTION ----------------------------------------------------
# Use the t distribution when σ is unknown and must be estimated from the sample.
# In practice: we often estimate a baseline mean from historical data, then ask
# whether a later period looks consistent with that baseline using a t-test and/or t-based CI.

# --- WORKED EXAMPLE (WITH ANSWERS INLINE) ----------------------------
# Goal: Build a historical "baseline" of average monthly sunshine (up to 2009),
# then test 2010–2019 against that baseline month-by-month using one-sample t-tests.

# 0) Load the data
East <- read.csv(here("data","Eastbourne.csv"))

# 1) Create three slices of the data:
#    - E20:     year == 2020 (for later "special month" checks)
#    - E_10_19: years 2010–2019 inclusive (we'll test these against baseline)
#    - E_le_09: years up to and including 2009 (our historical baseline)
E20      <- subset(East, year == 2020)
E_10_19  <- subset(East, year >= 2010 & year <= 2019)
E_le_09  <- subset(East, year <= 2009)

# 2) Compute the baseline mean *by month* using data up to 2009.
#    mbar[i] will store the historical average sunshine for month i (1=Jan,...,12=Dec).
mbar <- numeric(12)
for (i in 1:12) {
  mbar[i] <- mean(E_le_09$sun.hours[E_le_09$month == i], na.rm = TRUE)
}
mbar  # show the 12 baseline means

# (Optional) Helper: t-based CI for a numeric vector x at level (1 - alpha).
ci_t <- function(x, alpha = 0.05) {
  x <- x[is.finite(x)]
  n <- length(x); xbar <- mean(x); s <- sd(x)
  se <- s / sqrt(n)
  tcrit <- qt(1 - alpha/2, df = n - 1)
  c(lower = xbar - tcrit*se, upper = xbar + tcrit*se)
}

# 3) Example test for a single month:
#    For January (month == 1) in 2010–2019, test if the mean equals the historical
#    baseline January mean (mbar[1]). This is a *one-sample t-test*.
jan_10_19 <- E_10_19$sun.hours[E_10_19$month == 1]
t_jan <- t.test(jan_10_19, mu = mbar[1])  # H0: mean(Jan_10_19) = baseline Jan mean
t_jan

#    Also show a t-based CI for the 2010–2019 January mean:
ci_t(jan_10_19)

# 4) Repeat (programmatically) for *all* months:
#    We'll store the 12 p-values from one-sample t-tests comparing each
#    month’s 2010–2019 mean to its baseline (up to 2009).
pvals <- numeric(12)
for (i in 1:12) {
  x <- E_10_19$sun.hours[E_10_19$month == i]
  pvals[i] <- t.test(x, mu = mbar[i])$p.value
}
pvals  # quick scan; in [10-TY-1] you'll discuss multiplicity/false positives

# (Optional) A quick glance at CIs for a couple of months:
# ci_t(E_10_19$sun.hours[E_10_19$month == 6])  # June CI 2010–2019
# ci_t(E_10_19$sun.hours[E_10_19$month == 12]) # December CI 2010–2019

# --- TRY IT YOURSELF (NO ANSWERS HERE) -------------------------------
# [10-TY-1]
# Using the 'pvals' vector above, identify which months are "significant" at α = 0.05.
# How many rejections might you *expect by chance* when running 12 separate tests
# (even if H0 were true for all months)? Write 2–3 lines explaining your logic.

# [10-TY-2]
# Focus on May. Let’s compare May 2020 to the historical baseline of May values.
# (a) Compute the "z-style" tail probability using *Normal* with mean/sd taken from
#     May (month==5) data up to 2009 (the baseline period). Use pnorm() on the
#     single May 2020 value from E20 to get a one- or two-sided tail probability;
#     state which tail you use and why.
# (b) Now, treating the *historical* May values (up to 2009) as your sample,
#     run a *one-sample t-test* with H0: μ = (May 2020 value).
#     Compare the two results in a couple of lines: how similar are the conclusions,
#     and why might they differ?

# Hints for [10-TY-2]:
#   may_hist <- E_le_09$sun.hours[E_le_09$month == 5]
#   may_2020 <- E20$sun.hours[E20$month == 5]
#   # z-style using baseline Normal:
#   #   ztail <- ???  (use pnorm with mean=mean(may_hist), sd=sd(may_hist))
#   # t-test using historical data vs mu = may_2020:
#   #   t.test(may_hist, mu = may_2020)

# [10-TY-3]
# Load 'bikerides.csv' (a single numeric column of ride lengths). Draw a random
# sample of n = 30 without replacement and test H0: μ = 18 using a two-sided
# t.test(). Repeat this whole process B = 500 times, collecting the p-values.
# Plot a histogram of the 500 p-values and write 2–3 lines commenting on
# what the shape suggests (e.g., are small p-values common or rare under your setup?).

# Hints for [10-TY-3]:
#   rides <- read.csv(here("data","bikerides.csv"))[[1]]
#   B <- 500; n <- 30; pvals_rides <- numeric(B)
#   for (b in 1:B) {
#     x <- sample(rides, n, replace = FALSE)
#     pvals_rides[b] <- t.test(x, mu = 18)$p.value
#   }
#   hist(pvals_rides, main = "p-values over 500 repeats", xlab = "p")
