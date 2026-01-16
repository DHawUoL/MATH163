# MATH163 – Week 10: Answers
suppressPackageStartupMessages(library(here))
set.seed(163)

East <- read.csv(here("data","Eastbourne.csv"))
E20   <- subset(East, year == 2020)
E_10_19 <- subset(East, year >= 2010 & year <= 2019)
E_le_09 <- subset(East, year <= 2009)

# [10-TY-1]
mbar <- sapply(1:12, function(i) mean(E_le_09$sun.hours[E_le_09$month==i]))
pvals <- sapply(1:12, function(i) {
  x <- E_10_19$sun.hours[E_10_19$month==i]
  t.test(x, mu = mbar[i])$p.value
})
which(pvals < 0.05)
# About 12 tests at alpha=0.05 ⇒ expect ~0.6 false positives on average.

# [10-TY-2]
may_hist <- E_le_09$sun.hours[E_le_09$month == 5]
may2020  <- E20$sun.hours[E20$month == 5]
mhat <- mean(may_hist); shat <- sd(may_hist)
# z-style tail prob for a single value relative to N(mhat, shat^2/51)
pnorm(may2020, mean = mhat, sd = shat/sqrt(length(may_hist)), lower.tail = FALSE)
# One-sample t comparing mean of history vs fixed number (illustrative)
t.test(may_hist, mu = may2020)

# [10-TY-3]
bikes <- read.csv(here("data","bikerides.csv"), header = FALSE)[,1]
pvec <- replicate(500, {
  s <- sample(bikes, 30)  # σ unknown ⇒ t-test
  t.test(s, mu = 18)$p.value
})
hist(pvec, main = "P-values for 500 t-tests (n=30)")

