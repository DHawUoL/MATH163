# MATH163 – Week 09: Answers
suppressPackageStartupMessages({
  library(here)
  library(vcd)     # for goodfit()
})

## Load lottery data
lotDF <- read.csv(here("data","lottery.csv"))

# [9-TY-1] Uniform GOF for N2 alone
chisq.test(table(lotDF$N2))

# [9-TY-2] Poisson fit to N1 (shifted to 0); standing plot + GOF on fitted vs observed
g <- goodfit(lotDF$N1 - 1, type = "poisson")   # shift so support starts at 0
plot(g, type = "standing", main = "Poisson fit (N1)")
chisq.test(g$observed, p = g$fitted, rescale.p = TRUE)

################################################################
#### EXTENSION EXERCISE — Roulette fairness (European)     ####
################################################################
# Data structure expected: a data.frame with columns
#   Outcome: integer 0..36
#   Counts:  frequency of that outcome in ~1000 spins
#   Color:   "Red","Black","Green" (0 is Green)

# Load
roul <- readRDS(here("data","Roulette.Rds"))
stopifnot(all(c("Outcome","Counts","Color") %in% names(roul)))

## 1) Goodness-of-fit to Uniform(0..36) (includes 0)
# H0: All 37 numbers equally likely (p = 1/37 each)
# Build named vector of observed counts (ensure full 0..36 coverage and order)
obs_all <- with(roul, setNames(Counts, Outcome))
obs_all <- obs_all[as.character(0:36)]  # order by 0..36

cat("\n[Extension-1] Uniform(0..36) GOF\nObserved counts (0..36):\n")
print(obs_all)

gof_all <- chisq.test(obs_all, p = rep(1/37, 37), rescale.p = TRUE)
print(gof_all)
# Interpretation: small p (< 0.05) = evidence against uniformity; large p = consistent with fair.

## Prepare data excluding 0 for the 2×1 tests
roul_1_36 <- subset(roul, Outcome %in% 1:36)

## 2) Low (1–18) vs High (19–36) — exclude 0
band <- ifelse(roul_1_36$Outcome <= 18, "Low", "High")
counts_lh <- tapply(roul_1_36$Counts, band, sum)
counts_lh <- counts_lh[c("Low","High")]  # enforce order

cat("\n[Extension-2] Low vs High (drop 0)\nCounts:\n")
print(counts_lh)

gof_lh <- chisq.test(counts_lh, p = c(0.5, 0.5))
print(gof_lh)

## 3) Red vs Black — exclude 0 (use provided Color)
counts_rb <- tapply(roul_1_36$Counts, roul_1_36$Color, sum)
# keep only Red/Black and enforce order
counts_rb <- counts_rb[c("Red","Black")]

cat("\n[Extension-3] Red vs Black (drop 0)\nCounts:\n")
print(counts_rb)

gof_rb <- chisq.test(counts_rb, p = c(0.5, 0.5))
print(gof_rb)

## 4) Even vs Odd — exclude 0
parity <- ifelse(roul_1_36$Outcome %% 2 == 0, "Even", "Odd")
counts_eo <- tapply(roul_1_36$Counts, parity, sum)
counts_eo <- counts_eo[c("Even","Odd")]  # enforce order

cat("\n[Extension-4] Even vs Odd (drop 0)\nCounts:\n")
print(counts_eo)

gof_eo <- chisq.test(counts_eo, p = c(0.5, 0.5))
print(gof_eo)

## Notes:
# - One-sample chi-squared GOF on aggregated counts in each case.
# - Assumptions: independent spins; adequate expected counts (rule-of-thumb ≥ 5).
# - A non-significant result does not prove fairness; it indicates no strong evidence against
#   the specified fair model in this sample.
