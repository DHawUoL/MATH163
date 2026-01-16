# MATH163 – Week 08: Answers
suppressPackageStartupMessages(library(here))
set.seed(163)

ci_normal_sdknown <- function(x, sd_pop, alpha = 0.05) {
  xbar <- mean(x); n <- length(x); se <- sd_pop / sqrt(n)
  m <- qnorm(1 - alpha/2) * se
  c(lower = xbar - m, upper = xbar + m)
}

z <- rnorm(500, mean = 17, sd = 3)

# [8-TY-1]
ci95 <- ci_normal_sdknown(z, sd_pop = 3, alpha = 0.05)
ci99 <- ci_normal_sdknown(z, sd_pop = 3, alpha = 0.01)
ci95; ci99

# [8-TY-2]
autismDF <- readRDS(here("data","autism.rds"))
autismDF$Group <- factor(autismDF$Group, labels = c("No ASD","With ASD"))
AQ_noASD <- autismDF$AQ[autismDF$Group=="No ASD"]; sd_noASD <- 5.59
z_test_onesample <- function(x, mu0, sd_pop) {
  xbar <- mean(x); n <- length(x); se <- sd_pop / sqrt(n)
  z <- (xbar - mu0)/se; p <- 2*pnorm(-abs(z))
  list(z=z, p_value=p)
}
z_test_onesample(AQ_noASD, mu0 = 20, sd_pop = sd_noASD)

# [8-TY-3]
ci_verbose <- function(x, sd_pop, alpha = 0.05) {
  xbar <- mean(x); n <- length(x); se <- sd_pop / sqrt(n)
  m <- qnorm(1 - alpha/2) * se
  c(xbar = xbar, se = se, lower = xbar - m, upper = xbar + m)
}
ci_verbose(z, sd_pop = 3)

# [8-TY-4]
# Use z when σ is known or extremely well-established; use t when σ is estimated
# from the sample, especially for small/moderate n.
