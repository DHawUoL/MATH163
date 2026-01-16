# MATH163 – Week 08: Z-tests and Normal confidence intervals (σ known)
# Data: data/autism.rds  (Group factor: 1/2 → rename), meta SDs for AQ known.

suppressPackageStartupMessages(library(here))
set.seed(163)

# --- INTRODUCTION ----------------------------------------------------
# Goal this week:
# • When the population standard deviation (σ) is KNOWN (or agreed from prior large studies),
#   we estimate the population mean μ using a Normal-based confidence interval (CI),
#   and we can test H0: μ = μ0 with a z-test.
# • You’ll see this in a small simulated example and then with real AQ data where meta-analytic
#   σ values are provided (so “σ known” is reasonable there).

# --- WORKED EXAMPLE (WITH ANSWERS INLINE) ----------------------------
# 1) A reusable function for CI when σ is known.
#    INPUTS:
#      x       = numeric vector of sample observations
#      sd_pop  = KNOWN population standard deviation (σ) for the variable you’re measuring
#      alpha   = 1 - confidence level (e.g., 0.05 for a 95% CI)
#    IDEA:
#      The sampling distribution of the sample mean x̄ is Normal with mean μ and
#      standard error se = σ / sqrt(n) when σ is known (and n independent observations).
#      A (1−α) CI is x̄ ± z_{1−α/2} * se.
ci_normal_sdknown <- function(x, sd_pop, alpha = 0.05) {
  # Compute the sample mean (our point estimate of μ)
  xbar <- mean(x)
  
  # Number of observations (n) used to scale the standard error
  n <- length(x)
  
  # Standard error of the mean using the KNOWN σ
  se <- sd_pop / sqrt(n)
  
  # Two-sided Normal critical multiplier, e.g., z_{0.975} ≈ 1.96 for 95% CIs
  z_mult <- qnorm(1 - alpha/2)
  
  # Margin of error = multiplier × standard error
  m <- z_mult * se
  
  # Return the lower/upper CI bounds as a named vector
  c(lower = xbar - m, upper = xbar + m)
}

# 2) Simulated data where σ is known.
#    SCENARIO:
#      Suppose we are sampling a measurement that historically has σ = 3 exactly
#      (e.g., a physical instrument with a well-established precision).
#      We take a random sample of size n = 50 from a Normal population with mean 17.
z <- rnorm(50, mean = 17, sd = 3)

#    Our sample mean (x̄). This will vary from run to run because of randomness.
mean(z)

#    95% CI for μ using the KNOWN σ = 3. This is valid to quote as a Normal CI.
ci_normal_sdknown(z, sd_pop = 3, alpha = 0.05)

#    INTERPRETATION NOTE:
#    “95% CI” means: if we repeated this whole sampling procedure many times,
#    about 95% of such intervals would cover the true μ. It does NOT mean there is a 95%
#    probability that μ lies in *this* particular interval (μ is a fixed constant).

# 3) Autism AQ example: using literature SDs (treated as known).
#    DATA:
#      autism.rds has columns including:
#        - Group (coded 1/2; we’ll relabel to “No ASD”, “With ASD”)
#        - AQ    (Autism Quotient total score)
#    CONTEXT:
#      Meta-analyses report typical SDs for AQ in non-clinical vs ASD adult samples.
#      We’ll use those as σ to form z-based CIs for the group means.
autismDF <- readRDS(here("data","autism.rds"))

#    Make Group readable: 1 → "No ASD", 2 → "With ASD"
autismDF$Group <- factor(autismDF$Group, labels = c("No ASD", "With ASD"))

#    Split AQ by group (two independent samples, each with its own σ from the literature)
AQ_noASD <- autismDF$AQ[autismDF$Group == "No ASD"]
AQ_ASD   <- autismDF$AQ[autismDF$Group == "With ASD"]

#    Literature (meta-analysis) SDs treated as known (σ):
sd_noASD <- 5.59
sd_ASD   <- 6.27

#    95% CIs for μ_NoASD and μ_ASD using the known σ values:
ci_normal_sdknown(AQ_noASD, sd_pop = sd_noASD, alpha = 0.05)
ci_normal_sdknown(AQ_ASD,   sd_pop = sd_ASD,   alpha = 0.05)

#    QUICK CHECK / VISUAL: plot sample means with their z-based CIs (optional but helpful)
xbar_noASD <- mean(AQ_noASD); n1 <- length(AQ_noASD); se1 <- sd_noASD / sqrt(n1)
xbar_ASD   <- mean(AQ_ASD);   n2 <- length(AQ_ASD);   se2 <- sd_ASD   / sqrt(n2)
z_mult <- qnorm(0.975)

means <- c(NoASD = xbar_noASD, ASD = xbar_ASD)
los   <- means - z_mult * c(se1, se2)
his   <- means + z_mult * c(se1, se2)

op <- par(mar = c(5, 6, 4, 2) + 0.1)
plot(1:2, means, xaxt="n", xlab="", ylab="AQ mean (with z-95% CI)", pch=16,
     ylim = range(c(los, his))*c(0.98,1.02), main = "AQ means with σ-known CIs")
axis(1, at = 1:2, labels = c("No ASD","With ASD"))
arrows(1:2, los, 1:2, his, angle = 90, code = 3, length = 0.06)
abline(h = 0, col = "grey90")
par(op)

# 4) (Optional) A simple helper for a two-sided one-sample z-test of H0: μ = μ0.
#    WHY THIS WORKS HERE:
#      With σ known, Z = (x̄ − μ0)/SE follows Standard Normal under H0.
#    RETURNS:
#      z statistic and its two-sided p-value.
z_test_onesample <- function(x, mu0, sd_pop) {
  xbar <- mean(x)
  n    <- length(x)
  se   <- sd_pop / sqrt(n)
  z    <- (xbar - mu0) / se
  p    <- 2 * pnorm(-abs(z))     # two-sided p-value from N(0,1)
  list(z = z, p_value = p, xbar = xbar, se = se)
}

#    EXAMPLE (purely illustrative): is μ_NoASD equal to 20?
z_test_onesample(AQ_noASD, mu0 = 20, sd_pop = sd_noASD)

#    WHEN TO USE Z vs T (rule of thumb, repeated here for clarity):
#      • Use z when σ is truly known (e.g., calibrated instrument) or very well-established
#        from a large, trusted source (like a meta-analysis for this specific measure).
#      • Otherwise, use t (Week 10), which replaces σ with the sample SD and accounts
#        for its uncertainty (especially important for smaller n).

# --- TRY IT YOURSELF (NO ANSWERS HERE) -------------------------------
# [8-TY-1] For z ~ N(17, 3^2) with n=500, compute the 95% and 99% CIs (σ known).
# [8-TY-2] Using AQ_noASD and sd_noASD, perform a z-test of H0: μ = 20 (two-sided).
# [8-TY-3] Create a function that also returns xbar and se along with CI bounds.
# [8-TY-4] Briefly comment when a z-approach is reasonable vs when you’d switch to t.
