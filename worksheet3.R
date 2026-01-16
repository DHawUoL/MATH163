# MATH163 – Week 03: Plotting (scatter, line, hist, side-by-side, groups)
# Data: data/crying.rds, data/VCsalaries.rds

suppressPackageStartupMessages(library(here))
set.seed(163)

# --- INTRODUCTION ----------------------------------------------------
# Turn vectors into clear plots; place related plots side-by-side; choose
# plot types to match data; annotate.

# --- WORKED EXAMPLE (WITH ANSWERS INLINE) ----------------------------
# GOAL: Practise choosing the *right* plot for the data and the question.
#       We'll compare "values in data order" vs "sorted values", draw histograms,
#       use side-by-side layouts, and show grouped plots (boxplots by Sex).
#       Then we'll repeat the pattern on a second dataset (VC salaries).

# 1) LOAD AND PREPARE THE CRYING DATA
#    cryingDF has columns: Cry (crying measure), IQ (IQ at age 3), Sex (1/2).
#    Convert Sex from numeric codes to factor labels for nicer plots.
cryingDF <- readRDS(here("data","crying.rds"))
cryingDF$Sex <- factor(cryingDF$Sex, labels = c("Male","Female"))

# 2) LINES VS POINTS: WHEN DOES ORDER MATTER?
#    (a) Plot IQ in the *original data order*. This answers:
#        "Do I see any gross jumps or patterns *as recorded* (e.g., a block structure)?"
plot(cryingDF$IQ,
     main = "IQ in data order",
     xlab = "Row/index (original order)",
     ylab = "IQ")

#    (b) Sort IQ first, then plot as a *continuous line*.
#        This answers: "What is the *distributional shape* of IQ if I line up values?"
#        Sorting removes the original order and reveals the overall profile.
iq_sorted <- sort(cryingDF$IQ)
plot(iq_sorted,
     type = "l",                      # "l" = line plot (continuous)
     main = "IQ (sorted)",
     xlab = "Index (after sorting)",
     ylab = "IQ")

# 3) HISTOGRAMS: SHAPE, CENTER, SPREAD, TAILS
#    We’ll put two histograms *side-by-side* to compare IQ and Cry directly.
#    par(mfrow = c(1,2)) asks R to draw 1 row with 2 columns of plots.
par(mfrow = c(1,2))
hist(cryingDF$IQ,
     main = "IQ (histogram)",
     xlab = "IQ",
     col = "gray90", border = "white")      # soft fill aids readability
hist(cryingDF$Cry,
     main = "Cry (histogram)",
     xlab = "Cry",
     col = "gray90", border = "white")
#    Reading tips:
#      • Histograms are for *numeric* data and show frequency by bin.
#      • Look for skewness (long tail), multiple peaks, and outliers.
par(mfrow = c(1,1))  # reset to a single plot layout

# 4) GROUPED COMPARISONS: BOXPLOTS BY SEX
#    The formula syntax y ~ group tells R: "draw y grouped by group".
#    Boxplots summarise each group (median line, IQR box, whiskers, outliers).
#    Put the two boxplots (IQ-by-Sex, Cry-by-Sex) side-by-side for quick comparison.
par(mfrow = c(1,2))
boxplot(IQ ~ Sex, data = cryingDF,
        main = "IQ by Sex",
        ylab = "IQ",
        col = c("gray85","gray95"))
boxplot(Cry ~ Sex, data = cryingDF,
        main = "Cry by Sex",
        ylab = "Cry",
        col = c("gray85","gray95"))
#    Reading tips:
#      • Median = horizontal line inside the box.
#      • Box = interquartile range (middle 50%).
#      • Whiskers ≈ typical spread beyond the IQR.
#      • Dots = outliers (if any).
par(mfrow = c(1,1))  # reset layout

# 5) SECOND DATASET: VC SALARIES
#    File contains one numeric vector (or a one-column data frame).
#    We coerce to a plain vector 'VC' so that hist/boxplot get simple inputs.
VC <- readRDS(here("data","VC_salaries.rds"))
if (is.data.frame(VC) && ncol(VC) == 1) VC <- VC[[1]]

#    We’ll show *two complementary views*:
#      • Histogram: shows full distribution (skewness, tails).
#      • Boxplot (horizontal): compact summary + highlights outliers.
par(mfrow = c(1,2))
hist(VC,
     breaks = "Sturges",             # a standard automatic rule for bin count
     main = "VC salaries (histogram)",
     xlab = "Salary (thousands £)",
     col = "gray90", border = "white")
boxplot(VC,
        horizontal = TRUE,           # easy to read long-tailed distributions
        main = "VC salaries (boxplot)",
        xlab = "Salary (thousands £)",
        col = "gray95")
par(mfrow = c(1,1))
#    Reading tips:
#      • If the right tail is long, the distribution is positively skewed.
#      • Outliers often appear as individual points beyond whiskers.
#      • Histogram shows *where* mass is; boxplot summarises *how much* spread.

# --- TRY IT YOURSELF (NO ANSWERS HERE) -------------------------------
# [3-TY-1] Histograms of IQ by Sex (two panels); label clearly.
# [3-TY-2] Stripchart Cry by Sex (jitter) vs boxplot Cry by Sex; comment which shows what.
# [3-TY-3] IQ vs Cry scatter; add least-squares line via abline(lm(...)).
# [3-TY-4] Show top 10 VC salaries and add rug() to histogram; comment on skew/outliers.
