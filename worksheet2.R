# MATH163 – Week 02: Storing data in data frames; factors
# Data required in data/: crying.rds  (columns: Cry, IQ, Sex=1/2), legroomDF example is in-code.

#### Week 2 — Packages (install vs load) and tidy file paths ####

# Today we WILL use one package: 'here'.
# "Install" = once per computer; "Load" = each new R session.

# Step A: Install 'here' if it isn’t on this machine yet (run once on a given PC).
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}

# Step B: Load the package for this session.
library(here)

# Step C: Two ways to tell R where your files are:

# Option 1 (most reliable on lab machines): set the working directory each session.
#   Put ALL practical files under your M: folder, e.g. M:/MATH163/practicals
#   Then either:
# setwd("M:/MATH163/practicals")
# or via RStudio: Session > Set Working Directory > Choose Directory...

# Option 2 (project-style paths): let 'here()' build paths from a project root.
#   'here()' looks upwards for a project marker (an .Rproj or .git, or a file called ".here").
#   If you’re not using an .Rproj, you can create a marker ONCE in your course folder:
#     file.create(".here")
#   Now `here("data","crying.rds")` means: <your course folder>/data/crying.rds

# Confirm what 'here()' thinks your root is (teaching moment):
cat("Project root detected by here(): ", here::here(), "\n")

# Path helper (works with either approach):
# - If you used setwd() to your course folder, this is equivalent to file.path().
# - If you set up a project marker, here("data","...") points into that folder.
data_path <- function(...) here::here("data", ...)

# Example: load Week 2 data safely and loudly fail if it’s missing.
cry_path <- data_path("crying.rds")
if (!file.exists(cry_path)) {
  stop("I can't find: ", cry_path,
       "\nFixes:\n - Put 'crying.rds' inside a 'data' subfolder under your course folder.\n",
       " - Either setwd(\"M:/MATH163/practicals\") OR create a project marker with file.create('.here') in that folder.\n",
       " - Re-run and check the printed root above matches your course folder.")
}
cryingDF <- readRDS(cry_path)

set.seed(163)

# --- INTRODUCTION ----------------------------------------------------
# You’ll load a data frame, explore columns, convert a numeric code into a factor,
# and make a few simple plots and summaries.

# --- WORKED EXAMPLE (WITH ANSWERS INLINE) ----------------------------
# GOAL: Learn the basic *shape* of tabular data in R (a data.frame),
#       how to inspect it, how to convert a numeric code into a labelled factor,
#       and how base R chooses a sensible default plot depending on inputs.

# 1) A TINY TOY DATA FRAME (LEGROOM EXAMPLE)
#    We’ll create a 1-column data.frame called 'legroomDF'. Each row is one person’s
#    minimum legroom requirement in cm. This shows the structure of a data.frame:
#    columns have names; rows are observations.
legroomDF <- data.frame(Legroom = c(70, 67, 76, 77, 71, 73, 65, 68, 72))

#    summary() gives quick descriptive stats for each column: min, quartiles, median, mean, max.
summary(legroomDF)

#    For a single numeric column, a quick way to *see* the spread is to plot the sorted values.
#    (Why sort? You’ll get an increasing “profile” that’s easy to read by eye.)
plot(sort(legroomDF$Legroom),
     main = "Legroom (sorted profile plot)",
     xlab = "Person index (after sorting)",
     ylab = "Legroom (cm)")

# 2) REAL DATA: CRYING vs IQ, WITH SEX AS A CODED VARIABLE
#    Our file 'crying.rds' is already loaded above into 'cryingDF'. It has:
#      - Cry: a measure of crying activity soon after birth
#      - IQ:  Stanford–Binet IQ score at age 3
#      - Sex: coded 1 for male, 2 for female (i.e., a *categorical* variable stored as numbers)
#    Step A: Inspect the structure and peek at the first few rows.
str(cryingDF)     # column names, types, and a preview
head(cryingDF)    # first 6 rows so you can see actual values

#    Step B: Convert the numeric code 'Sex' into a *factor* with readable labels.
#    Important: the labels vector maps *in order* to the underlying codes. Here 1->"Male", 2->"Female".
cryingDF$Sex <- factor(cryingDF$Sex, labels = c("Male", "Female"))

#    Check how factors work:
levels(cryingDF$Sex)     # shows the set of category labels
as.numeric(cryingDF$Sex) # shows their internal codes (1 for "Male", 2 for "Female")

# 3) PLOTTING WITH A FACTOR ON THE X-AXIS
#    Base R’s generic plot() behaves differently depending on inputs:
#      - numeric ~ numeric => scatter plot
#      - factor  ~ numeric => *boxplot by group* (super useful shortcut!)
#    (a) Cry vs Sex: puts two boxplots side-by-side (one per Sex)
plot(cryingDF$Sex, cryingDF$Cry,
     main = "Crying measure by Sex",
     xlab = "Sex",
     ylab = "Cry (arbitrary units)")
#    Reading: each box shows median (line), IQR (box), whiskers, and outliers.

#    (b) IQ vs Sex: again two boxplots, now comparing IQ distributions by Sex.
plot(cryingDF$Sex, cryingDF$IQ,
     main = "IQ by Sex",
     xlab = "Sex",
     ylab = "IQ score")

#    (c) IQ vs Cry as a scatter plot (two numeric vectors).
#        This shows whether higher crying is associated with higher/lower IQ.
plot(cryingDF$Cry, cryingDF$IQ,
     main = "IQ vs Cry",
     xlab = "Cry (arbitrary units)",
     ylab = "IQ score")

# 4) GROUPED SUMMARIES (TABLE-LIKE STATS PER GROUP)
#    aggregate() applies a function to columns grouped by one (or more) factor(s).
#    Here, “.~Sex” means: apply to *all other columns* (.) grouped by Sex.
#    Using 'summary' as the function gives a small bundle of stats for each column per group.
aggregate(. ~ Sex, data = cryingDF, summary)
#    Reading: Look across the Cry and IQ blocks under "Male" and "Female".
#    You can quickly compare medians/means/IQRs without eyeballing the whole dataset.

# --- TRY IT YOURSELF (NO ANSWERS HERE) -------------------------------
# [2-TY-1] Recreate a fresh legroomDF but call the column "Leg room" (with a space).
#          Run summary(), then try plot(legroomDF$Leg room). What happens and why?
# [2-TY-2] Using cryingDF: make boxplots of Cry by Sex and IQ by Sex.
# [2-TY-3] Compute the mean Cry for each Sex using aggregate() and using tapply(); compare.
# [2-TY-4] Make a scatter plot of IQ vs Cry, coloured by Sex (base R only).
