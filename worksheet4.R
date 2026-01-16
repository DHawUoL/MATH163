# MATH163 – Week 04: Subsetting rows/columns, order, subset, with()
# Data: data/skills.csv

suppressPackageStartupMessages(library(here))
set.seed(163)

skillsDF <- read.table(here("data","skills.csv"), header = TRUE, sep = ",")

# --- INTRODUCTION ----------------------------------------------------
# Build precise row/column queries; create smaller views; sort; summarise.

# --- WORKED EXAMPLE (WITH ANSWERS INLINE) ----------------------------
# GOAL: Practise 2D data-frame indexing:
#       • rows selected by logical (TRUE/FALSE) conditions,
#       • columns selected by names or position,
#       • combining both at once: df[ row_condition , columns_to_keep ],
#       • shortcuts with with(), and sorting rows with order().

# 1) ORIENT YOURSELF: what columns exist, what types, and a quick peek at values.
str(skillsDF)  # column names, data types, and a few example values per column
head(skillsDF) # first 6 rows so you can see real data

# 2) SELECTING ROWS BY A CONDITION (1D indexing on a single column)
#    Q: “Show the Age values for participants aged 21 or over.”
#    • Inside [ ], we supply a logical vector the same length as the column.
#    • R keeps entries where the condition is TRUE.
skillsDF$Age[ skillsDF$Age >= 21 ]

#    Q: “Show the Age values for participants aged between 19 and 21 inclusive.”
#    • Combine conditions with & (AND) and | (OR); use parentheses to be explicit.
skillsDF$Age[ skillsDF$Age >= 19 & skillsDF$Age <= 21 ]

# 3) SELECTING ROWS *AND* (SOME) COLUMNS (2D indexing)
#    Pattern: df[ row_condition , columns_to_keep ]
#    Define a small set of columns we’ll use repeatedly (less typing, fewer mistakes).
cols <- c("Group","Age","Maths_Fluency","TotalLapses","MA_Eval")

#    Q: “Show Group, Age, Maths_Fluency, TotalLapses, MA_Eval
#        for Maths/Econ students who had at least one lapse.”
#    • LEFT of comma: row condition (TRUE/FALSE for each row).
#    • RIGHT of comma: vector of column names to keep (order matters).
skillsDF[ skillsDF$TotalLapses >= 1 & skillsDF$Group == "Maths/Econ", cols ]

# 4) USING with() TO REDUCE TYPING INSIDE A SINGLE EXPRESSION
#    with(df, <expression-using-bare-column-names>)
#    Q: “Within Maths/Econ, list Age for those Age >= 21 or Age <= 19.”
#    • Inside with(...), we can refer to columns directly (Age, Group).
with(skillsDF, Age[ (Age >= 21 | Age <= 19) & Group == "Maths/Econ" ])

# 5) MAKING A SMALLER DATA FRAME AND SORTING ROWS
#    Step A: keep only the columns of interest.
skills_small <- skillsDF[, cols]

#    Step B: order() returns the row indices that would sort by a key.
#            Here we sort ascending by Maths_Fluency; use decreasing=TRUE for high→low.
skills_small <- skills_small[ order(skills_small$Maths_Fluency), ]
#    Sanity check the result: lowest fluency at the top now.
head(skills_small)

# --- TRY IT YOURSELF (NO ANSWERS HERE) -------------------------------
# [4-TY-1] older_highflu: Maths_Fluency for Age≥22 & Group=="Maths/Econ"; show summary().
# [4-TY-2] cols2 <- c("Group","Gender","Age"); rows with Age<20 or Age>30; how many?
# [4-TY-3] Split by Gender and compare mean Maths_Fluency (na.rm=TRUE).
# [4-TY-4] Order by Age ascending; show first 15; then by Age descending.
