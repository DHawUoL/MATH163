# MATH163 – Week 04: Answers
suppressPackageStartupMessages(library(here))
skillsDF <- read.table(here("data","skills.csv"), header = TRUE, sep = ",")

# [4-TY-1]
older_highflu <- skillsDF$Maths_Fluency[
  skillsDF$Age >= 22 & skillsDF$Group == "Maths/Econ"
]
summary(older_highflu)

# [4-TY-2]
cols2 <- c("Group","Gender","Age")
res <- skillsDF[skillsDF$Age < 20 | skillsDF$Age > 30, cols2]
nrow(res); head(res)

# [4-TY-3]
skills_M <- subset(skillsDF, Gender == "Male")
skills_F <- subset(skillsDF, Gender == "Female")
mean(skills_M$Maths_Fluency, na.rm = TRUE)
mean(skills_F$Maths_Fluency, na.rm = TRUE)

# [4-TY-4]
head(skillsDF[order(skillsDF$Age), ], 15)
head(skillsDF[order(-skillsDF$Age), ], 15)
