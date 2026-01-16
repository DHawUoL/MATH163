# MATH163 – Week 02: Answers
suppressPackageStartupMessages(library(here))

cryingDF <- readRDS(here("data","crying.rds"))
cryingDF$Sex <- factor(cryingDF$Sex, labels = c("Male","Female"))

# [2-TY-1]
legroomDF <- data.frame("Leg room" = c(70,67,76,77,71,73,65,68,72))
summary(legroomDF)
# plot(legroomDF$Leg room)  # this is a syntax error - R replaces a space with a ".":
names(legroomDF)
plot(legroomDF$Leg.room, main = "Leg room", ylab = "cm")
# Avoid spaces in names: better to use Leg_room or Leg.room.

# [2-TY-2]
boxplot(Cry ~ Sex, data = cryingDF, main = "Cry by Sex", ylab = "Cry")
boxplot(IQ  ~ Sex, data = cryingDF, main = "IQ by Sex",  ylab = "IQ")

# [2-TY-3]
aggregate(Cry ~ Sex, data = cryingDF, mean)
tapply(cryingDF$Cry, cryingDF$Sex, mean)

# [2-TY-4]
cols <- ifelse(cryingDF$Sex == "Male", "steelblue", "tomato")
plot(cryingDF$Cry, cryingDF$IQ, col = cols, pch = 16,
     xlab = "Cry", ylab = "IQ", main = "IQ vs Cry by Sex")
legend("topleft", c("Male","Female"), pch = 16, col = c("steelblue","tomato"), bty = "n")
