# MATH163 – Week 03: Answers
suppressPackageStartupMessages(library(here))
cryingDF <- readRDS(here("data","crying.rds"))
cryingDF$Sex <- factor(cryingDF$Sex, labels = c("Male","Female"))
VC <- readRDS(here("data","VC_salaries.rds")); if (is.data.frame(VC)&&ncol(VC)==1) VC <- VC[[1]]

# [3-TY-1]
par(mfrow = c(1,2))
hist(cryingDF$IQ[cryingDF$Sex=="Male"],   main="IQ (Male)",   xlab="IQ")
hist(cryingDF$IQ[cryingDF$Sex=="Female"], main="IQ (Female)", xlab="IQ")
par(mfrow = c(1,1))

# [3-TY-2]
par(mfrow = c(1,2))
stripchart(Cry ~ Sex, data = cryingDF, method="jitter", pch=16,
           main="Cry by Sex (stripchart)", ylab="Cry")
boxplot(Cry ~ Sex, data = cryingDF, main="Cry by Sex (boxplot)", ylab="Cry")
par(mfrow = c(1,1))

# [3-TY-3]
plot(cryingDF$Cry, cryingDF$IQ, xlab="Cry", ylab="IQ",
     main="IQ vs Cry with LS line")
abline(lm(IQ ~ Cry, data = cryingDF), col="red", lwd=2)

# [3-TY-4]
sort(VC, decreasing = TRUE)[1:10]
hist(VC, main="VC salaries", xlab="k £"); rug(VC)
