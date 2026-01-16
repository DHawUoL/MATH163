# MATH163 – Week 01: Answers

suppressPackageStartupMessages(library(here))
set.seed(163)

# [1-TY-1]
x <- seq(0, 4*pi, by = 0.01*pi)
plot(x, sin(x), type = "l", xlab = "x", ylab = "value", main = "sin & cos")
lines(x, cos(x), col = "blue", lwd = 2)
legend("topright", c("sin","cos"), lty = 1, col = c("black","blue"), bty = "n")

# [1-TY-2]
g <- function(u) u^2 + u + 1
g(5); g(c(1,2,3))

# [1-TY-3]
f <- function(u) exp(-u^2/2)/sqrt(2*pi)
x1 <- seq(-5,5, by = 0.1);  trap1 <- sum(f(x1)) * 0.1
x2 <- seq(-5,5, by = 0.01); trap2 <- sum(f(x2)) * 0.01
trap1; trap2  # should be close to 1.0; note that the finer grid is NOT closer to 1

# Use trapezium rule to see the difference:
options(digits = 17)
f <- function(u) dnorm(u)
trap <- function(f, a, b, h) {
  x <- seq(a, b, by = h)
  h * (0.5*f(x[1]) + sum(f(x[-c(1,length(x))])) + 0.5*f(x[length(x)]))
}
trap(f, -5, 5, h = 0.1)
trap(f, -5, 5, h = 0.01)

# [1-TY-4]
x2 <- seq(1, 20, by = 2)
fx <- (x2^3 - 1)
d1 <- diff(fx) / diff(x2)
d2 <- diff(d1) / diff((x2[-1] + x2[-length(x2)])/2)
d2[1:5]
# For a cubic, second difference grows roughly linearly; positive indicates convexity.
