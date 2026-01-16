# MATH163 – Week 01: Getting started in R
# INTRODUCTION: We'll practise the console, scripts, vectors, simple plots, and a tiny function.
# WORKED EXAMPLE shows each step; TRY IT YOURSELF repeats ideas with different inputs.

#### Week 1 — First steps: your course folder & working directory ####

# 1) Make a folder for this course on your M: drive (university network storage):
#    e.g.  M:\MATH163\practicals
#    Put ALL the practical materials (data files + scripts) in that folder.

# 2) Tell R where that folder is (your "working directory").
#    Use forward slashes on Windows, or double backslashes if you prefer.
#    Replace the path below with your OWN M: path.
# setwd("M:/MATH163/practicals")
# setwd("M:\\MATH163\\practicals")  # alternative Windows form

#    Tip (RStudio menu): Session > Set Working Directory > Choose Directory...
#    Then check with:
# getwd()

# 3) We don’t need any packages this week. Base R is enough.
#    Next week we’ll use a package and explain install vs load vs paths in detail.

#### Week 1 — Getting started with packages (explicit + simple) ####

# You install a package ONCE per computer, using install.packages("<name>").
# You load a package in EACH new R session using library(<name>).

# For Week 1 we don't actually need any packages to run the code below.
# (Base R is enough.) We'll use a package called 'here' in Week 2.
# If you want to get ahead on your own laptop, you can install it now by
# removing the leading # from the next line and running it once:

# install.packages("here")

# We are NOT loading any packages in Week 1:
# library(here)   # <— not needed today

# Tip: If you ever see “there is no package called …”, it means you need to
# run install.packages("<name>") once on this machine.

# Tip: Use this R script as your “lab book”. Write comments to explain what each block does.


# --- WORKED EXAMPLE (WITH ANSWERS INLINE) ------------------------------------
# Goal: practise basic objects (vectors), plotting, tiny functions, and the idea of
#       finite differences (approximate derivatives).

# 1) PLOT THE FUNCTION y = cos(x) OVER 0 ≤ x ≤ 2π
#    a) Make a vector of evenly spaced x-values that covers the interval [0, 2π].
#    b) Make a matching vector y = cos(x), one value per x.
#    c) Plot points (x, y). Label axes and add a clear title so the plot is self-explanatory.
#       Note: seq(from, to, by) makes sequences; pi is built into R.
x <- seq(0, 2*pi, by = 0.01*pi)          # a grid of x values every 0.01π radians
y <- cos(x)                               # the corresponding y values
plot(x, y,
     main = "y = cos(x) on [0, 2π]",
     xlab = "x (radians)",
     ylab = "cos(x)",
     type = "l")                          # type = "l" draws a continuous line

# 2) DEFINE AND CALL A SIMPLE FUNCTION
#    We’ll define f(u) = u^2 + 1. Why? To show how *functions* wrap up a calculation
#    so you can reuse it for different inputs (a single number OR a whole vector).
quad_plus_one <- function(u) {
  # Everything you return must be computed from 'u'
  u^2 + 1
}
quad_plus_one(7)                          # evaluate at a single number (returns 50)
value_8 <- quad_plus_one(8)               # store the result (useful if needed later)

# 3) DISCRETE DIFFERENCES: WHAT DOES diff() DO?
#    Build a simple vector x2 = 1, 3, 5, ..., 19 (step size 2).
#    diff(x2) computes consecutive differences: x2[i+1] - x2[i].
#    Key point: diff() reduces length by 1, because there are N-1 gaps between N points.
x2 <- seq(1, 20, by = 2)                  # 1, 3, 5, ..., 19
dx <- diff(x2)                            # should be a vector of 2s
length(x2)                                # 10
length(dx)                                # 9  (one fewer)

# 4) APPROXIMATE A DERIVATIVE USING FINITE DIFFERENCES
#    We’ll approximate f’(x) when f(x) = x^3 - 1 using forward differences on x2.
#    Steps:
#      a) Define f(u) = u^3 - 1.
#      b) Evaluate f on our grid x2 to get a vector fx (same length as x2).
#      c) Form forward differences: Δf / Δx = diff(fx) / diff(x2).
#      d) Those slopes live *between* the original x points; a natural x-position
#         for plotting them is the midpoints of consecutive x’s.
f <- function(u) u^3 - 1
fx <- f(x2)
dydx_approx <- diff(fx) / diff(x2)        # finite-difference slopes
mid <- (x2[-1] + x2[-length(x2)]) / 2     # midpoints between consecutive x’s

#    e) Plot the approximate derivative against the midpoints.
plot(mid, dydx_approx,
     type = "l",
     main = "Approximate derivative of f(x) = x^3 - 1 (finite differences)",
     xlab = "x (midpoints of the grid)",
     ylab = "approximate f'(x)")

# 5) COMPARE TO THE ANALYTIC DERIVATIVE
#    For f(x) = x^3 - 1, the exact derivative is f’(x) = 3x^2.
#    To compare apples-to-apples, evaluate the analytic derivative AT THE MIDPOINTS,
#    because that’s where our approximate slopes live.
analytic_at_mid <- 3 * mid^2
lines(mid, analytic_at_mid, col = "blue", lwd = 2)
#    Visual reading: the blue curve is the exact derivative; the black curve is the
#    finite-difference approximation. With a coarse grid (step = 2) they’re close
#    but not identical; using a finer grid would make them closer.

# 6) QUICK “INTROSPECTION” ON A VECTOR (COMMON SUMMARY/UTILITY CALLS)
#    Build a small vector and ask typical questions: minimum, mean, median, etc.
#    Note: sort(..., decreasing = TRUE) puts largest first. exists("v") checks if a
#    named object is present in your workspace.
v <- c(3, 5, 2, 3, 1, 4)
min(v)                      # 1
mean(v)                     # 3
median(v)                   # 3
sum(v)                      # 18
range(v)                    # 1 5
str(v)                      # structure; confirms it's numeric with length 6
length(v)                   # 6
sort(v, decreasing = TRUE)  # 5 4 3 3 2 1
print(exists("v"))          # TRUE


# --- TRY IT YOURSELF (NO ANSWERS HERE) ---------------------------------------
# [1-TY-1] Plot sin(x) and cos(x) on the same axes over [0, 4π]. Add a legend.
# [1-TY-2] Write a function g(u) = u^2 + u + 1; compute g(5) and g(c(1,2,3)).
# [1-TY-3] For f(u) = exp(-u^2/2)/sqrt(2π), approximate ∫ f(u) du over [-5,5] using a fine grid
#          (Riemann sum). Then try a finer grid and compare.
# [1-TY-4] Using x2 from above, compute second differences of f(x2) and comment on the sign.
