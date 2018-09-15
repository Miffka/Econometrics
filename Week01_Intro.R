setwd("/media/glycosylase/EC6A2F256A2EEBD0/Users/miffka/Documents/!DataMining/Econometrics")
library(dplyr)


# Week 1

# Test

# Task 14
data(sleep)
sleep[8,1]

# Task 15
mean(sleep$extra)^2

# Task 16
max(sleep$extra)+min(sleep$extra)

# Task 17
round(var(sleep$extra[1:10]), 1)

# Task 18
data(mtcars)
summary(lm(mpg ~ disp + hp + wt, mtcars))

# Task 19
lm(mpg ~ disp + hp + wt + am, mtcars)

# Task 20
?aov
anova(lm(mpg ~ disp + hp + wt, mtcars),
      lm(mpg ~ cyl + hp + wt, mtcars),
      lm(mpg ~ disp + cyl + wt, mtcars),
      lm(mpg ~ disp + hp + cyl, mtcars))

# Task 20 (variant 2)
anova(lm(mpg ~ hp + wt + am, mtcars),
      lm(mpg ~ hp + wt + cyl, mtcars),
      lm(mpg ~ cyl + wt + am, mtcars),
      lm(mpg ~ hp + cyl + am, mtcars))

# Task 14 (variant 3)
sleep[6,1]

# Task 15 (variant 3)
mean(sleep$extra)^3

# Task 16 (variant 3)
max(sleep$extra) - min(sleep$extra)

# Task 17 (variant 3)
round(var(sleep$extra[1:10]), 1)

# Task 18 (variant 3)
summary(lm(mpg ~ disp + hp + wt, mtcars))

# Task 19 (variant 3)
lm(mpg ~ disp + hp + wt + am, mtcars)

# Task 20 (variant 3)
anova(lm(mpg ~ hp + wt + am, mtcars),
      lm(mpg ~ hp + wt + cyl, mtcars),
      lm(mpg ~ cyl + wt + am, mtcars),
      lm(mpg ~ hp + cyl + am, mtcars))
