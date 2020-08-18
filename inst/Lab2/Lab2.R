# Lab 2 MATH 4773/5773
# Contingency tables

library("vcdExtra")
library("s20x")

# learn about Arrays
# These will assist you in making larger dimension tables

dms = c(2,4,2)
dms
arrayA <- array(1:16, dim = dms)
arrayA

str(arrayA)

arrayB = array(1:16, dim = c(2,8))
arrayB

str(arrayB)


# We can add names to the array
# Use a list

dimnames(arrayA) = list(sex = c("M", "F"), group = letters[1:4], time = c("Pre", "Post"))

arrayA

str(arrayA)

# tables
# Collapsing tables

set.seed(12345)
sex = c("Male", "Female")
age = c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69")
education = c("low", "med", "high")
dat = expand.grid(sex = sex, age = age, education = education)
dat
counts = rpois(36,100)
dat = cbind(dat,counts)
dat
tab1 = xtabs(counts ~ sex + age + education, data = dat)
tab1
structable(tab1)


# Now collapse table to 3 age levels, education to 2 levels

tab2 = collapse.table(tab1, age = c("10-29", "10-29", "30-49", "30-49", "50-69", "50-69"),education = c("<high","<high", "high")  )
tab2
structable(tab2)

# read in excel sheets
# 
library(readxl)
mile=read_excel(file.choose())
mile

library(vcdExtra)
tab = xtabs(NUMBER ~ DISTANCE + EVAC, data = mile)
dev.new(noRStudioGD = TRUE)

# make a mosaic plot
mosaic(tab, data = mile, shade = TRUE)

library(s20x)

tt = rowdistr(tab, comp = "between")

summary(tt)

chisq.test(tab)

# Example of a function
myfun = function(x) 
{
  # if ggplot2 is not installed then install it!
  if(!require(ggplot2)) install.packages(ggplot2)
  
  y  = x^2
  df = data.frame(x=x,y=y)
  dev.new(noRStudioGD = TRUE)
  #plot(y~x, type="l", main = "Wayne's quadratic")
  
  g = ggplot(df, aes(x=x,y=y, col = y)) + geom_point(size = 2) + geom_smooth()
  print(g)
  out = list(x=x,y=y, df = df)
  return(out)
  
  
}

myfun(1:10)
body(myfun)




