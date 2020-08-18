# Get s20x library
if(!require(s20x)) {install.packages(s20x)}
help(package="s20x")

# Get readxl library
if(!require(readxl)) {install.packages(readxl)}
help(package="readxl")

#Use readxl function to read in data
MOBILE <- read_excel("E:/MATH4773-5773/DATA/Excel/MOBILE.xls")
# You could also use MOBILE = read_excel(file.choose())
View(MOBILE)
head(MOBILE)

# Get frequency for each category
f = MOBILE$NUMBER
# Get the style name category
nf = MOBILE$STYLE

nf

# put category names on the frequencies
m = rep(nf, f)
m
# make a table 
m.tab=table(m)
m.tab
class(m.tab)


# Make a barplot
dev.new(noRStudioGD = TRUE)
barplot(m.tab)
?barplot
# or use ggplot
library(ggplot2)
m.df=as.data.frame(m)
m.df
ggplot(m.df, aes(x=m, fill = m))+geom_bar()
?labs

# Make a window to plot in
dev.new(noRStudioGD = TRUE)
freq1way(f,hypothprob = rep(1/6,6), conf.level = 0.90)

# make an object to contain output
ff = freq1way(f,hypothprob = rep(1/6,6), conf.level = 0.90)
ff
names(ff)

?freq1way

# Obtain expected values and chi sq values
ff$exp
ff$chi

(f-ff$exp)^2/ff$exp

x2=sum((f-ff$exp)^2/ff$exp)
x2

dev.new()
curve(dchisq(x,5), xlim=c(0,20))

# Or try the exotic ggplot package
if(!require(ggplot2)){install.packages(ggplot2)}

p <- ggplot(NULL, aes(x = c(0,20))) + 
  stat_function(fun = dchisq, args = list(df = 5))+
  geom_area(stat="function",fun = dchisq, args=list(df=5),fill="red", alpha=0.5,xlim = c(qchisq(1-0.05,5),20)) + 
  geom_segment(aes(x=qchisq(1-0.05,5),y=0,xend=20,yend=0),col ="Green",alpha=0.8, size = 0.5) +
  xlab(expression(chi^2)) +
  ylab("density") +
  labs(title = paste("chi^2", "distribution showing rejection region")) +
  annotate("text", x = 12.5, y = 0.005,  label = "paste(alpha, \" = .05\")", parse = TRUE)
p
 
# The freq1way function 
# Change this to make the output plots as requested in the lab

freq1way = function (counts, hypothprob, conf.level = 0.95, addCIs = TRUE, 
          digits = 4, arrowwid = 0.1, estimated = 0) 
{
  varname = deparse(substitute(counts))
  if (length(dim(counts)) > 1) 
    stop(paste("freq1way: Dimension of", varname, "greater than 1"))
  if (as.integer(estimated) != estimated) 
    stop("freq1way: estimated must be an integer")
  dfs = length(counts) - 1
  if ((estimated < 0) | (estimated > dfs)) 
    stop(paste("freq1way: estimated must be between 0 and", 
               dfs))
  n = sum(as.vector(counts))
  cat("data: ", varname, "   n =", n, "\\\\n\\\\n")
  ncats = length(counts)
  ncatsC2 = choose(ncats, 2)
  if ((any(counts != trunc(counts))) | (n < max(30, 5 * ncats))) 
    warning("Expecting a vector of counts")
  if (is.null(names(counts))) 
    names(counts) = 1:ncats
  conf.pc = 100 * conf.level
  phat = counts/n
  qval = abs(qnorm((1 - conf.level)/(2 * ncats)))
  se = sqrt(phat * (1 - phat)/n)
  CIs = matrix(c(phat, phat - qval * se, phat + qval * se), 
               ncol = 3, dimnames = list(names(counts), c("sample prop", 
                                                          "conf.lower", "conf.upper")))
  if (!missing(hypothprob)) {
    if (length(hypothprob) != ncats) 
      stop("counts and hypothprob must have same length")
    CIs = cbind(CIs, hypothprob)
    colnames(CIs)[4] = "hypoth prob"
  }
  cat("Individual (large sample)", paste(conf.pc, "%", sep = ""), 
      "CIs", "\\\\n", "(adjusted for", ncats, "multiple comparisons)", 
      "\\\\n")
  print(round(CIs, 3), quote = FALSE)
  cat("\\\\n")
  if (missing(hypothprob)) {
    chitest = chisq.test(counts, p = rep(1, ncats)/ncats)
    chitest$p.value = 1 - pchisq(chitest$statistic, dfs - 
                                   estimated)
    cat("Chi-square test for uniformity", "\\\\n    ")
  }
  else {
    chitest = chisq.test(counts, p = hypothprob)
    chitest$p.value = 1 - pchisq(chitest$statistic, dfs - 
                                   estimated)
    names(hypothprob) = names(counts)
    cat("Chi-square test for hypothesized probabilities", 
        "\\\\n    ")
  }
  cat(names(chitest$statistic), " = ", format(round(chitest$statistic, 
                                                    4)), ", ", sep = "")
  cat(paste(names(chitest$parameter), " = ", format(round(chitest$parameter - 
                                                            estimated, 3)), ",", sep = ""), "")
  cat("p-value =", format.pval(chitest$p.value, digits = digits), 
      "\\\\n")
  if (any(chitest$exp < 5)) 
    warning("Chi-square approximation may be incorrect")
  cat("\\\\n")
  uplim = ifelse(addCIs, max(phat + qval * se), max(phat))
  disp = 0
  modlength = 1
  if (missing(hypothprob)) {
    midp = barplot(phat, ylab = "Proportion", main = "Proportions at each level", 
                   sub = paste("[freq1way(", varname, ")]"), ylim = c(0, 
                                                                      uplim))
    if (addCIs) 
      abline(h = 1/ncats, lty = 2)
  }
  else {
    midp = barplot(rbind(phat, hypothprob), beside = TRUE, 
                   ylab = "Proportion", main = "Proportions at each level", 
                   sub = paste("[freq1way(", varname, ")]"), ylim = c(0, 
                                                                      uplim), legend = c("sample", "hypothesis"))[1, 
                                                                                                                  ]
    disp = 0
    modlength = 2
  }
  if (addCIs) 
    for (i in 1:length(midp)) arrows(midp[i] - disp, phat[i] - 
                                       qval * se[i], midp[i] - disp, phat[i] + qval * se[i], 
                                     code = 3, angle = 45, length = 0.9 * arrowwid/modlength)
  if (missing(hypothprob)) {
    matw = matrix(NA, ncats - 1, ncats - 1)
    namew = names(phat)
    dimnames(matw) = list(namew[-length(namew)], namew[-1])
    for (i1 in 1:(ncats - 1)) {
      for (i2 in 2:ncats) {
        tempw = phat[i1] - phat[i2] + abs(qnorm((1 - 
                                                   conf.level)/(2 * ncatsC2))) * c(-1, 1) * sqrt(((phat[i1] + 
                                                                                                     phat[i2]) - ((phat[i1] - phat[i2])^2))/n)
        tempw = round(tempw, 3)
        matw[i1, i2 - 1] = ifelse((i1 < i2), paste("(", 
                                                   tempw[1], ",", tempw[2], ")", sep = ""), " ")
        if ((0 <= tempw[1] | 0 >= tempw[2]) & (i1 < i2)) 
          matw[i1, i2 - 1] = paste(matw[i1, i2 - 1], 
                                   "*", sep = "")
      }
    }
    cat(paste(conf.pc, "%", sep = ""), "CIs for differences in true proportions (rowname-colname)", 
        "\\\\n", "(adjusted for", ncatsC2, "multiple comparisons)", 
        "\\\\n")
    print(matw, quote = FALSE)
  }
  invisible(list(CIs = CIs[, 1:3], exp = chitest$exp, chi = (counts - 
                                                               chitest$exp)^2/chitest$exp))
}






