### Useful Code Snippets to Save
### by Yarrow Dunham yarrow.dunham@yale.edu

#### Data Wrangling ####

## reorder a factor 

# by a numeric variable
df$orderedFactor <- reorder(df$unorderedFactor,df$numericVar)

# within the factor itself by setting the reference level
df$var <- relevel(df$var, ref="baseline")


# Clean Qualtrics import, stripping off same row
df <- read_csv('filename')  # import
df <- df[2:nrow(df),]  # strip off 2nd row
#convert numeric columns to numeric (end up as chracter because of the Qualtrics 2nd row)
cols = 10:20   # edit to range of columns you want to be numeric
df[,cols] = apply(df[,cols], 2, function(x) as.numeric(as.character(x)))


#### Producing all interactions ####
data <- data.frame(matrix(rnorm(9 * 10), ncol = 9))
names(data) <- c(paste0("x", 1:8), "y")
# First step: using .*. for all interactions
f <- as.formula(y ~ .*.)
y <- data$y
# Second step: using model.matrix to take advantage of f
x <- model.matrix(f, data)[, -1]


#### Multilevel Modeling ####

# Non-liner model optimizers to add as options to glmer
control=glmerControl(optimizer="bobyqa")
# or using optimx package
control = glmerControl(optimizer = "optimx", calc.derivs = F,
                       optCtrl = list(method = "nlminb"))

#### Plotting with ggplot et al ####

# using the stat_summary functions for points, lines, and bootstrapped error bars
ggplot(use,aes(x=bin,y=prop,color=speaker)) +
  stat_summary(fun.y=mean, geom="point", size=4) + 
  stat_summary(fun.y=mean, geom="line", size=2) +
  stat_summary(fun.y=mean, geom="bar", size=2) +
  stat_summary(fun.data="mean_cl_boot", geom="linerange") 

# geom_smooth with logistic fit:
geom_smooth(method = "glm", method.args = list(family = "binomial"))

# reorder by a numeric variable within ggplot aesthetic
# in this case reordering the x variable by value on var_to_order
aes(x=fct_reorder(x_var,var_to_order))

# reorder a factor within ggplot aesthetic, specifying the factor levels in order you want them
aes(x=fct_relevel(x_var,var_to_order,"first level","second level","third level"))

#### Other data wrangling ####

#substr from right, grabs n characters starting count from right 
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# extract numbers from a string; use w/ substr to get numbers from just one part of a string
as.numeric(gsub("[^\\d]+", "", df, perl=TRUE))


#### Other useful mini functions ####

# standard error of a proportion
se.prop <- function(x) {
  sqrt( (mean(x) * (1- mean(x))) / length(x)) }

## standard error of the mean
sem <- function (x) {
  sd(x,na.rm=TRUE) / sqrt(length(x))
}


## Logits to probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


#### Regularized Regression and Related ####


## using the causal tree package

df <- read.csv("data.csv", stringsAsFactors=FALSE)

tree <- causalTree(scale(dv) ~ as.factor(iv1) + as.factor(iv2) + iv3 + iv4, 
                   data = df, treatment =df$condition,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, xval = 100, cp = 0, minsize = 50, propensity = 0.5)
# you can add minbucket=X to set the minimum sample size in each final bucket

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
opfit <- prune(tree, opcp)
rpart.plot(opfit)


#### Packages to Explore ####

library(jmv)  
# adds many features to common functions such as correlations and linear regression
# has some nice-looking ways to make tables of descriptives and correlations, PCA, etc

library(GGally)
# ggplot2 extension; nice graphs for plotting regression coefficients and diagnostics, making network plots,
# correlation grids and pair plots,etc

library(ggnet2)  # for network plots in ggplot2 framework! uses igraph objects as input!

library(sjPlot)  # regression and other visualizations including for mixed models

library(tidyboot)  # convenience functions for bootstrapping many statistics

library(RSienna)  # various network functions include stochastimc agent-based modeling

library(skimr)  # useful and tidy summary stats

library(infer)   # easy to use permutation tests in the tidyverse  http://infer.netlify.com/index.html