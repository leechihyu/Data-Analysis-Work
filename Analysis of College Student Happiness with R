library('lme4')
library('lmerTest')
library('onewaytests')
library('DescTools')
library('MASS')
#import files
happiness = read.csv("C:/Users/24729/Desktop/Academic/college_happiness/happiness.csv",
                      encoding = 'UTF-8')
happyrank = read.csv("C:/Users/24729/Desktop/Academic/college_happiness/happyrank.csv",
                     encoding = 'UTF-8')
#inspect the first rows of the data set
head(happiness)
#conduct an anova with 区域 as treatment
model_aov1 = aov(综合满意度~区域, data = happiness)
summary(model_aov1)
#conduct Bartlett test to test whether the assumption of homogeneous variance
bartlett.test(综合满意度~区域, data = happiness)
#post hoc, multiple comparison with Scheffe method
ScheffeTest(综合满意度~城市级别, data = happiness) 

#conduct an anova with 城市级别 as treatment
model_aov2 = aov(综合满意度~城市级别, data = happiness)
summary(model_aov2)
#conduct Bartlett test to test whether the assumption of homogeneous variance
bartlett.test(综合满意度~城市级别, data = happiness)
#anova model failed to pass Bartlett test, so we choose Welch test instead
welch.test(综合满意度~城市级别, data = happiness, na.rm = F, verbose = T, rate = 0, alpha = 0.05)
#define a function to do post hoc in Games Howell's method
games.howell <- function(grp, obs) {
  
  #Create combinations
  combs <- combn(unique(grp), 2)
  
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
    
    #t-values
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) + (std[combs[2,x]] / n[combs[2,x]]))
    
    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 3)
  
  # Rename data frame columns
  colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit', 'lower limit')
  
  return(results)
}

#post hoc in Games Howell's method
games.howell(happyrank$城市级别, happyrank$综合满意度)

# relevel 区域, making 华东 as the reference
happyrank$区域= relevel(as.factor(happyrank$区域), ref = '华东')
# relevel 城市级别, making 新一线城市 as the reference
happyrank$城市级别 = relevel(as.factor(happyrank$城市级别), ref = "新一线城市")
# robust linear regession model
modellinear=rlm(生活满意度~高考录取线+一流学科建设高校+城市级别+区域, data = happyrank)
summary(modellinear)
confint.default(object = modellinear, level = 0.95)
