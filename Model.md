
```r
income_bach_model <- lm(Median_Household_Income_2019~Percent_Bachelors,data=inc_edu_county)
summary(income_bach_model)
```

```
## 
## Call:
## lm(formula = Median_Household_Income_2019 ~ Percent_Bachelors, 
##     data = inc_edu_county)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -39801  -6408   -618   5773  56468 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       32019.20     454.23   70.49   <2e-16 ***
## Percent_Bachelors  1078.28      18.95   56.90   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10170 on 3139 degrees of freedom
##   (3 observations deleted due to missingness)
## Multiple R-squared:  0.5077,	Adjusted R-squared:  0.5076 
## F-statistic:  3238 on 1 and 3139 DF,  p-value: < 2.2e-16
```


```r
income_no_diploma_model <- lm(Median_Household_Income_2019~Percent_No_Diploma,data=inc_edu_county)
summary(income_no_diploma_model)
```

```
## 
## Call:
## lm(formula = Median_Household_Income_2019 ~ Percent_No_Diploma, 
##     data = inc_edu_county)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -27915  -7802  -2544   4779  87778 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        71367.01     511.23  139.60   <2e-16 ***
## Percent_No_Diploma -1199.23      35.31  -33.96   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12390 on 3139 degrees of freedom
##   (3 observations deleted due to missingness)
## Multiple R-squared:  0.2687,	Adjusted R-squared:  0.2685 
## F-statistic:  1153 on 1 and 3139 DF,  p-value: < 2.2e-16
```


```r
percent_bach_no_diploma_model <- lm(Percent_No_Diploma~Percent_Bachelors,data=inc_edu_county)
summary(percent_bach_no_diploma_model)
```

```
## 
## Call:
## lm(formula = Percent_No_Diploma ~ Percent_Bachelors, data = inc_edu_county)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.305  -3.563  -0.586   2.705  52.354 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       21.614064   0.224779   96.16   <2e-16 ***
## Percent_Bachelors -0.389598   0.009378  -41.55   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.032 on 3139 degrees of freedom
##   (3 observations deleted due to missingness)
## Multiple R-squared:  0.3548,	Adjusted R-squared:  0.3546 
## F-statistic:  1726 on 1 and 3139 DF,  p-value: < 2.2e-16
```


```r
percent_bach_no_diploma_model <- lm(Percent_No_Diploma~Percent_Bachelors,data=inc_edu_county)
summary(percent_bach_no_diploma_model)
```

```
## 
## Call:
## lm(formula = Percent_No_Diploma ~ Percent_Bachelors, data = inc_edu_county)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.305  -3.563  -0.586   2.705  52.354 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       21.614064   0.224779   96.16   <2e-16 ***
## Percent_Bachelors -0.389598   0.009378  -41.55   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.032 on 3139 degrees of freedom
##   (3 observations deleted due to missingness)
## Multiple R-squared:  0.3548,	Adjusted R-squared:  0.3546 
## F-statistic:  1726 on 1 and 3139 DF,  p-value: < 2.2e-16
```
