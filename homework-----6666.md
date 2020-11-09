homework6
================
Yamei Li, Meirou Guan, Tamires Amorim, Carol-Ann Jackson
11/9/2020

``` r
setwd("/Users/yameili/Desktop/ecob2000_lecture1/acs2017_ny")
load("acs2017_ny_data.Rdata")
attach(acs2017_ny)
```

``` r
#convert the LABFORCE and MARST from numeric to factor
str(LABFORCE)
```

    ##  'haven_labelled' int [1:196585] 2 2 2 2 2 1 1 2 1 2 ...
    ##  - attr(*, "label")= chr "Labor force status"
    ##  - attr(*, "labels")= Named num [1:3] 0 1 2
    ##   ..- attr(*, "names")= chr [1:3] "N/A" "No, not in the labor force" "Yes, in the labor force"

``` r
str(MARST)
```

    ##  'haven_labelled' int [1:196585] 1 1 6 6 1 1 5 1 1 6 ...
    ##  - attr(*, "label")= chr "Marital status"
    ##  - attr(*, "labels")= Named num [1:6] 1 2 3 4 5 6
    ##   ..- attr(*, "names")= chr [1:6] "Married, spouse present" "Married, spouse absent" "Separated" "Divorced" ...

``` r
acs2017_ny$LABFORCE <- as.factor(acs2017_ny$LABFORCE)
levels(acs2017_ny$LABFORCE) <- c("NA","Not in LF","in LF")

acs2017_ny$MARST <- as.factor(acs2017_ny$MARST)
levels(acs2017_ny$MARST) <- c("married spouse present","married spouse absent","separated","divorced","widowed","never married")
```

``` r
str(LABFORCE)
```

    ##  'haven_labelled' int [1:196585] 2 2 2 2 2 1 1 2 1 2 ...
    ##  - attr(*, "label")= chr "Labor force status"
    ##  - attr(*, "labels")= Named num [1:3] 0 1 2
    ##   ..- attr(*, "names")= chr [1:3] "N/A" "No, not in the labor force" "Yes, in the labor force"

``` r
str(MARST)
```

    ##  'haven_labelled' int [1:196585] 1 1 6 6 1 1 5 1 1 6 ...
    ##  - attr(*, "label")= chr "Marital status"
    ##  - attr(*, "labels")= Named num [1:6] 1 2 3 4 5 6
    ##   ..- attr(*, "names")= chr [1:6] "Married, spouse present" "Married, spouse absent" "Separated" "Divorced" ...

``` r
#cut age into different range
acs2017_ny$age_bands <- cut(acs2017_ny$AGE,breaks=c(0,25,35,45,55,65,100))
table(acs2017_ny$age_bands,acs2017_ny$LABFORCE)
```

    ##           
    ##               NA Not in LF in LF
    ##   (0,25]   31680     11717 13256
    ##   (25,35]      0      4271 20523
    ##   (35,45]      0      4064 18924
    ##   (45,55]      0      5406 21747
    ##   (55,65]      0     10563 18106
    ##   (65,100]     0     28701  5880

NA means them maybe too younger to have a job,because the NA exist in
(0,25\].

``` r
pick_use1 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 55)
dat_use1 <- subset(acs2017_ny, pick_use1)

dat_use1$LABFORCE <- droplevels(dat_use1$LABFORCE)
str(dat_use1$LABFORCE)
```

    ##  Factor w/ 2 levels "Not in LF","in LF": 2 2 2 1 2 2 1 2 2 2 ...

``` r
# add mortgage into the binomial logitistic regression model
model_logit1 <- glm(LABFORCE ~ AGE + I(AGE^2) + female + MORTGAGE+ AfAm + Asian + race_oth + Hispanic 
            + educ_hs + educ_somecoll + educ_college + educ_advdeg 
            + MARST,
            family = binomial, data = dat_use1)
summary(model_logit1)
```

    ## 
    ## Call:
    ## glm(formula = LABFORCE ~ AGE + I(AGE^2) + female + MORTGAGE + 
    ##     AfAm + Asian + race_oth + Hispanic + educ_hs + educ_somecoll + 
    ##     educ_college + educ_advdeg + MARST, family = binomial, data = dat_use1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7809   0.3307   0.4797   0.6552   1.5818  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 0.4310677  0.2455356   1.756  0.07915 .  
    ## AGE                         0.0137316  0.0121619   1.129  0.25887    
    ## I(AGE^2)                   -0.0003092  0.0001478  -2.092  0.03645 *  
    ## female                     -0.6841893  0.0206115 -33.194  < 2e-16 ***
    ## MORTGAGE                    0.2029638  0.0079752  25.449  < 2e-16 ***
    ## AfAm                       -0.0915394  0.0286645  -3.193  0.00141 ** 
    ## Asian                      -0.0723147  0.0376156  -1.922  0.05455 .  
    ## race_oth                   -0.0215404  0.0333773  -0.645  0.51869    
    ## Hispanic                    0.2597131  0.0317466   8.181 2.82e-16 ***
    ## educ_hs                     0.8319838  0.0312683  26.608  < 2e-16 ***
    ## educ_somecoll               1.3539680  0.0354045  38.243  < 2e-16 ***
    ## educ_college                1.8486275  0.0374055  49.421  < 2e-16 ***
    ## educ_advdeg                 2.2583589  0.0440371  51.283  < 2e-16 ***
    ## MARSTmarried spouse absent -0.4069138  0.0521666  -7.800 6.18e-15 ***
    ## MARSTseparated              0.0285078  0.0583075   0.489  0.62490    
    ## MARSTdivorced               0.1711151  0.0380558   4.496 6.91e-06 ***
    ## MARSTwidowed               -0.2018454  0.0942819  -2.141  0.03228 *  
    ## MARSTnever married         -0.2509121  0.0246543 -10.177  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 71408  on 74934  degrees of freedom
    ## Residual deviance: 64183  on 74917  degrees of freedom
    ## AIC: 64219
    ## 
    ## Number of Fisher Scoring iterations: 5

dependent variable: in the laborforce and not in the laborforce,from the
result, we can see that female、
MORTGAGE、Hispanic、educ\_hs、educ\_somecoll、educ\_college、educ\_advdeg、MARSTmarried
spouse absent、MARSTdivorced 、MARSTnever married are all significantly.
some of them ar not significantly.

``` r
library(car)
```

    ## Loading required package: carData

``` r
vif(model_logit1)
```

    ##                     GVIF Df GVIF^(1/(2*Df))
    ## AGE           121.147401  1       11.006698
    ## I(AGE^2)      120.089694  1       10.958544
    ## female          1.039113  1        1.019369
    ## MORTGAGE        1.183055  1        1.087683
    ## AfAm            1.143078  1        1.069148
    ## Asian           1.323385  1        1.150385
    ## race_oth        1.528840  1        1.236463
    ## Hispanic        1.458075  1        1.207508
    ## educ_hs         2.355068  1        1.534623
    ## educ_somecoll   2.038651  1        1.427813
    ## educ_college    1.935280  1        1.391143
    ## educ_advdeg     1.589255  1        1.260657
    ## MARST           1.364010  5        1.031530

we can see except age and age^2, the GVIF of rest of them are less than
2. it means multicolinearity is small.

``` r
#HL test

library(generalhoslem)
```

    ## Loading required package: reshape

    ## Loading required package: MASS

``` r
logitgof(dat_use1$LABFORCE,fitted(model_logit1))
```

    ## 
    ##  Hosmer and Lemeshow test (binary model)
    ## 
    ## data:  dat_use1$LABFORCE, fitted(model_logit1)
    ## X-squared = 93.522, df = 8, p-value < 2.2e-16

``` r
or<-exp((summary(model_logit1))$coef[,'Estimate'])
or
```

    ##                (Intercept)                        AGE 
    ##                  1.5388998                  1.0138264 
    ##                   I(AGE^2)                     female 
    ##                  0.9996909                  0.5044991 
    ##                   MORTGAGE                       AfAm 
    ##                  1.2250281                  0.9125254 
    ##                      Asian                   race_oth 
    ##                  0.9302381                  0.9786899 
    ##                   Hispanic                    educ_hs 
    ##                  1.2965581                  2.2978727 
    ##              educ_somecoll               educ_college 
    ##                  3.8727620                  6.3510965 
    ##                educ_advdeg MARSTmarried spouse absent 
    ##                  9.5673751                  0.6657015 
    ##             MARSTseparated              MARSTdivorced 
    ##                  1.0289181                  1.1866274 
    ##               MARSTwidowed         MARSTnever married 
    ##                  0.8172213                  0.7780908

Through the HL test, we can see female are more likely not in the
laborforce, someone never married or spouse absent are also more likely
not in the laborforce, however, if someone have college or advance
college educational degree, they have huge probability in the
laborforce.

``` r
#Predict
set.seed(11111)
index<-sample(x=2,size=nrow(dat_use1),replace=TRUE,prob=c(0.7,0.3))
train<-dat_use1[index==1,]
test<-dat_use1[index==2,]
dim(dat_use1)
```

    ## [1] 74935   110

``` r
dim(train)
```

    ## [1] 52543   110

``` r
dim(test)
```

    ## [1] 22392   110

``` r
trainmodel<-glm(LABFORCE ~AGE + I(AGE^2) +female + MORTGAGE+ AfAm + Asian + Hispanic 
            + educ_hs + educ_somecoll + educ_college + educ_advdeg 
            + MARST,
            family = binomial, data = train)
prob<-predict(object=trainmodel,newdata=test,type="response")# use the trainmodel to predict
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1)) #Reclassify the predicted probability values
ta<-table(pred$LABFORCE,pred$predict) #Compare the actual and predicted values of the model
ta
```

    ##            
    ##                 0     1
    ##   Not in LF   319  3809
    ##   in LF       264 18000

``` r
sum_diag<-sum(diag(ta)) #the sum of the correct predict Numbers
sum<-sum(ta) #the sum of predict Numbers
sum_diag/sum #Prediction accuracy
```

    ## [1] 0.8181047

the prediction accuracy is 0.8181047.

``` r
#ROC CURVE
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
roc_curve<-roc(test$LABFORCE,prob)
```

    ## Setting levels: control = Not in LF, case = in LF

    ## Setting direction: controls < cases

``` r
x<-1-roc_curve$specificities
y<-roc_curve$sensitivities
plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),xlab="1-specificity",
ylab="sensitivity",main="ROC Curve",type="l",lwd=2)
abline(a=0,b=1,col="grey")
auc<-roc_curve$auc
text(0.5,0.4,paste("AUC",round(auc,digits=2)),col="blue")
```

![](homework-----6666_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
The AUC=0.72, less than 0.75, maybe the accuracy of this model is not
particularly good.
