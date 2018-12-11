Post2
================
Grace Puryear
December 9, 2018

Read in data
------------

``` r
#install.packages("readxl")
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 3.4.4

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.4.4

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.4.4

``` r
library(e1071)
```

    ## Warning: package 'e1071' was built under R version 3.4.4

``` r
user_knowledge <- as.data.frame(read_excel("User_Knowledge_test.xls",sheet="Training_Data",range="A1:F259"))
head(user_knowledge)
```

    ##    STG  SCG  STR  LPR  PEG      UNS
    ## 1 0.00 0.00 0.00 0.00 0.00 very_low
    ## 2 0.08 0.08 0.10 0.24 0.90     High
    ## 3 0.06 0.06 0.05 0.25 0.33      Low
    ## 4 0.10 0.10 0.15 0.65 0.30   Middle
    ## 5 0.08 0.08 0.08 0.98 0.24      Low
    ## 6 0.09 0.15 0.40 0.10 0.66   Middle

``` r
# STG (The degree of study time for goal object materials), (input value)
# SCG (The degree of repetition number of user for goal object materials) (input value)
# STR (The degree of study time of user for related objects with goal object) (input value)
# LPR (The exam performance of user for related objects with goal object) (input value)
# PEG (The exam performance of user for goal objects) (input value)
# UNS (The knowledge level of user) (target value)
# Very Low: 24
# Low: 83
# Middle: 88
# High 63
```

***Clustering Method***

``` r
user_knowledge[,6] <- as.factor(user_knowledge[,6])
plot(user_knowledge[,1:5],
     pch=c("o","+","*","^")[user_knowledge$UNS],
     col=c("red","green3","blue","orange")[user_knowledge$UNS])
```

![](Post2_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#There appears to be a clear distinction between the 4 groups.
```

``` r
High <- user_knowledge[which(user_knowledge$UNS=="High"),]
Low <- user_knowledge[which(user_knowledge$UNS=="Low"),]
Middle <- user_knowledge[which(user_knowledge$UNS=="Middle"),]
vlow <- user_knowledge[which(user_knowledge$UNS=="very_low"),]


col_means <- as.data.frame(rbind(colMeans(High[,1:5]),colMeans(Middle[,1:5]),colMeans(Low[,1:5]),colMeans(vlow[,1:5])))
row.names(col_means)[1] <- "High"
row.names(col_means)[2] <- "Middle"
row.names(col_means)[3] <- "Low"
row.names(col_means)[4] <- "Very Low"
col_means
```

    ##                STG       SCG       STR       LPR        PEG
    ## High     0.4216508 0.4231905 0.5016667 0.5012698 0.77253968
    ## Middle   0.3999773 0.3679205 0.5068182 0.3428409 0.54238636
    ## Low      0.3211446 0.3370000 0.4307229 0.4973494 0.23762651
    ## Very Low 0.3057917 0.1981250 0.3662500 0.3587500 0.09083333

``` r
#It appears that the high knowledge users have (almost) the highest averages of each variable across the board.The only one that the high knowledge users slightly trail in is 'STR', or the degree of study time of user for related objects with goal object.The middle knowledge users contain the second highest value in each column, except fot the 'LPR' variable, or the exam performance of user for related objects with goal object. 
```

K-Medoids
---------

``` r
library(cluster)

# group into 3 clusters
pam.result <- pam(user_knowledge[,1:5], 4)

table(pam.result$clustering, user_knowledge$UNS)
```

    ##    
    ##     High Low Middle very_low
    ##   1   30   9     30        3
    ##   2    2  42      8       17
    ##   3   13  13     10        1
    ##   4   18  19     40        3

``` r
par(mar=c(4,4,1,1))
plot(pam.result)
```

![](Post2_files/figure-markdown_github/unnamed-chunk-3-1.png)![](Post2_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
#It appears there is a lot of overlap in the 4 target values. 
```

determining number of clusters
------------------------------

``` r
library(fpc)
```

    ## Warning: package 'fpc' was built under R version 3.4.4

``` r
set.seed(1);pamk.result <- pamk(user_knowledge[,1:5])

# check clustering against actual user knowledge
table(pamk.result$pamobject$clustering, user_knowledge$UNS)
```

    ##    
    ##     High Low Middle very_low
    ##   1   11  66     21       22
    ##   2   52  17     67        2

``` r
par(mar=c(4,4,1,1))
plot(pamk.result$pamobject)
```

![](Post2_files/figure-markdown_github/unnamed-chunk-4-1.png)![](Post2_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
pamk.result$pamobject$medoids
```

    ##       STG  SCG  STR  LPR  PEG
    ## [1,] 0.35 0.38 0.32 0.60 0.16
    ## [2,] 0.29 0.30 0.56 0.25 0.67

``` r
#pamk tells us that 2 clusters is best here. However, there is still no clear distinction. It appears that cluster 1 is the low and very low knowledge users. Cluster 2 is the middle/high knowledge users.
#Cluster 1 has a higher average degree of study time, higer average degree of repetition for goal object materials, and higher average exam performance for related objects with goal object. Cluster 2 has higher average degree of study time for related objects with goal object and a higher exam performance for goal objects.
```

***Doing a classification based approach***

``` r
user_test <- as.data.frame(read_excel("User_Knowledge_test.xls",sheet="Test_Data",range="A1:F146"))
user_test$UNS <- as.factor(user_test$UNS)


# Train a naive Bayes model (based on the training set)
fit_nb <- naiveBayes(UNS ~ ., user_knowledge)

# Check performance (based on the test set)
table(predict(fit_nb, user_test), user_test$UNS) # 122 out of 145 (84%) correct
```

    ##           
    ##            High Low Middle Very Low
    ##   High       39   0      0        0
    ##   Low         0  42      9       10
    ##   Middle      0   4     25        0
    ##   very_low    0   0      0       16
