Post 1: Breast Cancer Analysis
================
Grace Puryear
December 9, 2018

Read in data
------------

``` r
#setwd("~/BZAN552")
library(arules)
```

    ## Warning: package 'arules' was built under R version 3.4.4

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.4.4

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.4.4

``` r
library(RWeka)
```

    ## Warning: package 'RWeka' was built under R version 3.4.4

``` r
library(caret)
library(e1071)
```

    ## Warning: package 'e1071' was built under R version 3.4.4

``` r
data <- read.table("BreastCancerData.txt",sep=",",col.names = c("Class","Age","Menopause","Tumor-Size","inv-nodes","node-caps","deg-malig","breast","breast-quad","irradiat"))
data[,7] <- as.factor(data[,7])
```

Creating Rules that predict Class (Recurrence/no-recurrence)
------------------------------------------------------------

``` r
rules <- apriori(data,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.01, conf=.99), #minimum 1% of all observations
                 appearance = list(rhs=c("Class=no-recurrence-events",
                                         "Class=recurrence-events"),
                                   default="lhs"))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted[1:20])
```

    ##      lhs                        rhs                          support confidence     lift count
    ## [1]  {Menopause=premeno,                                                                      
    ##       inv.nodes=15-17}       => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [2]  {inv.nodes=9-11,                                                                         
    ##       irradiat=no}           => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [3]  {inv.nodes=6-8,                                                                          
    ##       breast.quad=right_low} => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [4]  {Age=30-39,                                                                              
    ##       Tumor.Size=35-39}      => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [5]  {node.caps=yes,                                                                          
    ##       breast.quad=right_low} => {Class=recurrence-events} 0.01748252          1 3.364706     5
    ## [6]  {Menopause=premeno,                                                                      
    ##       inv.nodes=15-17,                                                                        
    ##       node.caps=yes}         => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [7]  {Menopause=premeno,                                                                      
    ##       inv.nodes=15-17,                                                                        
    ##       deg.malig=3}           => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [8]  {Menopause=premeno,                                                                      
    ##       inv.nodes=15-17,                                                                        
    ##       irradiat=no}           => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [9]  {node.caps=?,                                                                            
    ##       deg.malig=1,                                                                            
    ##       breast=left}           => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [10] {inv.nodes=9-11,                                                                         
    ##       node.caps=yes,                                                                          
    ##       deg.malig=3}           => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [11] {inv.nodes=9-11,                                                                         
    ##       node.caps=yes,                                                                          
    ##       irradiat=no}           => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [12] {Menopause=premeno,                                                                      
    ##       inv.nodes=9-11,                                                                         
    ##       irradiat=no}           => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [13] {inv.nodes=6-8,                                                                          
    ##       node.caps=yes,                                                                          
    ##       breast.quad=right_low} => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [14] {inv.nodes=6-8,                                                                          
    ##       breast=left,                                                                            
    ##       breast.quad=right_low} => {Class=recurrence-events} 0.01048951          1 3.364706     3
    ## [15] {inv.nodes=6-8,                                                                          
    ##       deg.malig=3,                                                                            
    ##       irradiat=yes}          => {Class=recurrence-events} 0.02097902          1 3.364706     6
    ## [16] {Age=50-59,                                                                              
    ##       inv.nodes=6-8,                                                                          
    ##       irradiat=yes}          => {Class=recurrence-events} 0.01398601          1 3.364706     4
    ## [17] {Menopause=ge40,                                                                         
    ##       inv.nodes=6-8,                                                                          
    ##       irradiat=yes}          => {Class=recurrence-events} 0.01748252          1 3.364706     5
    ## [18] {inv.nodes=6-8,                                                                          
    ##       breast=left,                                                                            
    ##       irradiat=yes}          => {Class=recurrence-events} 0.02447552          1 3.364706     7
    ## [19] {Age=50-59,                                                                              
    ##       inv.nodes=6-8,                                                                          
    ##       deg.malig=3}           => {Class=recurrence-events} 0.01398601          1 3.364706     4
    ## [20] {inv.nodes=6-8,                                                                          
    ##       deg.malig=3,                                                                            
    ##       breast.quad=left_low}  => {Class=recurrence-events} 0.01748252          1 3.364706     5

``` r
#For itemsets that occur in at least 1% of all women in the dataset, the top 20 have rhs with recurrence events. 
```

``` r
rules <- apriori(data,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.05, conf=.99), #minimum 5% of all observations
                 appearance = list(rhs=c("Class=no-recurrence-events",
                                         "Class=recurrence-events"),
                                   default="lhs"))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted[1:20])
```

    ##      lhs                        rhs                             support confidence     lift count
    ## [1]  {Tumor.Size=10-14,                                                                          
    ##       inv.nodes=0-2}         => {Class=no-recurrence-events} 0.09090909          1 1.422886    26
    ## [2]  {Tumor.Size=10-14,                                                                          
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.08741259          1 1.422886    25
    ## [3]  {Tumor.Size=10-14,                                                                          
    ##       node.caps=no}          => {Class=no-recurrence-events} 0.09440559          1 1.422886    27
    ## [4]  {inv.nodes=0-2,                                                                             
    ##       breast.quad=right_low,                                                                     
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.05594406          1 1.422886    16
    ## [5]  {node.caps=no,                                                                              
    ##       breast.quad=right_low,                                                                     
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.05594406          1 1.422886    16
    ## [6]  {Tumor.Size=10-14,                                                                          
    ##       inv.nodes=0-2,                                                                             
    ##       breast=left}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [7]  {Tumor.Size=10-14,                                                                          
    ##       node.caps=no,                                                                              
    ##       breast=left}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [8]  {Tumor.Size=10-14,                                                                          
    ##       inv.nodes=0-2,                                                                             
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.08391608          1 1.422886    24
    ## [9]  {Tumor.Size=10-14,                                                                          
    ##       inv.nodes=0-2,                                                                             
    ##       node.caps=no}          => {Class=no-recurrence-events} 0.09090909          1 1.422886    26
    ## [10] {Tumor.Size=10-14,                                                                          
    ##       node.caps=no,                                                                              
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.08741259          1 1.422886    25
    ## [11] {Age=50-59,                                                                                 
    ##       Menopause=ge40,                                                                            
    ##       deg.malig=1}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [12] {inv.nodes=0-2,                                                                             
    ##       node.caps=no,                                                                              
    ##       breast.quad=right_low,                                                                     
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.05594406          1 1.422886    16
    ## [13] {Tumor.Size=10-14,                                                                          
    ##       inv.nodes=0-2,                                                                             
    ##       node.caps=no,                                                                              
    ##       breast=left}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [14] {Tumor.Size=10-14,                                                                          
    ##       inv.nodes=0-2,                                                                             
    ##       node.caps=no,                                                                              
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.08391608          1 1.422886    24
    ## [15] {Age=50-59,                                                                                 
    ##       Menopause=ge40,                                                                            
    ##       inv.nodes=0-2,                                                                             
    ##       deg.malig=1}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [16] {Age=50-59,                                                                                 
    ##       Menopause=ge40,                                                                            
    ##       deg.malig=1,                                                                               
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [17] {Age=50-59,                                                                                 
    ##       Menopause=ge40,                                                                            
    ##       node.caps=no,                                                                              
    ##       deg.malig=1}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [18] {Menopause=ge40,                                                                            
    ##       inv.nodes=0-2,                                                                             
    ##       deg.malig=1,                                                                               
    ##       breast=left}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [19] {Menopause=ge40,                                                                            
    ##       deg.malig=1,                                                                               
    ##       breast=left,                                                                               
    ##       irradiat=no}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15
    ## [20] {Menopause=ge40,                                                                            
    ##       node.caps=no,                                                                              
    ##       deg.malig=1,                                                                               
    ##       breast=left}           => {Class=no-recurrence-events} 0.05244755          1 1.422886    15

``` r
#For itemsets that occur in at least 5% of all women in the dataset, the top 20 have rhs with no-recurrence events. 
```

***Using Naive Bayes to predict Class***

``` r
#Rule based classifier using jrip
set.seed(1);jripFit <- train(data[,-1], data[,1], method = "JRip")
summary(jripFit)
```

    ## 
    ## === Summary ===
    ## 
    ## Correctly Classified Instances         217               75.8741 %
    ## Incorrectly Classified Instances        69               24.1259 %
    ## Kappa statistic                          0.2899
    ## Mean absolute error                      0.3661
    ## Root mean squared error                  0.4278
    ## Relative absolute error                 87.5151 %
    ## Root relative squared error             93.613  %
    ## Total Number of Instances              286     
    ## 
    ## === Confusion Matrix ===
    ## 
    ##    a   b   <-- classified as
    ##  194   7 |   a = no-recurrence-events
    ##   62  23 |   b = recurrence-events

``` r
plot(jripFit)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
jripFit$finalModel
```

    ## JRIP rules:
    ## ===========
    ## 
    ## (deg.malig = 3) and (node.caps = yes) => .outcome=recurrence-events (30.0/7.0)
    ##  => .outcome=no-recurrence-events (256.0/62.0)
    ## 
    ## Number of Rules : 2

``` r
#JRIP only outputs 2 rules. The first one indicates that if the degree of malignancy is 3 (the tumor predominately consists of cells that are highly abnormal) and node.caps=yes (the cancer is allowed to spread to surrounding tissues), a recurrence in cancer is likely to come also.
#If the degree of malignancy is not 3 and node caps is no, the second rule indicates that the cancer will not return. 
#JRIP correctly classifies 75.9% of the instances correctly.
```

``` r
#using PART method
set.seed(1);partFit <- train(data[,-1], data[,1], method = "PART")
plot(partFit)
```

![](Post1_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
summary(partFit)
```

    ## 
    ## === Summary ===
    ## 
    ## Correctly Classified Instances         229               80.0699 %
    ## Incorrectly Classified Instances        57               19.9301 %
    ## Kappa statistic                          0.4544
    ## Mean absolute error                      0.3033
    ## Root mean squared error                  0.3894
    ## Relative absolute error                 72.4955 %
    ## Root relative squared error             85.2021 %
    ## Total Number of Instances              286     
    ## 
    ## === Confusion Matrix ===
    ## 
    ##    a   b   <-- classified as
    ##  191  10 |   a = no-recurrence-events
    ##   47  38 |   b = recurrence-events

``` r
partFit$finalModel # print the rules
```

    ## PART decision list
    ## ------------------
    ## 
    ## node.caps = no AND
    ## inv.nodes = 0-2 AND
    ## Tumor.Size = 20-24: no-recurrence-events (33.0/7.0)
    ## 
    ## deg.malig = 1 AND
    ## Tumor.Size = 10-14: no-recurrence-events (14.0)
    ## 
    ## deg.malig = 2 AND
    ## inv.nodes = 0-2 AND
    ## breast.quad = left_low: no-recurrence-events (37.0/7.0)
    ## 
    ## deg.malig = 2 AND
    ## inv.nodes = 0-2 AND
    ## breast.quad = left_up: no-recurrence-events (23.0/2.0)
    ## 
    ## deg.malig = 1 AND
    ## Tumor.Size = 30-34: no-recurrence-events (13.0/2.0)
    ## 
    ## deg.malig = 1: no-recurrence-events (36.0/9.0)
    ## 
    ## deg.malig = 2 AND
    ## Tumor.Size = 25-29: no-recurrence-events (9.0/3.0)
    ## 
    ## deg.malig = 2 AND
    ## Tumor.Size = 20-24 AND
    ## breast = right: no-recurrence-events (7.0/1.0)
    ## 
    ## deg.malig = 2: no-recurrence-events (39.0/11.0)
    ## 
    ## node.caps = yes: recurrence-events (30.0/7.0)
    ## 
    ## inv.nodes = 3-5: recurrence-events (6.0/1.0)
    ## 
    ## inv.nodes = 0-2 AND
    ## Tumor.Size = 25-29 AND
    ## breast = left: recurrence-events (5.0/1.0)
    ## 
    ## inv.nodes = 0-2 AND
    ## Tumor.Size = 30-34 AND
    ## irradiat = no: no-recurrence-events (11.0/4.0)
    ## 
    ## inv.nodes = 0-2 AND
    ## Tumor.Size = 25-29: no-recurrence-events (4.0/1.0)
    ## 
    ## Tumor.Size = 15-19: no-recurrence-events (3.0)
    ## 
    ## Age = 50-59 AND
    ## irradiat = no: no-recurrence-events (7.0)
    ## 
    ## node.caps = no: recurrence-events (7.0/1.0)
    ## 
    ## : no-recurrence-events (2.0)
    ## 
    ## Number of Rules  :   18

``` r
#INTERPRETATION: If a woman does not have node.caps (the cancer has not replaced a lymph node and penetrated the capsule of the lymph node, which would allow it to invade the surround tissues),
#the number of axillary lymph nodes that contain metastatic breast cancer is between 0 and 2, and the tumor size is in the 20-24, the woman is not likely to have a return of cancer. 
#If a woman has a degree of malignancy of 1 (tumors that are grade 1 predominately consist of cells that retain many of their usual characteristics) and have a tumor size
#10-14 are also likely to not having recurrence events.
#The first time a rule with a recurrent case occurs is when node.caps=yes (the cancer is allowed to spread to surrounding tissues). This intuitively makes sense.
#The PART method correctly classifies 80.1% of the instances correctly and outputs 18 rules.

###PART has the better accuracy!
```
