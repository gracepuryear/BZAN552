Post 3
================
Grace Puryear
December 11, 2018

R Markdown
----------

``` r
spotify <- read.csv('SpotifyAudioFeaturesNov2018.csv')
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.4

``` r
library(reshape)
library(class)
```

    ## Warning: package 'class' was built under R version 3.4.4

    ## 
    ## Attaching package: 'class'

    ## The following object is masked from 'package:reshape':
    ## 
    ##     condense

``` r
#Scaling
spotify$acousticness <- scale(spotify$acousticness)
spotify$danceability <- scale(spotify$danceability)
spotify$duration_ms <- scale(spotify$duration_ms)
spotify$energy <- scale(spotify$energy)
spotify$instrumentalness <- scale(spotify$instrumentalness)
spotify$key <- scale(spotify$key)
spotify$liveness <- scale(spotify$liveness) 
spotify$loudness <- scale(spotify$loudness)
spotify$mode <- scale(spotify$mode)
spotify$speechiness <- scale(spotify$speechiness)
spotify$tempo <- scale(spotify$tempo)
spotify$time_signature <- scale(spotify$time_signature)
spotify$valence <- scale(spotify$valence)
```

***Initial Analysis***

``` r
# Visualizing popularity
qplot(spotify$popularity, binwidth = 1, 
      main = "Histogram of ratings", xlab = "Raw Popularity")
```

![](Post3_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
#appears to have a normal distribution besides the peak at 0.

m5 <- head(spotify[order(spotify$popularity,decreasing = TRUE),],5)
m5<- m5[, c(1,4,5,7,8,10,16)] 
m5<- as.data.frame(m5)
m5.long <- melt(m5, id.vars="artist_name")

mp1<- ggplot(data=m5.long, aes(x=variable, y=value))+geom_bar(aes(y=value, fill=artist_name),stat="identity", alpha=0.8 , position="dodge")+ ylab("Value")+ xlab("Variables to a song")+coord_flip()+ggtitle("Top 5 songs in Spotify 2017 ")
mp1
```

![](Post3_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
#In this initial analysis, we can see that the top 5 most popular songs have high valence, energy, and danceability and low liveness, instrumentalness, and acousticness.
```

***What makes a song popular?***

``` r
fit <- kmeans(spotify[,4:16],4)
# determining number of clusters with SSE
SSEs <- rep(NA,10) # a vector to store SSEs for different k's
SSEs[1] <- fit$totss # total SSE if no clustering is done
for(k in 2:10){
  fit <- kmeans(spotify[,4:16],k)
  SSEs[k] <- fit$tot.withinss
}
```

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 5818600)

``` r
par(mar=c(4,4,1,1))
plot(1:10,SSEs,type="b",xlab="Number of Clusters")
```

![](Post3_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
set.seed(1);fit <- kmeans(spotify[,4:16], 6) #using k=6
fit$center  # centers of each variable
```

    ##   acousticness danceability  duration_ms      energy instrumentalness
    ## 1   -0.4687668    0.1846141 -0.058201440  0.29177118        1.6081986
    ## 2    1.5222188   -1.1552481  0.165127313 -1.52724559        0.9380894
    ## 3   -0.2677418    0.2197740 -0.003870139  0.27936057       -0.5565619
    ## 4   -0.1881289    0.5799671 -0.234616259  0.08882113       -0.5266259
    ## 5   -0.3145080   -0.4975153  0.141161790  0.66353490       -0.1033668
    ## 6   -0.3295320    0.2980444 -0.005583918  0.36251675       -0.5687936
    ##           key    liveness     loudness         mode speechiness
    ## 1  0.01900569 -0.23335572 -0.002117751 -0.093142670 -0.35247423
    ## 2 -0.06492740 -0.28317830 -1.578799709  0.126055613 -0.48958770
    ## 3 -0.96060455 -0.19524719  0.442634399  0.412794193 -0.26333850
    ## 4  0.01093255 -0.01619706  0.135173836 -0.150406141  2.18832997
    ## 5  0.03365941  2.88931886  0.204785599 -0.009229116  0.08932653
    ## 6  0.76742786 -0.20770421  0.472959747 -0.293613727 -0.24632204
    ##         tempo time_signature     valence
    ## 1  0.21419182     0.12016249 -0.01878142
    ## 2 -0.52206883    -0.49401613 -0.78661286
    ## 3  0.10846506     0.09050453  0.17178902
    ## 4  0.06960073     0.14125233  0.25123495
    ## 5  0.01325801    -0.01995440 -0.17612550
    ## 6  0.09574742     0.11269920  0.26912753

``` r
spotify_fit <- cbind(spotify,fit$cluster)


#Here, I chose to stick with 6 clusters for the rest of the analysis. Although changing each time, 6 clusters consistently seemed to be the 'elbow' of the plot.
#In other words, it was the point at which the sum of squared errors went from a dramatic decreasing to a constant decrease. 
```

Using 6 clusters
----------------

``` r
df <- as.data.frame(matrix(data=NA,nrow=6,ncol=2))
colnames(df)[1] <- "cluster"
colnames(df)[2] <- "average popularity"
for(i in 1:6){
  popularity <- mean(spotify_fit[which(spotify_fit$`fit$cluster`==i),"popularity"])
  df[i,1] <- i
  df[i,2] <- popularity
}
print(df)
```

    ##   cluster average popularity
    ## 1       1           18.46262
    ## 2       2           18.67701
    ## 3       3           27.59084
    ## 4       4           24.00480
    ## 5       5           23.02452
    ## 6       6           27.67386

``` r
boxplot(spotify_fit$popularity~spotify_fit$`fit$cluster`)
```

![](Post3_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
#There is no obvious difference in popularity. However, we do notice that clusters 1 and 2 have the lowest average popularity. Clusters 3 and 6 have the highest average popularity.



##Boxplots for binning popularity with other variables
boxplot(spotify_fit$acousticness~cut(spotify$popularity,breaks = 10),ylab="Acousticness",xlab="Popularity")
```

![](Post3_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
#There is no obvious difference between the popularity groups and the level of acousticness. However it does appear that the least popular group does have the highest level of acousticness, while the 70-80 range of popularity had the lowest level of acousticness.

boxplot(spotify_fit$danceability~cut(spotify$popularity,breaks = 10),xlab="Popularity",ylab="Danceability")
```

![](Post3_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
#Here it does seem that an increase in popularity is associated with an increase in danceability.

#After doing analysis on the other variables, these are the only variables that show any difference between clusters.
```

***Can we make recommendations for Spotify listeners?***

``` r
colnames(spotify_fit)[18] <- "Cluster"


c1songs <- spotify_fit[which(spotify_fit$Cluster==1),]
c1 <- as.character(head(c1songs[order(c1songs$popularity,decreasing = TRUE),"track_name"],10))
c2songs <- spotify_fit[which(spotify_fit$Cluster==2),]
c2 <- as.character(head(c2songs[order(c2songs$popularity,decreasing = TRUE),"track_name"],10))
c3songs <- spotify_fit[which(spotify_fit$Cluster==3),]
c3 <- as.character(head(c3songs[order(c3songs$popularity,decreasing = TRUE),"track_name"],10))
c4songs <- spotify_fit[which(spotify_fit$Cluster==4),]
c4 <- as.character(head(c4songs[order(c4songs$popularity,decreasing = TRUE),"track_name"],10))
c5songs <- spotify_fit[which(spotify_fit$Cluster==5),]
c5 <- as.character(head(c5songs[order(c5songs$popularity,decreasing = TRUE),"track_name"],10))
c6songs <- spotify_fit[which(spotify_fit$Cluster==6),]
c6 <- as.character(head(c6songs[order(c6songs$popularity,decreasing = TRUE),"track_name"],10))

top_songs <- cbind(c1,c2,c3,c4,c5,c6)
colnames(top_songs) <- c("Cluster 1", "Cluster 2" ,"Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")
top_songs
```

    ##       Cluster 1            Cluster 2                     
    ##  [1,] "New Rules"          "when the party's over"       
    ##  [2,] "Your Love"          "lovely (with Khalid)"        
    ##  [3,] "Read"               "You Are The Reason"          
    ##  [4,] "I'LL BE THERE"      "Sober"                       
    ##  [5,] "Magenta Riddim"     "In My Head"                  
    ##  [6,] "COME THRU"          "Come Back to Earth"          
    ##  [7,] "Losing It"          "This Way"                    
    ##  [8,] "I'LL SEE YOU IN 40" "before I close my eyes"      
    ##  [9,] "Woman"              "Canâ€™t Help Falling in Love"
    ## [10,] "Unity"              "Will He"                     
    ##       Cluster 3                                       
    ##  [1,] "thank u, next"                                 
    ##  [2,] "Taki Taki (with Selena Gomez, Ozuna & Cardi B)"
    ##  [3,] "Happier"                                       
    ##  [4,] "Sunflower - Spider-Man: Into the Spider-Verse" 
    ##  [5,] "Mo Bamba"                                      
    ##  [6,] "Sweet but Psycho"                              
    ##  [7,] "In My Mind"                                    
    ##  [8,] "Taste (feat. Offset)"                          
    ##  [9,] "In My Feelings"                                
    ## [10,] "BEBE"                                          
    ##       Cluster 4                                
    ##  [1,] "Eastside (with Halsey & Khalid)"        
    ##  [2,] "I Love It (& Lil Pump)"                 
    ##  [3,] "FEFE (feat. Nicki Minaj & Murda Beatz)" 
    ##  [4,] "Adan y Eva"                             
    ##  [5,] "Drip Too Hard"                          
    ##  [6,] "Lucky You (feat. Joyner Lucas)"         
    ##  [7,] "Mala MÃ­a"                              
    ##  [8,] "Never Recover (Lil Baby & Gunna, Drake)"
    ##  [9,] "Fine China"                             
    ## [10,] "Armed And Dangerous"                    
    ##       Cluster 5                              
    ##  [1,] "Waste It On Me (feat. BTS)"           
    ##  [2,] "Always Remember Us This Way"          
    ##  [3,] "HipÃ³crita"                           
    ##  [4,] "Venom - Music From The Motion Picture"
    ##  [5,] "Diamond Heart"                        
    ##  [6,] "Ciumeira - Ao Vivo"                   
    ##  [7,] "Me Niego"                             
    ##  [8,] "Atrasadinha - Ao Vivo"                
    ##  [9,] "Largado Ã€s TraÃ§as - Ao Vivo"        
    ## [10,] "Spotlight"                            
    ##       Cluster 6                                  
    ##  [1,] "MIA (feat. Drake)"                        
    ##  [2,] "SICKO MODE"                               
    ##  [3,] "ZEZE (feat. Travis Scott & Offset)"       
    ##  [4,] "Without Me"                               
    ##  [5,] "Arms Around You (feat. Maluma & Swae Lee)"
    ##  [6,] "Lucid Dreams"                             
    ##  [7,] "Promises (with Sam Smith)"                
    ##  [8,] "SAD!"                                     
    ##  [9,] "Moonlight"                                
    ## [10,] "Better Now"

``` r
#These are the most popular songs for each cluster. At first glance, it seems a little more obvious why Clusters 1 and 2 are
#less popular (they are either older songs or less well known). Clusters 3 and 6 are the currently most popular songs. Because 
#popularity is only for the month of November, it makes sense that the most recently released songs
#and the most well known artists would appear in the most popular clusters.
```

***Using K nearest neighors to predict Popularity***

``` r
index <- sample(1:nrow(spotify),.5*nrow(spotify))

spot_train <- spotify[index,c(4:16)]
spot_test <- spotify[-index,c(4:16)]
spot_train_labels <- spotify[index, 17]
spot_test_labels <- spotify[-index, 17]
k_nn <- knn(train = spot_train, test = spot_test,cl = spot_train_labels, k=20)
k_nn <- as.integer(k_nn)
errors = abs(k_nn - spot_test_labels)
mean(errors)
```

    ## [1] 17.83668

``` r
#Finally, I have used K nearest neighbors to predict popularity. This uses the euclidean distance to see what 20 songs are the most similar and takes the average popularity of those songs. This can be used where someone wants to see how popular a song will be given the song attributes. The average error is about 17.8 points off what the popularity actually is.
```
