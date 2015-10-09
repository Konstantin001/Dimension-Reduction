## Analysis Script: Professors Data -- Classification Dimensions
## Developed by Konstantin Tskhay
## Date: Tue Sep 22 22:18:15 2015

## R version 3.2.2 (2015-08-14) -- "Fire Safety"
## x86_64-apple-darwin13.4.0 (64-bit)

## Set WD
setwd("Insert_Path Here")

## Read in the data
data <- read.csv('./data/new_prof_data.csv')

## load packages
library(psych)
library(ggplot2)

## Examined data
names(data) ## Inf has a period at the end -- Any Guesses Why?


names(data) ## Check
str(data) ## The data represent aggregates from multiple students in multiple classes
summary(data)

## Okay, let's start with Cluster Analysis and see if we can cluster Profs
## based on their evaluations

## 1. Standardize your variables using scale() function
scaled.data <- scale(data[, 3:8])

## create a distance matrix:
distance.mat <- dist(scaled.data, method='euclidean' )**2

## Perform hierarchical cluster analysis
hierarchical.cluster <- hclust(distance.mat, method="ward.D")

## Create merge schedule
cbind(hierarchical.cluster$merge, hierarchical.cluster$height)

## plots a dendrogram
plot(hierarchical.cluster, labels = FALSE)

## plots reverse scree
plot(hierarchical.cluster$height)

## cuts the data into clusters at k-defined cluster
data$clusters <- cutree(hierarchical.cluster, k=4)

## K-Means Cluster Analysis 
kmeans.cluster <- kmeans(scaled.data, 4, algorithm='MacQueen')
kmeans.cluster

## how many Professors in each cluster?
table(kmeans.cluster$cluster) 

## What are the cluster centers?
cluster.means <- as.data.frame(t(kmeans.cluster$centers))
cluster.means

## Create a new mini-dataset with cluster means for graphing
cluster.means$dim <- row.names(cluster.means)
names(cluster.means) <- c("c1", "c2", "c3", "c4", "dim")
cluster.means

## Reshape the dataset into a long format
cluster.means.l <- reshape(cluster.means, 
                           varying = names(cluster.means[,1:4]),
                           v.names = "centroid",
                           times = names(cluster.means[, 1:4]),
                           direction = "long")

## reorder the factor; otherwise will graph alphabetically
cluster.means.l$dim <- with(cluster.means.l, 
                            factor(dim, levels = c("Present", 
                                                   "Explain",
                                                   "Communi", 
                                                   "Teach",
                                                   "Difficulty", 
                                                   "Workload")))

## rename time variable to cluster variable
names(cluster.means.l)[2] <- 'cluster'

## PLot cluster means to see how your variables describe each cluster
ggplot(data=cluster.means.l, aes(x=dim, y=centroid, group=cluster, colour=cluster)) +
        geom_line() +
        geom_point() +
        ylab('Score') +
        xlab('Dimension') +
        theme_bw()

## What you see is that there are 4 clusters marked by different colors
## On the y-axis, you see the score that each cluster receives in each
## evaluation dimension (x-axis)

## Red: people who present worse than average, but have an average difficulty class
## Green: Good presenters with average difficulty classes
## Blue: Average Presenters who make their classes easy
## Purple: Average Presenters who make their classes difficult
## You can come up with your own labels now! 

## Let's see how these clusters perfrom on other measures! 
names(data) ## remind yourself about the names

with(data, plot(x = as.factor(clusters), y = learn.Exp)) ## Seems like people like moderate classes
with(data, plot(x = as.factor(clusters), y = Retake)) ## probably no effects

## Cluster membership does not relate to professors' personalities
with(data, plot(x = as.factor(clusters), y = Inf.)) 
with(data, plot(x = as.factor(clusters), y = Kind))

############
############
############
## PCA #####

## So, now that we have grouped professors in the group, 
## we can see whether we can reduce the number of variables 
## To do so, we need to see interrelationships between the variables
## you probably can already see that 2 types of variables emerged:
## Type 1: The variables that specify how good the professors are at communication
## Type 2: The variables that track the course difficulty
## Let us examine whether this in fact may be the case

## PCA = Principal Components Analysis

## Let's exract the variables we are interested in grouping:
comp.data <- data[, 3:8]


## Let's start with the basic correlation matrix
round(cor(comp.data), digits = 3)



## You can see immediately that all communication variables are highly correlated
## The difficulty variable correlates quite highly with the Workload variable
## However, the correlations between communications and difficulty variables
## seem to be orthogonal

## This suggests that there are probably 2 factors in our data
## This example is simple because here we see the separation of the 
## variables in the correlation matrix
## however, the data in the real world are more complex 
## with 20 and even 100 variables or more
## now imagine trying to see how these variables form components 
## or factors using correlation matrix

## The solution to this problem may be to reduce the 
## number of dimensions that are underlying these items

## Okay, let's perform some PCA -- here the variables are standardized 
## prior to analysis
pcaSolution <- prcomp(comp.data, center = TRUE, scale. = TRUE) 

## let's take a look at results
print(pcaSolution)
pcaSolution$sdev^2

## Produced the object with standard deviatons of all variables
## and Rotation -- or the loadings on the principal components

## Let's graph variances of the components versus components
plot(pcaSolution, type = "l")

## This figure will help us to decide how many components we should have
## The first two PC explain most of the variability in the data

## So, how importnat is each component? 
summary(pcaSolution)


## from this, we can see that the first two components explain 
## 85% of variance in the data! 

## Let's plot the loadings on the unit circle! 

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle, aes(x,y)) + geom_path()

loadings <- data.frame(pcaSolution$rotation, 
                       .names = row.names(pcaSolution$rotation))
p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
        coord_fixed(ratio=1) +
        labs(x = "PC1", y = "PC2")

## As you can see, the variables loaded pretty orthogonally 
## onto two components, as we have specified! 

## Now what do people do with this information? Well, in psychology research and 
## I am assuming in many other applications, people would:
## 1. Name each component [Communication Skills, Course Difficulty] -- CS, CD
## 2. Aggregate (mean) the variable that make up each component 
data$CS <- rowMeans(data[, 3:6])
data$CD <- rowMeans(data[, 7:8])

## 3. enter them into a model [e.g., linear model]
fit1 <- lm(learn.Exp ~ CS + CD, data = data); summary(fit1)
fit2 <- lm(Retake ~ CS + CD, data = data); summary(fit2)

## As you can see both CS and CD predict better learning experience, 
## explaining a large proportion of variance

## However, the likelihood of retake increases when the 
## Communication Skills are high but decreases when the 
## Difficulty is high 


#########################################
#########################################
#########################################

## (Exploratory) Factor analysis
## Unlike PCA, EFA assumes that there is an underlying factor that causes the items
## For example, suppose your respondents answer the questions about how
## "outgoing" they are, and how "sociable" they are. We assume further that 
## an underlying personality of extraversion causes people to respond in a 
## certain way! That is, people who are extraverted are expected to say that they 
## are more "outgoing" and "sociable" than people who are introverted. PCA does not 
## necessarily makes such an assumption -- assumes that all of the variables are 
## measured perfectly, and that the data represent total variance that is out there.

## This makes sense, however, the problem is as follows:
## How do we identify this latent factor thing that causes people to 
## behave in one way versus another? This is where Exploratory 
## Factor Analysis comes in! 

## As you can imagine, this method is more common in survey research

## Let us use the professors' data once again to see if the 
## students evaluations can be caused by some unobserved 
## factor or a group of factors

## before we begin, let's recreate the data from comp.data to factor.data
factor.data <- comp.data

## First, let's see how many factors we need -- you guessed it 2
eigen.values <- eigen(cor(factor.data))

## Produces Scree plot
plot(eigen.values$values, type="both")

## as you can see, the eigen values come from the same matrix, 
## hence we see that the 
## first 2 factors explain most of the variance

## Further, you also need to select the factors based on the eigen 
## values themselves
## they should be greater than 1

## additionally, if you are quite adventurous, you would want to 
## examine how much variance
## the factors explain -- 
## your goal to explain as much variance as possible
## > 50%

## Okay, you have decided that you would like to have 2 latent constructs estimated
## This is great. But now, you need to decide whether these factpors would be
## correlated with each other (you can also do this with PCA)
## if correlated, use olique rotation that specifies the correlation [promax, oblimin]
## else, use orthogonal rotation [varimax]

## In the current case, the factors should not be correlated--one specifies
## interpersonal skills and another specifies course information
## as such, you should select orthogonal rotation here
## Also, note the factoring method -- fm = pa, 
## This says to place factor communalities in the diagonal, which would estimate 
## the residual variation, stating that the factors do not explain all of the 
## variance

## Step 1. Construct correlation Matrix from the Data
corMat <- cor(factor.data)
corMat

## Step 2. Conduct Factor analysis
efaSolution <- fa(r = corMat, nfactors = 2, rotate = "varimax", fm = 'pa')
efaSolution

pchisq(4.94, 15, ncp = 0, lower.tail = TRUE, log.p = FALSE)
## Here, you see that there are 2 factors, accountinf for 77% of 
## the variance
## and now we can plot the solution:
fa.diagram(efaSolution)

## What we also see is that 2 factors do not sufficient to explain 
## the data, 
## and it appears that 3 factor solution may be a better fit
## However, looking at the root mean square of the Residuals, 
## we see that the fit
## is not too bad -- this is where different models should be 
## compared agains each other
## to see which solution is better
efaSolution2 <- fa(r = corMat, nfactors = 3, rotate = "varimax", fm = 'pa')
efaSolution2

## Here, what you see is that items explain and present cross-load on 2 factors
## in survey research you do not want to see this--hence, arguably, the 2-factor
## solution is more desirable -- In scale construction, people can either delete
## the third factor or remove the items that have high cross-loadings.

## afterwards, you want to confirm your solution via CFA, which involves fitting the obtained
## data structure to a new dataset all toegther. for example, you can achieve this with a 
## package lavaan. But, this can be a lecture on it's own! 

