---
title: "Spatial Autocorrelation Tutorial"
author: "Parker Gibbons"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output: pdf_document
#output: 
#  bookdown::html_document2:
#    number_sections: false
#   fig_caption: true
#    global_numbering: true 
---


## Introduction

The process of measuring spatial autocorrelation is a foundational concept in regards to the field of spatial statistics. It refers to the correlation among values of a single variable that are strictly attributable to their relatively close locational positions [ 1 ]. When conducting a spatial autocorrelation test a positive SPAC would indicate that similar values do tend to be around one another such as in an urban setting when wealthy neaighbourhoods tend to cluster together. In contrast to this, a negative SPAC would show that dissimilar values are often found around each other similar to when industrial areas are built next to residential zones. This is related to Tobler's first law of geography which states that “everything is related to everything else, but near things are more related than distant things.” The presence of spatial autocorrelation is important to understand as it is regularly taken as indicating that there are phenomena of interest in the distribution of the dataset that should be further investigated [ 2 ]. For this example, a collection of census data from across is used and subset to the variable of interests for this specific study. Those two variables are median total income and percentage of respondents with french language knowledge.


Libraries in R refers to a collection of functions, datasets and code that have been pre-configured to meet the needs of certain tasks. They are crucial tools in tasks such as data manipulation, visualization, statistical modeling and spatial analysis. In order to call packages in from the library, they first need to be installed using either the package search function in the bottom right window of R or by using the code install.packages(package name). Once the package is installed it can be called into R using the code seen in the window below [ 3 ].

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}

#Load in libraries:
library(knitr)
library(tmap)
library(spdep)
library(raster)
library(shinyjs)
library(e1071)
library(sf)
library(rmarkdown)
library(tinytex)

```

As with any analysis using R, the first thing needing to be done is determining where we pull the data we are using from. To do this we have to set a working directory using the appropriate code and copy the address to the desired folder in order to attach it to this code. Once we have established what folder R will be pulling data from we can work on bringing the appropriate data into the work space. To start, we’ll want to read the shape file connected to the census boundaries in order to bring it into R as an st data frame. We will also need to bring the census data into the data frame as well. In this example the .csv file contains the census data and values and the .shp produces the boundaries that the census data resides in from a geographical standpoint.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#Set working directory
dir <- "C:/Users/parke/OneDrive/Documents/Geog 418 Assignment 3"
setwd(dir)


#This is the census data file containing the data and values of interest
csv <- read.csv("~/Geog 418 Assignment 3/ucgsJQnBVLvP_data.csv") 

#Data source is the working dir (where the layer is), layer is the name of the file (without .shp)
shp <- st_read("~/Geog 418 Assignment 3/lda_000b16a_e.shp") 

```

At this point we have successfully brought our data into the workspace, however the data is far from perfect and we will need to clean it up to make it simpler to use for further analysis. Taking a look at the csv file we previously brought into the dataframe, the column names don't tell us a whole lot about what the values are indicating. To resolve this we can create a vector of the column names to identify which columns contain which data. Once this is determined we will remove any rows with fewer than 8 numbers and merge the cleaned up data with the boundary polygon dataframe from earlier. With our data cleaned and sorted we can subset it to the chosen city for our study, which in this case is Vancouver.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New names given to their associated columns
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID characters
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for Windsor
Municp <- subset(census_DAs, census_DAs$CMANAME == "Windsor")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Building on the idea of ensuring our data is clean, we also need to make sure that each value present in the data set is relevant. We must confirm that none of our data points have NA or 0 values connected to them as the presence of these missing values can skew the results of the analysis leading to inaccurate interpretations. To avoid this we simply need to remove any polygons that do not contain real values for the variables of interest, median total income and knowledge of french. Doing so is relatively straightforward using the following code.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$`PercFrench`)),]

```

With our data cleaned up and rid of any potential null values we can start focusing on interpreting the specific french knowledge and median income statistics. Doing so will require calculating a number of descriptive statistics including the mean, standard deviation and skewness. Once these descriptive statistics have been calculated we can create a data frame with these results to be displayed in a neat table.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$`PercFrench`)
stdevFrench <- sd(French_noNA$`PercFrench`)
skewFrench <- skewness(French_noNA$`PercFrench`)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for Windsor", 2016, " census variables"))
```
![image](https://github.com/user-attachments/assets/978ce939-d966-4d0f-83e9-4c7ac8f700c9)

The following section works towards creating two different thematic maps for each of chosen variables. The first line of code is responsible for specifying the spatial object that we are wanting to plot while the second line is the function that adds the polygon shape to our map defining the regions to be filled by the desired spatial object. Next are the specific characteristics unique to the map such as the title and classification style. In this scenario a “jenks” or natural breaks classification is used, which groups the data into classes that minimize variance within classes and maximizes variance between classes. At the same time we must select the number of classes we want this data to be grouped into, six in this case. Last thing left to do is remove the borders around the polygons by setting the transparency to zero, make it so any polygons with NA values are displayed as grey, and determine where we want the legend to be. We can then repeat these same lines of code for the french knowledge variable, making sure to change out the variables in the first three lines to ensure the map communicates the appropriate information.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Windsor census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete
# tmaptools::palette_explorer() #Tool for selecting pallettes
# CRS for Windsor using UTM zone 10N:

#Map median Income
map_Income <- tm_shape(Income_noNA) + #specifies the spatial object
  tm_polygons(col = "Median total income", #adds polygon shapes to map
              title = "Median total income", 
              style = "jenks", #sets the classification scale to natural breaks
              palette = "BuGn", n = 6, #sets the colour ramp used and determines number of classes
              border.alpha = 0,
              colorNA = "grey") + #ensures areas with missing data are marked as grey 
  tm_layout(legend.position = c("RIGHT", "BOTTOM")) #determines the location of the legend

#Map French Knowledge
map_French <- tm_shape(French_noNA) + #change
  tm_polygons(col = "PercFrench", #change
              title = "Percentage with \n French Knowledge", #change
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "BOTTOM"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
![image](https://github.com/user-attachments/assets/06ad3327-f753-4d32-ad0e-859a733c239e)

Figure 1. 2016 Windsor census areas showing median total income (left) and percentage of respondants with knowledge of french (right). 

## Neighbourhood matrix

A weighted neighbourhood matrix is an effective tool in regards to representing the spatial relationships between any chosen geographic units. Each value in the matrix indicates the strength of the relationship or connection between the pairs of locations with higher weights indicating a stronger connection [ 4 ].  Determining a weighted neighbourhood matrix is a crucial step in further calculating SPAC measures such as Moran’s I as it puts into perspective which nearby locations share similar values. Arguably the two most common methods for defining spatial weights are the queen and rook weightings. These methods function very similarly to how a queen and a rook are able to move across a chessboard. In a queen weighting scheme, two areas are considered neighbours if they share a common edge or vertex similar to how a queen can move horizontally, vertically, or diagonally. A rook weighting scheme requires two areas to share a common edge in order to be considered neighbours such as how a rook in chess can only move horizontally or vertically.

The poly2nb( ) function which is a part of the spdep package that we had previously called into R from the library in the first section of this analysis makes creating a list of neighbours for our variables incredibly simple. In this package, the default weighting scheme is queen weighting, therefore if we want to use a rook weighting scheme we can just add queen = FALSE within the brackets of the poly2nb function.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)

```

Below is an example of how we can use these weighting schemes to create maps showing spatial autocorrelation of our chosen variables, in this example median total income was used. The first chunk of code generates a map utilizing a queen weighting matrix. The first line sets the base map layer (Income_noNA) with light grey borders while the second line adds the code that correlates to the desired queen weighting matrix (income.net) which adds lines in the colour red to show the queen contiguity. The second chunk repeats the same steps from the Queen weighting matrix but instead uses the variable that correlates to the rook weighting matrix (income.net2) in the second line. The third chunk utilizes the same (Income_noNA) variable to set the base map with light grey border before using the same (income.net) and (income.net2) to display the same rook and queen weighting contiguities on the same map.

Comparing the two outputs on the left side by side, there does not appear to be any immediate differences. This is where the third output showing both weighting schemes overlaid is beneficial as it allows to see the additional connections between neighbourhoods that were able to be made by the queen weighting scheme as displayed in red.

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Windsor census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Create a map using the Queen weighting matrix
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='red')

#Create a map using the Rook weighting matrix
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

#Create a map that combines the queen and rook weighting matrix 
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='red', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

#Print the three maps side by side 
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
```
![image](https://github.com/user-attachments/assets/3b898b4d-3c21-49dc-a721-0832b5eeabbb)

Figure 2. 2016 Windsor census areas showing median total income neighbours using queens weight (left) rooks weight (middle) and combination of both (right).

The following section looks to create spatial weight matrices and extract a subset of the weights for further inspection. There are a number of spatial weights that can be used in spatial analysis with each focusing on different aspects of the spatial relationship of the study objects. The primary decision that has to be made in this section is the “style” of weight we want to use. In this instance we select a “W” style which refers to row-standardized weights. This results in the weight for each location being normalized so that the sum of the weights for all neighbours of a given location will sum to a value of 1 [ 5 ]. Doing this makes the weights comparable across different locations, ensuring that the influence of each location's neighbour is standardized.

To create spatial weight matrices for this R analysis, we can use the nb2listw function which is a part of the spdep library we have previously installed and called into R from the library. The first line of code below creates a row standardized weights matrix for our total median income variable. Here, (Income.nb) refers to the chosen variable object and (style = “W”) specifies that we would like to use row-standardized weights and setting zero.policy to true will assign a weight of zero to regions with no neighbours. The code succeeding this creates an identical weight matrix for the french knowledge variable by changing out the variable that is first in the brackets. Once the weight matrix is created we can extract the first three elements from the income weights into a subset that can be printed and viewed for an initial inspection. 

As you can see in the outputs below, the use of row standardized weighting has made it so that the weights for each sum to a value of 1. The first two observations are identical and show that each neighbouring region contributes an equal weight of 0.2 indicating the regions have 5 total neighbours. The third observation shows that each neighbour has an assigned weight of approximately 0.142857 indicating that the region has 7 neighbours.

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Creates a weight matrix for the income variable using row-standardized weights
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create a weight matrix for the french knowledge variable using row-standardized weights
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

#Extracts the first 3 elements of income weight matrix and prints the subset
subset_weights <- head(Income.lw[["weights"]])[c(1:3)]
print(subset_weights)
```
![image](https://github.com/user-attachments/assets/2cb95946-3f3c-4ab5-b639-c89a232c548b)

Figure 3. Weight matric for total median income varibale using row-standardized weights.

## Global Moran’s I

With the understanding of how to both choose and weigh our neighbourhoods, we can move onto the calculation of a Global Moran’s I statistic. This statistic is a measure of spatial autocorrelation that is used to assess whether similar values are found clustering among each other across a data set. Values for a global moran’s I will range from -1 to 1 with a value closer to 1 indicating strong positive spatial autocorrelation (similar values are clustered) and a value close to -1 indicating negative spatial autocorrelation (dissimilar values are clustered). The contrary to these possibilities is a value of 0 which suggests no spatial autocorrelation or a random distribution of values [ 6 ].  The global moran’s I is considered a global statistic because it provides a single value that summarizes the overall spatial pattern across an entire dataset. This is in contrast to a local Moran’s I statistic which will assess spatial autocorrelation at an individual location. Think of a global statistic, such as global moran’s I, providing “the big picture”. While a local statistic provides location specific details and insights. The equation for the Global Moran’s I can be seen in the window below.

![image](https://github.com/user-attachments/assets/55c7e3d8-d5b2-41ea-8872-00c47d12dac4)

In the above formula, the spatial weight (Wij) captures the relationship between the two locations (i and j), while (xi - x̄) and (xj - x̄) represents the deviations of the values at those locations from the global mean which is represented by (x̄). The numerator displays the spatial covariance between the pairs of locations weighted by their spatial relationship [ 7 ]. The denominator is used to normalize the covariance by considering the global variance in the data which is expressed as the product of the sum of weights (the first set of brackets) and the variance (everything after the first set of brackets).


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income. Observed Moran's I
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]] ##Observed Moran's I
eIIncome <- miIncome$estimate[[2]] #Expected Moran's I
varIncome <- miIncome$estimate[[3]] #Variance of Moran's I

#Calculate Global Moran's I for French. Observed Moran's I
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]] #Observed
eIFrench <- miFrench$estimate[[2]] #Expected
varFrench <- miFrench$estimate[[3]] #Variance
```

By using this formula alongside our chosen total median income and french knowledge variables, we can calculate the observed Moran’s I (miIncome and miFrench). For our income variable the observed Moran’s I variable was 0.781 and the French variable saw a value of 0.331. This indicates to us that our income variable has a high degree of positive spatial autocorrelation and the french variable, while still positive, has a weaker relationship with spatial autocorrelation. 

Now that the observed Moran’s I is calculated we can extract the expected Moran’s I and the variance of it. The expected Moran’s I is the value that we would expect to observe under the null hypothesis of no spatial autocorrelation, thus providing a theoretical baseline for comparison against the observed Moran’s I. The expected Moran’s I for the income and french variable was -0.001862 and -0.001828 respectively. The variance of Moran’s I helps in determining the statistical significance of the Moran’s I value by measuring the spread or variability under the assumption of no spatial autocorrelation, our null hypothesis. It is also used to calculate a z-score which can tell us hope for the observed Moran’s I deviated from the expected value in terms of the standard deviation [ 8 ]. In this analysis the income variable had a variance of 0.00065 and the french variable saw a variance of 0.00062 which would indicate that there is a relatively small degree of variability in the distribution of Moran’s I under the null hypothesis.

```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

With the observed, expected and global moran’s I calculated, the next step is to determine the Global Moran’s I range for our income variable. We do this in order to provide a summary of how much the spatial relationship varies across the chosen dataset and can help indicate positive or negative spatial autocorrelation. By using the above code the minimum value of the range was determined to be -0.6642 and the maximum value was found to be 1.0367. This high variability in the range suggests a diverse spatial structure within the data set indicating that there is likely to be areas of both significant clustering and significant dispersion [ 1 ]. From this point, further analysis can be done in order to determine if the patterns that are being shown are statistically significant or not. We can do this by using a Z-test in which we determine the null hypothesis to be that there is no spatial autocorrelation. In contrast to this, the alternative hypothesis would be that there is a presence of spatial autocorrelation. By using an alpha value of 0.05 we can determine that if our calculated z score falls above 1.96 or below -1.96 then the result is significant and we would reject the null hypothesis. If the value falls between these two values then the result is not statistically significant and we would fail to reject the null hypothesis. In order to complete a z-test and obtain a calculated z score we can use the following code.

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

By subtracting the expected moran’s I value from the observed moran’s I value and dividing it by the square root of the variance we can obtain a z-score for both of our variables. The outcome is a z-score of 30.631 for the income variable and a z-score of 13.341 for our french variable. With both of the calculated z-scores being this large it is safe to reject the null hypothesis confirming that they significantly deviate from the mean. 

## Local spatial autocorrelation

Local spatial autocorrelation looks to assess how similar values of a variable are clustered or dispersed within a specific geographic area. This is in contrast to global spatial autocorrelation which considers the entire study region. Oftentimes it will use Local Indicators of Spatial Association (LISA) in order to identify significant spatial patterns with the goal of revealing hot spots of similar values as well as outliers such as when high values are surrounded by low values or vice versa [ 9 ]. For this example we will be completing a calculation for Local Moran’s I, which in many ways is similar to the Global Moran’s I discussed earlier, rather the arrangements in the calculation itself are different. 	

![image](https://github.com/user-attachments/assets/a783005c-bb2f-4bcd-9bc8-1b23a2a9142a)

Thankfully, rather than having to manually enter the formula and calculations to obtain a local moran’s value, there is a function available to streamline this process. The localmoran( ) function is able to handle all the calculation work within R provided we are able to input the correct variables and weighting scheme for the desired scenario. Below is the code and steps to complete this with the income variable being calculated first and the french knowledge variable being calculated second.

```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```

With this collection of values calculated and on hand, we can work on creating a visual representation on them on a map similar to the one earlier in this tutorial. Below is the code used to create side by side maps of our calculated LISA z-scores for both the income and french knowledge variable.

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Windsor census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```
![image](https://github.com/user-attachments/assets/00709252-03b1-46d1-a763-f588c8cca42c)

As seen in the maps, the census districts have been divided up into three categories and assigned a class represented by a colour. Represented by grey are the census districts whose z score falls between -1.96 and 1.96 which would indicate that the results are not statistically significant and would fail to reject the null hypothesis. Represented by blue and red are the census districts whose z-score falls below and above the rejection region respectively. The red census districts are regions which see significant positive spatial autocorrelation, meaning high values are surrounded by other high values and low values are surrounded by other low values. The blue census districts are regions that experience significant negative spatial autocorrelation, meaning high values are surrounded by low values or vice versa, which can indicate outliers in the dataset. Although the maps serve as an excellent visualization tool, graphing the trends seen on the map can provide an additional layer of information.

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```
![image](https://github.com/user-attachments/assets/4a96b235-7d7e-474e-b10c-201cf8e57140)


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```
![image](https://github.com/user-attachments/assets/c5927754-9aed-4cec-8e62-de0e7152df1c)

Although these plots appear rather intimidating at first glance, they are relatively simple to analyze in practice. The x-axis represents the actual median income values for each observation while the y-axis reflects the income values of neighboring observations. The solid diagonal line represents the best fit line through the observations with the upward trend indicating the presence of positive spatial autocorrelation. Points in the top right of the scatter plot represent locations in which the observed observation and its neighbors are well above the mean. Points in the bottom left indicate locations in which the observed observation and its neighbours are below the mean. Both of these outcomes on the plot represent positive spatial autocorrelation as regardless of if the observed observation and its neighbours are above or below the mean, they are still surrounded by similar values. You might also notice a number of the points on the plot being diamond shaped, these are points that were found to be statistically significant in the data.

## Summary

This analysis sought to extract information regarding the total median income and knowledge of french variables from the 2016 census data within the city limits of Windsor Ontario as a demonstration for a number of statistical tests related to spatial autocorrelation. We discussed the basics of starting an analysis in R right from loading the appropriate libraries and setting a working directory to pull data from. In addition to this we looked at how to clean up data to remove any null values, making it easier to analyse and interpret. Creating thematic maps using calculated descriptive statistics and weighting matrices was done on more than one occasion to ensure a better understanding of what we were looking at through visual cues. Finally, to avoid jumping to any conclusions as a result of only considering visualized data, we completed a number of statistical tests including global and local spatial autocorrelation through the use of Moran’s I and LISA. Throughout this example we determined that both the total median income and knowledge of french variables exemplified positive spatial autocorrelation which resulted in rejecting our null hypothesis.

## References
1. Chen, Y. (2021). An analytical process of spatial autocorrelation functions based on moran’s index. PloS One, 16(4), e0249589–e0249589. https://doi.org/10.1371/journal.pone.0249589

2. Haining, R. P. (2001). Spatial Autocorrelation. In N. J. Smelser & P. B. Baltes (Eds.), International
Encyclopedia of the Social & Behavioral Sciences (pp. 14763–14768). Pergamon.
https://doi.org/10.1016/B0-08-043076-7/02511-0

3. Quick list of useful R packages. (2024, April 26). Posit Support.
https://support.posit.co/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages

4. Earnest, A., Morgan, G., Mengersen, K., Ryan, L., Summerhayes, R., & Beard, J. (2007). Evaluating
the effect of neighbourhood weight matrices on smoothing properties of Conditional
Autoregressive (CAR) models. International Journal of Health Geographics, 6(1), 54–54. https://doi.org/10.1186/1476-072X-6-54

5. Griffith, D. A., & Paelinck, J. H. P. (n.d.). The Spatial Weights Matrix and ESF. In Morphisms for
Quantitative Spatial Analysis (pp. 49–60). Springer International Publishing.
https://doi.org/10.1007/978-3-319-72553-6_5

6. Chen, Y. (2013). New Approaches for Calculating Moran’s Index of Spatial Autocorrelation. PloS One,
8(7), e68336–e68336. https://doi.org/10.1371/journal.pone.0068336

7. How Spatial Autocorrelation (Global Moran’s I) works—ArcGIS Pro | Documentation. (n.d.).
Retrieved October 18, 2024, from
https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/h-how-spatial-autocorrela
ion-moran-s-i-spatial-st.htm

8. Flow Spatiotemporal Moran’s I: Measuring the Spatiotemporal Autocorrelation of Flow
Data—University of Victoria. (n.d.). Retrieved October 19, 2024, from
https://search.library.uvic.ca/discovery/fulldisplay?docid=cdi_crossref_primary_10_1111_gean_12397&context=PC&vid=01VIC_INST:01UVIC&lang=en&search_scope=MyInst_and_CI&adaptor=Primo%20Central&tab=LIBALL&query=any,contains,moran%27s%20i&offset=0

9. BOOTS, B., & TIEFELSDORF, M. (2000). Global and local spatial autocorrelation in bounded regular
tessellations. Journal of Geographical Systems, 2(4), 319–348.
https://doi.org/10.1007/PL00011461
