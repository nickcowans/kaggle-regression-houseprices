###########################
##   Kaggle Competition  ##
##   House Prices        ##
###########################

# Set up the directory
#setwd("/Users/Thomas/Dropbox/Data Science/Kaggle/competition-house prices-advanced regression")
setwd("data")

# Load up the required packages
library(dplyr)
library(ggplot2)
library(reshape2)

# Import the data
train <- read.csv('train.csv')


#----------------------------------------#
# Stage 1: Preliminary Feature Analysis
#----------------------------------------#

# Set up a numeric matrix
numdata <- data.matrix(train)

# Perform correlations
correlations <- as.data.frame(cor(numdata[,1:80],numdata[,81]))
correlations$feature <- row.names(correlations)
correlations <- arrange(correlations, desc(V1))
colnames(correlations)[1] <- "corr_coef"

# Calculate proportion of missing data for features
missingness <- as.data.frame(apply(numdata, 2, function(col)sum(is.na(col))/length(col)))
colnames(missingness)[1] <- "prop"
missingness <- missingness %>%
    mutate(variable = rownames(missingness)) %>%
    filter(prop != 0) %>%
    arrange(desc(prop))



#----------------------------------------#
# Stage 2: Feature Engineering
#----------------------------------------#

# Define Scatterplot Function
targetscat <- function(feature){
    ggplot(train, aes(x = feature, y = SalePrice)) +
        geom_point(aes(color = SalePrice)) +
        scale_color_gradient(low = "darkgreen", high = "navy") 
}

targetscat(feature = train$OverallQual)
targetscat(feature = train$GrLivArea)
targetscat(feature = train$GarageCars)
targetscat(feature = train$GarageArea)
targetscat(feature = train$TotalBsmtSF)



#----------------------------------------#
# Divide features into categories
#----------------------------------------#

# Category 1: Garage 

# Category 2: Living Areas

# Category 3: Basement

# Category 4: Years / History / Misc

# Category 5: Outdoors

# Category 6: Materials 

# Category 7: Location / Environment
