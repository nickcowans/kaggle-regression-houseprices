##########################
##   Feature Analysis   ##
##   Titanic Dataset    ##
##########################

os <- "win" ## Specofy operating system as "osx" or "win"

# Set up the Working Directory and libraries

ifelse(os == "osx",
  setwd("/Users/Thomas/Dropbox/Data Science/Kaggle/competition-titanic"),
  setwd("C:/Users/Thomas/Dropbox/Data Science/Kaggle/competition-titanic")
)

library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)
library(mice)

# Import the data and create initial datasets
train <- read.csv('train.csv')
test <- read.csv('test.csv')
temp <- mutate(test, Survived = NA, Source = 'Test')
temp2 <- mutate(train, Source = 'Train')
all_data <- rbind(temp2, temp)
rm(temp, temp2)


#----------------------------------#
# Stage 1 Analysis:
#   Univariate Feature Analysis
#----------------------------------#

# Analysis for Age

ggplot(data = train, aes(Age, fill = Survived)) +
    geom_histogram(data = subset(train, Survived == 1),
                   bins = 25, alpha = 0.2, fill = 'darkgreen') +
    geom_histogram(data = subset(train, Survived == 0),
                   bins = 25, alpha = 0.2, fill = 'red')
    

summary(subset(train$Age, train$Survived == 1))
summary(subset(train$Age, train$Survived == 0))


model <- lm(data = train, Survived ~ Age)
summary(model)$r.squared
model <- lm(data = train, Survived ~ log(Age))
summary(model)$r.squared

# From the Analysis, the log of Age appears more appropriate

all_data$Age <- log(all_data$Age)


# Analysis for Name

# Grab title from passenger names
all_data$Title <- gsub('(.*, )|(\\..*)', '', all_data$Name)

#Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
all_data$Title[all_data$Title == 'Mlle']        <- 'Miss' 
all_data$Title[all_data$Title == 'Ms']          <- 'Miss'
all_data$Title[all_data$Title == 'Mme']         <- 'Mrs' 
all_data$Title[all_data$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex
table(all_data$Sex, all_data$Title)





# Analysis for Family


table(all_data$Survived, all_data$SibSp)

ggplot(data = train, aes(x = SibSp + Parch, fill = factor(Survived))) + 
  geom_bar(position = 'dodge')



# Assess Various Predictive Regression Models


model <- lm(data = train, Survived ~ SibSp + Parch)
summary(model)$r.squared


model <- lm(data = train, Survived ~ SibSp + Parch + SibSp^2 + Parch^2)
summary(model)$r.squared



# Feature Selection Assessment:
  # Use log(Age)
  # Use title
  # Retain other variables

titanic <- all_data
titanic <- subset(titanic, select = -c(Name, Cabin, Ticket))

#----------------------------------#
# Stage 2:
#   Handling Missing Values
#   Data Prep for ML
#----------------------------------#



# Do a "dirty Multiple Imputation by Chained Equations" 
impute_data <- subset(titanic, select = -c(Survived))
imp <- mice(impute_data, method = "norm.predict", m = 10)
export_data <- complete(imp)

titanic <- (cbind(export_data, titanic$Survived))
colnames(titanic)[11] <- "Survived"


# Create indicator variables

for(level in unique(titanic$Pclass)){
  titanic[paste("dummy", level, sep = "_")] <- ifelse(titanic$Pclass == level, 1, 0)
}

for(level in unique(titanic$Embarked)){
  titanic[paste("dummy", level, sep = "_")] <- ifelse(titanic$Embarked == level, 1, 0)
}

for(level in unique(titanic$Title)){
  titanic[paste("dummy", level, sep = "_")] <- ifelse(titanic$Title == level, 1, 0)
}

titanic$Sex <- as.numeric(as.factor(titanic$Sex))
titanic$Sex <- ifelse(titanic$Sex == 2, 0, 1)

titanic <- subset(titanic, select = -c(Title, Pclass, Embarked))


# Convert Factors to Integers
#titanic$Sex <- as.integer(factor(titanic$Sex))
#titanic$Embarked <- as.integer(factor(titanic$Embarked))
#titanic$Title <- as.integer(factor(titanic$Title))

mod_test <- subset(titanic, Source == "Test" ,select =  -c(Source))
mod_train <- subset(titanic, Source == "Train" , select = -c(Source))



#----------------------------#
# Export the Data
#----------------------------#

write.csv(x = mod_test, file = "mod_test.csv", row.names = TRUE)
write.csv(x = mod_train, file = "mod_train.csv", row.names = TRUE)










