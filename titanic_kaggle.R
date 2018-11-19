#Load raw data 
library(readr)

<<<<<<< HEAD
train <- read.csv("R/Kaggle-Titanic/train.csv", header = TRUE)
test <- read.csv("R/Kaggle-Titanic/test.csv", header = TRUE)
=======
train <- read.csv("R/train.csv", header = TRUE)
test <- read.csv("R/test.csv", header = TRUE)
>>>>>>> b808cd6aaa92c7c4fde4facae882ee7c45aa13d6

#Add a "survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived= rep("None",nrow(test)),test[,])
#Combine data sets 
data.combined <- rbind(train,test.survived)

#A bit about R data types 
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass<- as.factor(data.combined$Pclass)

# Take a look at gross survival rates 

table(data.combined$Survived)

#Distribution across classes 

table(data.combined$Pclass)

# ggplot2 package for visualization

library(ggplot2)

# Hypothesis - Rich people survided at a higher rate 

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +  stat_count(width = 0.5) + xlab("Pclass") + ylab("Total Count") + labs(fill="Survived")

# Examine the first few names in the training data set 

head(as.character(train$Name))

# How many unique nams are there across both train & test ? 
length(unique(as.character(data.combined$Name)))

# Two duplicates names it is a good place to check 
# First, we get the duplicate names and store tham as an array 
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Then, we take a look ate the records in hte combined data set 

data.combined[which(data.combined$Name %in% dup.names),]

#What is up with the 'Miss' and 'Mr.' thing ?  
library(stringr)

# Check any correlation with other variables

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]
mrses[1:5,]

#Check out males to check any pattern 

males <-data.combined[which(train$Sex == "male"),]
males[1:5,]

# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
# NOTE - Using the grep function here, but could have used the str_detect function as well.
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}


# NOTE - The code below uses a for loop which is not a very R way of
#        doing things
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)


# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# What's the distribution of females to males across trains & test?
table(data.combined$Sex)

# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"age"])

# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master. " is a good proxy for male children 
masters <- data.combined[which(data.combined$title == "Master."),]
summary(masters$Age)

misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")
<<<<<<< HEAD

# Appears, that female children may have different survival rate, 
# could be a candidate for a feature engineering later 

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch ==0),]
summary(misses.alone$Age )
length(which(misses.alone$Age <= 14.5))

# Move on to the Sibsp variable and summarize the variable

summary(data.combined$SibSp)

#Treat as a factor

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

#Visualization of survival rate (Sibsp, Pclass and title)

ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Treat the parch vaiable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
=======
>>>>>>> b808cd6aaa92c7c4fde4facae882ee7c45aa13d6
