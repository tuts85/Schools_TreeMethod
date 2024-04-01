install.packages("ISLR")
library(ISLR)

head(College)

# load datafram
df <- College

# EDA
install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(Room.Board, Grad.Rate, color=Private)) + geom_point()

ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill=Private), color = 'black', bins = 50)

ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill=Private), color = 'black', bins = 50)

# Changing all grad rates above 100% to 100%

# Find colleges with graduation rate above 100%
above_100 <- df$Grad.Rate > 100

# Update graduation rate to 100% for colleges with rate above 100%
df$Grad.Rate[above_100] <- 100

# split in train and test
install.packages("caTools")
library(caTools)

# Set seed for reproducibility
set.seed(101)

# Split the data into train (70%) and test (30%) sets
split <- sample.split(df$Private, SplitRatio = 0.7)

# Create training and testing sets
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

## decision tree
install.packages("rpart")
library(rpart)

tree <- rpart(Private ~., method = 'class', data=train_data)

printcp(tree)

plot(tree, uniform = TRUE, main = 'Private School tree')
text(tree, use.n = T, all=T)

# predicting on test
predict.private <- as.data.frame(predict(tree, test_data))

head(predict.private)

predited.values <- ifelse(predict.private$Yes > 0.5, 'Yes', 'No')

table(test_data$Private, predited.values)

install.packages("rpart.plot")
library(rpart.plot)

prp(tree)

# Random Forest
install.packages("randomForest")
library(randomForest)
help("randomForest")

set.seed(101)

rf.model <- randomForest(Private ~. , data= train_data, importance = TRUE)

rf.model$confusion

rf.model$importance

rf.prediction <-predict(rf.model, test_data)

head(rf.prediction)

table(test_data$Private, rf.prediction)
