# Generate dataset of leopard and tiger speeds

# Number of points per class
n <- 50

# Set seed for reproducibility
set.seed(1234)

# Generate random tiger speeds in km/hr
tiger_speed <- rnorm(n, mean = 50, sd = 3)

# Generate random leopard speeds in km/hr
leopard_speed <- rnorm(n, mean = 54, sd = 3)

dataset <- data.frame(speed = c(tiger_speed, leopard_speed),
                      class = c(rep("tiger", n), rep("leopard", n)),
                      stringsAsFactors = T)

# Shuffle rows
dataset <- dataset[sample(nrow(dataset)),]

# Print rows and columns
dim(dataset)

# Count instances in each class
table(dataset$class)

# Compute summary stats
summary(dataset)

# Plot data points.
#png("felineSpeeds.png", width = 7, height = 3, units = "in", res = 200)

plot(dataset$speed, rep(1, n*2),
     col=as.integer(dataset$class)+2,
     pch=as.integer(dataset$class),
     cex = 2,
     yaxt = 'n',
     xlab = "Speed",
     ylab = "",
     ylim = c(1,1.5),
     main = "Tiger and leopard speeds")

# Plot vertical lines at the mean.
abline(v = mean(tiger_speed), col = 4, lty = 2)
abline(v = mean(leopard_speed), col = 3, lty = 2)

# Add a legend.
legend("topleft", c("tiger","leopard"),
       pch = c(2,1),
       col=c(4,3))

#abline(v = mean(tiger.speed) +
#         (mean(leopard.speed)-mean(tiger.speed))/
#         2, col = "red", lty = 2)
#text(51.5,1.47,"decision\nboundary", cex=0.7, col="red")

#dev.off()

# Define a simple classifier that learns a centrality measure
# for each class
simple_model_train <- function(data, centrality = mean){
  
  # Store unique classes.
  classes <- unique(data$class)
  
  # Define an array to store learned parameters.
  params <- numeric(length(classes))
  
  # Make this a named array
  # names(params) labels the elements of the vector
  names(params) <- classes
  
  # Iterate thru each class and compute centrality measure
  for(c in classes){
    
    # Filter instances by class.
    tmp <- data[which(data$class == c),]
    
    # Compute the centrality measure
    centrality_measure <- centrality(tmp$speed)
    
    # Store the centrality measure for the class
    params[c] <- centrality_measure
  }
  
  return(params)
  
}

# Define a function that returns a class
# based on the learned parameters
simple_classifier_predict <- function(newdata, params){
  
  # Variable to store the predictions of
  # each instance in newdata
  predictions <- NULL
  
  # Iterate instances in newdata
  for (i in 1:nrow(newdata)){
    
    instance <- newdata[i,]
    
    # Predict the name of the class whose 
    # centrality measure is the closest
    pred <- names(which.min(abs(instance$speed - params)))
    
    predictions <- c(predictions, pred)
  }
  
  return(predictions)
}

classes <- unique(dataset$class)
params <- numeric(length(classes))
names(params) <- classes

#### Test the simple model ####

### Holdout validation ###

# Percent to be used as training
pctTrain <- 0.7

# set seed for reproducibility
set.seed(123)

idxs <- sample(nrow(dataset),
               size = nrow(dataset) * pctTrain,
               replace = F)

trainset <- dataset[idxs,]

testset <- dataset[-idxs,]

# Train the model using train set
params <- simple_model_train(trainset, mean)

params

# Predict classes on test set
test_predictions <- simple_classifier_predict(testset, params)

# Show first predictions
head(test_predictions)

# Compute test accuracy
sum(test_predictions == as.character(testset$class)) / nrow(testset)

# Compute train accuracy
train_predictions <- simple_classifier_predict(trainset, params)
sum(train_predictions == as.character(trainset$class)) / nrow(trainset)

#### k-fold cross validation ####

# Number of folds

k = 5

set.seed(123)

# Generate random folds
folds <- sample(k, 
                size = nrow(dataset),
                replace = T)

# Print how many instances ended up in each fold
table(folds)

# Variable to store accuracies on each fold.
test_accuracies <- NULL; train_accuracies <- NULL

for (i in 1:k){
  
  testset <- dataset[which(folds == i),]
  trainset <- dataset[which(folds != i),]
  
  params <- simple_model_train(trainset, mean)
  
  test_predictions <- simple_classifier_predict(testset, params)
  train_predictions <- simple_classifier_predict(trainset, params)
  
  # Accuracy on test set
  acc <- sum(test_predictions == as.character(testset$class)) /
    nrow(testset)
  
  test_accuracies <- c(test_accuracies, acc)
  
  # Accuracy on train set
  acc <- sum(train_predictions == as.character(trainset$class)) /
    nrow(trainset)
  
  train_accuracies <- c(train_accuracies, acc)
}

# Print mean accuracy across folds on test set
mean(test_accuracies)

# Train data accuracy
mean(train_accuracies)


### Add a third class ###

set.seed(123)

# Generate random jaguar speeds in km/hr
jaguar_speed <- rnorm(n, mean = 60, sd = 3)

# Generate dataset with 3 classes
dataset3classes <- rbind(dataset,
                         data.frame(speed = jaguar_speed,
                                    class = "jaguar"))

# Shuffle rows

dataset3classes <- dataset3classes[sample(nrow(dataset3classes)),]

# Percent to be used as trainng data

pctTrain <- 0.7

idxs <- sample(nrow(dataset3classes),
               size = nrow(dataset3classes) * pctTrain,
               replace = F)

trainset <- dataset3classes[idxs,]

testset <- dataset3classes[-idxs,]


# Train the model using trainset
params <- simple_model_train(trainset, mean)

# Predict the classes on the first set
predictions <- simple_classifier_predict(testset, params)

# Show first predictions
head(predictions)

# Compute accuracy
sum(predictions == as.character(testset$class)) / nrow(testset)


#### Simple regression ####

# Iterates thru each instance in newdata and returns
# the mean speed from params
simple_regression_predict <- function(newdata, params){
  
  # Variable to store the predictions of 
  # each instance in newdata
  predictions <- NULL
  
  # Iterate instances in new data
  for(i in 1:nrow(newdata)) {
    
    instance <- newdata[i,]
    
    # Return the mean value of the corresponding class stored in params
    pred <- params[which(names(params) == instances$class)]
    
    predictions <- c(predictions, pred)
  }
}
