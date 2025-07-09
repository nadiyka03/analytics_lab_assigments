# part 0: preparation
# loading the data set
fashion_data<- read_csv("~/documents/programming_code/labs_final_assigment/fashion_data.csv")
fashion_data <- as.data.frame(fashion_data)

fashion_data<- na.omit(fashion_data)
set.seed(123)
ids <- sample(1:nrow(fashion_data), 20000) 
fashion_data <- fashion_data[ids, ]

# exploring the data set
print(head(fashion_data))
print(ncol(fashion_data))
print(colnames(fashion_data))

# fixing the column for easier further work
fashion_data$`Review Count` <- as.numeric(fashion_data$`Review Count`)
colnames(fashion_data)[colnames(fashion_data) == "Review Count"] <- "Review.Count"

# calculating means of numerical columns
print(mean(fashion_data$Price))
print(mean(fashion_data$Age))
print(mean(fashion_data$Rating))
print(mean(fashion_data$Review.Count))

# calculating standard variations of numerical columns
print(sd(fashion_data$Price))
print(sd(fashion_data$Age))
print(sd(fashion_data$Rating))
print(sd(fashion_data$Review.Count))

# part 1: linear reggression
# computing correlations 
print(cor(fashion_data$Price, fashion_data$Age))
print(cor(fashion_data$Price, fashion_data$Rating))
print(cor(fashion_data$Price, fashion_data$Review.Count))

# linear regression model
model <- lm(Price ~ Age, fashion_data)
plot(fashion_data$Price, fashion_data$Age, 
     main = "Predicting price from review count",
     xlab = "Price, $", ylab = "Age",
     pch = 0.1, frame = FALSE)
abline(model, col="green")

# analyzing the model
summary(model)
# plotting residuals against the explanatory variable 
plot(fashion_data$Age, residuals(model), 
     main = "Ptotting residuals against Age column",
     xlab = "Age", ylab = "residuals",
     pch = 0.1, frame = FALSE)
# histogram of residuals 
hist(residuals(model),
     main = "Histogram of residuals",
     xlab = "residuals")

# building multivariate regression model
model_2 <- lm(Price ~ Age + Review.Count + Rating, data = fashion_data)

# analizing model_2
summary(model_2)

# plotting residuals against the explanatory variable 
plot(fashion_data$Review.Count, residuals(model_2), 
     main = "Ptotting residuals against Review Count column",
     xlab = "Rewiew Count", ylab = "residuals",
     pch = 0.1, frame = FALSE)

plot(fashion_data$Rating, residuals(model_2), 
     main = "Ptotting residuals against Rating column",
     xlab = "Rating", ylab = "residuals",
     pch = 0.1, frame = FALSE)
# histogram of residuals 
hist(residuals(model_2),
     main = "Histogram of residuals",
     xlab = "residuals")


# part 2: t-test 
# checking if there is enough data points in each group 
print(nrow(fashion_data[ fashion_data$Brand == "Tommy Hilfiger", ]))
print(nrow(fashion_data[ fashion_data$Brand == "Calvin Klein", ]))

# subselecting dataframe in two distinct groups 
hilfiger <- fashion_data[ fashion_data$Brand == "Tommy Hilfiger", ]
calvin_klein <- fashion_data[ fashion_data$Brand == "Calvin Klein", ]

# calculating the mean, standard deviation and median 
# for each numerical column for each group separated
mean(hilfiger$Price)
mean(hilfiger$Age)
mean(hilfiger$Review.Count)
mean(hilfiger$Rating)

mean(calvin_klein$Price)
mean(calvin_klein$Age)
mean(calvin_klein$Review.Count)
mean(calvin_klein$Rating)

median(hilfiger$Price)
median(hilfiger$Age)
median(hilfiger$Review.Count)
median(hilfiger$Rating)

median(calvin_klein$Price)
median(calvin_klein$Age)
median(calvin_klein$Review.Count)
median(calvin_klein$Rating)

sd(hilfiger$Price)
sd(hilfiger$Age)
sd(hilfiger$Review.Count)
sd(hilfiger$Rating)

# Price column
# checking the distribution
hist(hilfiger$Price,
     main = "Price distribution of items by Tommy hilfiger",
    xlab = "Price, $")
hist(calvin_klein$Price,
      main = "Price distribution of items by Calvin Klein",
      xlab = "Price, $")
# checking the varience
var(hilfiger$Price)
var(calvin_klein$Price)
# t-test
t.test(hilfiger$Price, calvin_klein$Price, alternative = "two.sided")

# Review.Count column
# checking the distribution
hist(hilfiger$Review.Count,
     main = "Review.Count distribution of items by Tommy hilfiger",
     xlab = "Review.Count")
hist(calvin_klein$Review.Count,
     main = "Review.Count distribution of items by Calvin Klein",
     xlab = "Review.Count")
# checking the varience
var(hilfiger$Review.Count)
var(calvin_klein$Review.Count)
# t-test
t.test(hilfiger$Review.Count, calvin_klein$Review.Count, alternative = "two.sided")





