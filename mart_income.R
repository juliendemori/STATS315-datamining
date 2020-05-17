#Importing data for Income problem
income = read.table('Income_Data.txt', sep=',')
names(income) <- c("annual_income", "sex", "marital_status", "age", "education", "occupation", "bay_area", "dual_income", "house_size", "children", "householder", "house_type", "ethnicity", "language")

#Declaring types of each variable
type = c(2,2,2,2,2,2,2,1,1,2,2,2,2)

#To be able to compare to HW1, have to sample a training and test set
sampled_ints <- sample(1:nrow(income), nrow(income), replace=FALSE)
train_sample <- income[sampled_ints[1:6295], ]
test_sample <- income[sampled_ints[6295:nrow(income)], ]

x_train = as.matrix(train_sample[,2:14])
x_test = as.matrix(test_sample[,2:14])
y_train = train_sample[,1]
y_test = test_sample[,1]

#Fitting a mart model
mart(x_train, y_train, type)
#run more to see if a higher number of iterations yields better accuracy
moremart()

par(mar=c(4,6,4,4)+0.1)
varimp()

#Show training and testing error
progress()

#Predicting using this model
pred <- martpred(x_test)
diff = abs(pred - y_test)
avErr = mean(diff)



#Repeating the same process using CART (and the same sampled integers to maintain consistency)
#Do the same thing with CART to see which predicts better
library(rpart)

income_data <- read.table('/Users/juliendemori/Documents/Academics/STATS_315B/Income_Data.txt', sep= ",", col.names = c("annual_income", "sex", "marital_status", "age", "education", "occupation", "bay_area", "dual_income", "house_size", "children", "householder", "house_type", "ethnicity", "language"))


#Framing data
income_data$sex <- as.factor(income_data$sex)
income_data$marital_status <- as.factor(income_data$marital_status)
income_data$occupation <- as.factor(income_data$occupation)
income_data$dual_income <- as.factor(income_data$dual_income)
income_data$householder <- as.factor(income_data$householder)
income_data$house_type <- as.factor(income_data$house_type)
income_data$ethnicity <- as.factor(income_data$ethnicity)
income_data$language <- as.factor(income_data$language)
income_data$bay_area <- as.factor(income_data$bay_area)
income_data$age <- as.ordered(income_data$age)
income_data$education <- as.ordered(income_data$education)
income_data$children <- as.ordered(income_data$children)

#Obtaining accurately framed training and testing samples for CART
new_train <- income_data[sampled_ints[1:6295],]
new_test <- income_data[sampled_ints[6296:nrow(income_data)],]
train_tree <- rpart(formula = annual_income ~ ., data = new_train, control = rpart.control(cp = 0), method = 'anova')

#Prune to the optimal CP parameter found in HW1
pruned <- prune(train_tree, cp = 0.0018)

#Predict using this rpart tree
ycart <- predict(pruned, new_test)

#Mean absolute error
avErrNew = mean(abs(ycart - new_test[,1]))