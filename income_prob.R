library(rpart)

#Reading in the data
income_data <- read.table('Income_Data.txt', sep= ",", col.names = c("annual_income", "sex", "marital_status", "age", "education", "occupation", "bay_area", "dual_income", "house_size", "children", "householder", "house_type", "ethnicity", "language"))

#Telling which parameters are categorical, numerical, etc
attach(income_data)
my_income <- data.frame(as.numeric(annual_income), as.factor(sex), as.factor(marital_status),
as.ordered(age), as.ordered(education), as.factor(occupation),
as.factor(bay_area), as.factor(dual_income), as.ordered(house_size),
as.ordered(children), as.factor(householder),as.factor(house_type),
as.factor(ethnicity),as.factor(language))
detach(income_data)
colnames(my_income) <- colnames(income_data)

#Run rpart with cross validation
mah_tree <- rpart(formula = annual_income ~ ., data = my_income, control = rpart.control(cp = 0.001), method = 'anova')

#Plotting the resulting tree
plot(mah_tree, uniform=TRUE, main="Income Predicting Tree")
text(mah_tree, use.n=TRUE, all=TRUE, cex=.5)


#Part (a)
sampled_ints <- sample(1:nrow(my_income), nrow(my_income), replace=FALSE)
train_sample <- my_income[sampled_ints[1:6295] , ]
test_sample <- my_income[sampled_ints[6296:nrow(my_income)], ]

#Fitting tree to train data
train_tree <- rpart(formula = annual_income ~ ., data = train_sample, control = rpart.control(cp = 0.001104), method = 'anova')

#Plotting the resulting training tree
#plot(train_tree, uniform=TRUE, main="Income Predicting Tree")
#text(train_tree, use.n=TRUE, all=TRUE, cex=.3)

#Predicting for test data set on various trees
#Extract complexity parameters from cptable
cp = train_tree$cptable[ ,1]
n_cp = length(cp)
cp_error = array(0, n_cp)

for (i in 1:n_cp) {
	new_train_tree <- prune(train_tree, cp = cp[i])
	pred = predict(new_train_tree, test_sample)
	#Determine square error loss
	cp_error[i] = sqrt(sum((pred - test_sample[,1])^2))
}
#plot(cp, cp_error, type='l')

#Part C
#Using values for myself to predict my own annual income using the tree obtained in part (a)
my_stats <- c(2, 1, 5, 2, 6, 6, 3, 1, 1, 0, 2, 3, 8, 1)
my_income[1, ] <- my_stats
my_prediction <- predict(train_tree, my_income[1,])

