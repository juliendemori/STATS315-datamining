library(rpart)

#Loading in the data into a table
big_income_data <- read.table("Income_Big.txt", sep=",", col.names = c("annual_income", "sex", "marital_status", "age", "education", "occupation", "bay_area", "dual_income", "house_size", "children", "householder", "house_type", "ethnicity", "language", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"))

#Part (A)
#Framing data
attach(big_income_data)
my_big_income <- data.frame(as.numeric(annual_income), as.factor(sex), as.factor(marital_status),
as.ordered(age), as.ordered(education), as.factor(occupation),
as.factor(bay_area), as.factor(dual_income), as.ordered(house_size),
as.ordered(children), as.factor(householder),as.factor(house_type),
as.factor(ethnicity),as.factor(language), as.factor(one), as.factor(two), as.factor(three), as.factor(four), as.factor(five), as.factor(six), as.factor(seven), as.factor(eight), as.factor(nine), as.factor(ten))
detach(big_income_data)
colnames(my_big_income) <- colnames(big_income_data)


#Fitting a tree to this large data set
mah_big_tree <- rpart(formula = annual_income ~ ., data = my_big_income, control = rpart.control(cp = 0.001), method = 'anova')

sampled_ints <- sample(1:nrow(my_big_income), nrow(my_big_income), replace=FALSE)
train_sample <- my_big_income[sampled_ints[1:6295] , ]
test_sample <- my_big_income[sampled_ints[6296:nrow(my_big_income)], ]

#Fitting tree to train data
train_tree <- rpart(formula = annual_income ~ ., data = train_sample, control = rpart.control(cp = 0.001), method = 'anova')

#Plotting the resulting training tree
# plot(train_tree, uniform=TRUE, main="Income Predicting Tree")
# text(train_tree, use.n=TRUE, all=TRUE, cex=.3)

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

#Part (B)

#Reading in the data
income_data <- read.table("Income_Data.txt", sep= ",", col.names = c("annual_income", "sex", "marital_status", "age", "education", "occupation", "bay_area", "dual_income", "house_size", "children", "householder", "house_type", "ethnicity", "language"))

#Telling which parameters are categorical, numerical, etc
attach(income_data)
my_income <- data.frame(as.numeric(annual_income), as.factor(sex), as.factor(marital_status),
as.ordered(age), as.ordered(education), as.factor(occupation),
as.factor(bay_area), as.factor(dual_income), as.ordered(house_size),
as.ordered(children), as.factor(householder),as.factor(house_type),
as.factor(ethnicity),as.factor(language))
detach(income_data)
colnames(my_income) <- colnames(income_data)

n_in <- length(my_income)

#Sampling a few times from the big income dataframe
samp_one <- my_income[sample(1:n_in, n_in, replace = TRUE)]
samp_two <- my_income[sample(1:n_in, n_in, replace = TRUE)]
samp_three <- my_income[sample(1:n_in, n_in, replace = TRUE)]
samp_four <- my_income[sample(1:n_in, n_in, replace = TRUE)]
samp_five <- my_income[sample(1:n_in, n_in, replace = TRUE)]
samp_six <- my_income[sample(1:n_in, n_in, replace = TRUE)]
samp_seven <- my_income[sample(1:n_in, n_in, replace = TRUE)]
samp_eight <- my_income[sample(1:n_in, n_in, replace = TRUE)]


#Running Rpart on each of these data frames
mah_one <- rpart(formula = annual_income ~., data = samp_one, control = rpart.control(cp = 0.001), method = 'anova')
mah_two <- rpart(formula = annual_income ~., data = samp_two, control = rpart.control(cp = 0.001), method = 'anova')
mah_three <- rpart(formula = annual_income ~., data = samp_three, control = rpart.control(cp = 0.001), method = 'anova')
mah_four <- rpart(formula = annual_income ~., data = samp_four, control = rpart.control(cp = 0.001), method = 'anova')
mah_five <- rpart(formula = annual_income ~., data = samp_five, control = rpart.control(cp = 0.001), method = 'anova')
mah_six <- rpart(formula = annual_income ~., data = samp_six, control = rpart.control(cp = 0.001), method = 'anova')
mah_seven <- rpart(formula = annual_income ~., data = samp_seven, control = rpart.control(cp = 0.001), method = 'anova')
mah_eight <- rpart(formula = annual_income ~., data = samp_eight, control = rpart.control(cp = 0.001), method = 'anova')

dev.new()
plot(mah_four, uniform=TRUE, main ="four")
text(mah_four, use.n=TRUE, all=TRUE, cex=0.6)

dev.new()
plot(mah_five, uniform=TRUE, main ="five")
text(mah_five, use.n=TRUE, all=TRUE, cex=0.6)