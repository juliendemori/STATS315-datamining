library(rpart)

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


#Framing data
# income_data$sex <- as.factor(income_data$sex)
# income_data$marital_status <- as.factor(income_data$marital_status)
# income_data$occupation <- as.factor(income_data$occupation)
# income_data$dual_income <- as.factor(income_data$dual_income)
# income_data$householder <- as.factor(income_data$householder)
# income_data$house_type <- as.factor(income_data$house_type)
# income_data$ethnicity <- as.factor(income_data$ethnicity)
# income_data$language <- as.factor(income_data$language)
# income_data$bay_area <- as.factor(income_data$bay_area)
# income_data$age <- as.ordered(income_data$age)
# income_data$education <- as.ordered(income_data$education)
# income_data$children <- as.ordered(income_data$children)

sampled_ints <- sample(1:nrow(income_data), nrow(income_data), replace=FALSE)
new_train <- my_income[sampled_ints[1:6295],]
new_test <- my_income[sampled_ints[6296:nrow(income_data)],]
train_tree <- rpart(formula = annual_income ~ ., data = new_train, control = rpart.control(cp = 0), method = 'anova')