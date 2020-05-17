library(rpart)
housetype_data <- read.table("Housetype_Data.txt", sep=",", col.names = c("house_type", "sex", "marital_status", "age", "education", "occupation", "ann_house_income", "bay_area", "dual_income", "house_size", "children", "householder", "house_type", "ethnicity", "language"))

attach(income_data)
my_housetype <- data.frame(as.factor(house_type), as.factor(sex), as.factor(marital_status),
as.ordered(age), as.ordered(education), as.factor(occupation), as.numeric(ann_house_income),
as.factor(bay_area), as.factor(dual_income), as.ordered(house_size),
as.ordered(children), as.factor(householder),as.factor(house_type),
as.factor(ethnicity),as.factor(language))
detach(income_data)

