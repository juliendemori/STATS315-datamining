library(rpart)
housetype_data <- read.table("Housetype_Data.txt", sep=",", col.names = c("house_type", "sex", "marital_status", "age", "education", "occupation", "house_income", "bay_area", "dual_income", "house_size", "children", "householder", "ethnicity", "language"))

#Making the data frame and declaring the appropriate variables to be factors
my_housetype=data.frame(housetype_data)
for(i in c(1,2,3,6,8,9,11,12,13,14)){
  my_housetype[,i]=as.factor(housetype_data[,i])
}

#Determining a tree to predict housetype.
mah_house <- rpart(formula = house_type ~., data = my_housetype, control = rpart.control(cp = 0.0005), method = 'class')

#Plotting the tree
plot(mah_house, uniform=TRUE, main ="My house-type predicting tree")
text(mah_house, use.n=TRUE, all=TRUE, cex=0.6)