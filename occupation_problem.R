#Occupation problem, importing data
occupation = read.table('/Users/juliendemori/Documents/Academics/STATS_315B/Occupation_Data.txt', sep = ',')
names(occupation) <- c("occupation", "hometype", "sex", "maritalstat", "age", "education", "annincome", "bayarea", "dualincome", "housemembers", "childrenhouse", "householder", "ethnicity", "language")
occ = occupation[,1]
x = occupation[,2:14]
x = as.matrix(x)

#Declaring the types of each variable
type = c(2,2,2,1,1,1,2,2,2,1,1,2,2,2)

#Running a multi-class-ification mart
mart(x, occ, type, niter = 200, martmode="class")
#To see the dependence of misclassification on number of iterations
progress()

#To see what percentage total were misclassified, and what fraction were from each of the other classes
dev.new()
classerrors()
dev.new()
classerrors(class = 1)

# #To see which of each classes were misclassified, and what they were classified as
dev.new()
attach(mtcars)
par(mfrow=c(3,3))
for (j in 1:9) {
	classerrors(class = j)
}
	
	

# #All the misclassification errors of each class 

# #Meaning gained from each variable
dev.new()
par(mar=c(4,6,4,4)+0.1)
varimp()

# #Obtaining the important variables for making predictions on each class.
dev.new()
attach(mtcars)
par(mfrow=c(3,3))
for (j in 1:9) {
	par(mar=c(4,6,4,4)+0.1)
	varimp(class = j)
}

#Plots of dependencies
#Class 1
dev.new()
attach(mtcars)
par(mfrow=c(4,4))
for (j in 1:13) {
	par(mar=c(4,6,4,4)+0.1)
	singleplot(class = 6, var = j)
}

#Class 6
dev.new()
attach(mtcars)
par(mfrow=c(3,3))
for (j in 1:13) {
	par(mar=c(4,6,4,4)+0.1)
	singleplot(class = 1, var = j)
}


#We now make some pairplots to note any correlation between the most important variables, in predicting the class of class 6, and also class 1
dev.new()
attach(mtcars)
par(mfrow=c(2,3))
pairplot(class = 6, 4, 5)
pairplot(class = 6, 4, 6)
pairplot(class = 6, 5, 6)
pairplot(class = 6, 4, 10)
pairplot(class = 6, 5, 10)
pairplot(class = 6, 6, 10)

dev.new()
attach(mtcars)
par(mfrow=c(2,3))
pairplot(class = 1, 4, 5)
pairplot(class = 1, 4, 6)
pairplot(class = 1, 5, 6)
pairplot(class = 1, 4, 10)
pairplot(class = 1, 5, 10)
pairplot(class = 1, 6, 10)

