#Spam classification problem, loading data to be trained on
spam = read.table('/Users/juliendemori/Documents/Academics/STATS_315B/Spam_Train.txt', sep = ',')
spamOnot = spam[,58]
x = spam[,1:57]
x = as.matrix(x)
lx = rep(1,57)
#Reading in test data
test = read.table('/Users/juliendemori/Documents/Academics/STATS_315B/Spam_Test.txt', sep = ',')
xtest = test[,1:57]
xtest = as.matrix(xtest)
ytest = test[,58]

#Running the MART classification model
mart(x, spamOnot, lx, martmode = "class")
moremart()
moremart()
dev.new()
progress()
L = length(pred)

pred = martpred(xtest, probs = F)
good = sum((pred == 1) & (ytest == 0))/sum(ytest == 0)
bad = sum((pred == 0) & (ytest == 1))/sum(ytest == 1)

#Total error rate
error = mean(abs(pred - ytest))


#Determining the number of misclassification of each class
# dev.new()
# classerrors()


#Now we want to determine the optimal cost matrix
#Experimenting with different values of k
k = 6
spam.cost = matrix(c(0,k,1,0),2)
mart(x, spamOnot, lx, martmode = "class", cost.mtx=spam.cost)

newPred = martpred(xtest, probs = F)
misclassGood = sum((newPred == 1) & (ytest == 0))/(sum(newPred == 1))
misclassBad = sum((newPred == 0) & (ytest == 1))/(sum(newPred == 0))

# classerrors()
# dev.new()
# classerrors(class = 1)
dev.new()
varimp()
# dev.new()
# varimp(class = 1)
# dev.new()
# varimp(class = 2)

dev.new()
attach(mtcars)
par(mfrow=c(2,2))
singleplot(class = 1, var = 52)
singleplot(class = 1, var = 25)
singleplot(class = 1, var = 53)
singleplot(class = 1, var = 7)

