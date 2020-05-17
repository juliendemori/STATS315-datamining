#Loading in the neural network library
library(nnet)

#Reading in the data into a frame (all numerical variables)
spam_train <- read.table('/Users/juliendemori/Documents/Academics/STATS_315B/Spam_Train.txt', sep = ',')
spam_test <- read.table('/Users/juliendemori/Documents/Academics/STATS_315B/Spam.Test.txt', sep = ',')


xtrain = spam_train[,1:57]
ytrain = as.factor(spam_train[,58])
xtest = spam_test[,1:57]
ytest = as.factor(spam_test[,58])


#Now we want to standardize all the predictor variables (xtrain and xtest)
mtrain = vector(length = 57)
sdtrain = vector(length = 57)
mtest = vector(length = 57)
sdtest = vector(length = 57)

for (i in 1:57) {
	mtrain = mean(xtrain[,i])
	mtest = mean(xtest[,i])
	sdtrain = sd(xtrain[,i])
	sdtest = sd(xtest[,i])
	xtrain[,i] = (xtrain[,i] - mtrain)/sdtrain
	xtest[,i] = (xtest[,i] - mtest)/sdtest
}


#Framing the training data
train = data.frame(xtrain, ytrain)

#Value of interest
N = length(ytest)


# #//////////////////////////////////////////////////////////////////
# #Part (A)
# #Now we run the neural network 10 times for each of 1 through 10 hidden weights and average the misclassification error
# #for each number of hidden weights, to determine the optimal number 
# av_err = vector(length = 10)
# for (i in 1:10) {
	# errors = vector(length = 10)
	# for (j in 1:10) {
		# neural <- nnet(ytrain ~ ., train, size = i, rang = 0.5, maxit = 1000)	
		# ypred <- predict(neural, xtest, type = "raw")
		# ypred <- (ypred > 0.5)
		# errors[j] = sum(ytest != (ypred + 0))/N; 
	# }
	# av_err[i] = mean(errors)
# }


# #Plotting the error
# plot(1:10, av_err, main = "Average error vs number of hidden units", ylab = "Average test error", xlab = "Hidden units")
# lines(av_err)



# #/////////////////////////////////////////////////////////////////////
# #Part (B)
# #In this part we are computing the optimal decay parameter for the neural network
# b_av_err = vector(length = 11)
# for (i in 1:11) {
	# b_errors = vector(length = 10)
	# for (j in 1:10) {
		# neural <- nnet(formula = ytrain ~ ., data = train, size = 4, rang = 0.5, decay = (i-1)/10, maxit = 1000)	
		# ypred <- predict(neural, xtest, type = "raw")
		# ypred <- (ypred > 0.5)
		# b_errors[j] = sum(ytest != (ypred + 0))/N; 
	# }
	# b_av_err[i] = mean(b_errors)
# }


# #Plotting the error
# rang = range(b_av_err)
# plot(b_av_err, type = 'o', ylim = rang, axes = FALSE, ann = FALSE)
# title(main = "Average error vs weight parameter", font.main = 4)
# axis(1, at = 1:11, lab = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# axis(2, lab = TRUE)
# title(xlab = "Decay parameter")
# title(ylab = "Average Error for 10 runs with this weight parameter")

#/////////////////////////////////////////////////////////////////////
#Part (C)
#Here we are computing the optimal threshold value for our spam filter, so that non-spam misclassification is less than 1%
#I had already guessed a few values in the 0.5 - 0.7 threshold range to determine that I needed to go higher than that.
#The only values printed are the ones obtained from the following code.

thresh = seq(0.9, 0.95, 0.01)
L = length(thresh)
non_spam_err = vector(length = L)
spam_err = vector(length = L)
total_err = vector(length = L)
for (i in 1:L) {
	error = vector(length = L)
	spamer = vector(length = L)
	nospamer = vector(length = L)
	for (j in 1:10) {
		neural <- nnet(ytrain ~ ., train, size = 4, rang = 0.5, decay = 0.3, maxit = 1000)
		ypred <- predict(neural, xtest, type = "raw")
		ypred <- (ypred > thresh[i])
		error[j] = sum(ytest != (ypred + 0))/N
		nospamer[j] = sum((ytest == 0) & ((ypred + 0) == 1))/(sum(ytest == 0))
		spamer[j] = sum((ytest == 1) & ((ypred+ 0) == 0))/(sum(ytest == 1))
	}
	non_spam_err[i] = mean(nospamer)
	spam_err[i] = mean(spamer)
	total_err[i] = mean(error)
}


# #BONUS
# #Here I want to run 10 neural networks for 10 iterations each with different starting weights, and then use those weights as the new starting weights for the next 10 neural networks, until there has been no new minimum testing error for 50 iterations

# #number of weights
# nw = 178

# #matrix to store weights
# W <- matrix(0,nw,10)
# for (i in 1:10) {
	# W[,i] <- runif(nw, -0.5, 0.5)
# }

# total_error <- c()
# while(TRUE) {
	# errors = vector(length = 10)
	# for (j in 1:10) {
		# neural <- nnet(ytrain ~ ., data = train, size = 3, Wts = W[,j], maxit = 10)
		# ypred <- predict(neural, xtest, type = "raw")
		# ypred <- (ypred > 0.5)
		# errors[j] <- sum(ytest != (ypred + 0))/N
		# W[,j] <- neural$wts
	# }
	# new_err <- mean(errors)
	# total_error <- c(total_error, new_err)
	# if (length(total_error) > 5) {
		# if (min(total_error) != min(tail(total_error, 5))) {
			# break
		# }
	# }
# }

# #Plotting the errors
# niter <- seq(10, 10*length(total_error), 10)
# plot(niter, total_error, main = "Average test error vs number of iterations", xlab = "Average test error", ylab = "Number of iterations")
# lines(niter, total_error)

