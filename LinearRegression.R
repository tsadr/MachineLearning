#Mohammad Jafari Sadr
#First Machine Learning HomeWork(Third Question)
computeCost = function(x_train, y_train, theta) {
	m =length(y_train)
	j <- sum((x_train%*%theta- y_train)^2)/(2*m)
	return(j)
}

linearRegressionFit = function(x_train, y_train, alpha, threshold=0, max_iter, plotJ=FALSE) {
	m = length(y_train)
	theta = rep(0,ncol(x_train))
	j_list = rep(0,max_iter)
	for(i in 1:max_iter){
		 theta = theta - alpha*(1/m)*(t(x_train)%*%(x_train%*%theta - y_train))
		j_list[i] = computeCost(x_train, y_train, theta)
	}
	if(plotJ){
		plot(1:num_iters, j_list, type = 'l',xlab="Iteration",ylab="J")
	}
	return(theta)
}

linearRegressionPredict = function(coefficients, x_test){
	y_predict = coefficients %*% x_test
	return(y_predict)
}

evaluate = function(y_test,y_predict){
	sse = sum((y_predict - y_test)^2)
	sst = sum((mean(y_test) - y_test)^2)
	r2 = 1-(sse/sst)
	rmse = sqrt(mean(y_test - y_predict)^2)
	return(list("RMSE" = rmse, "R2" = r2))
}

linearRegressionFit1 = function(x_train, y_train, alpha, threshold=0, max_iter, plotJ=FALSE, regularization=FALSE, landa=0) {
	m = length(y_train)
	theta = rep(0,ncol(x_train))
	j_list = rep(0,max_iter)
	for(i in 1:max_iter){
		if(regularization){
			theta = theta - alpha*(1/m)*(t(x_train)%*%(x_train%*%theta - y_train)) + (landa/m)*theta
		} else {
			theta = theta - alpha*(1/m)*(t(x_train)%*%(x_train%*%theta - y_train))
		}
		
		j_list[i] = computeCost(x_train, y_train, theta)
	}
	if(plotJ){
		plot(1:num_iters, j_list, type = 'l',xlab="Iteration",ylab="J")
	}
	return(theta)
}
