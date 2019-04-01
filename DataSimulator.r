Data_Simulator <- function(data, n, unique.num, outliers.remove){

require(doParallel)

if(n < 1){return("n must be a positive integer")}
if(unique.num < 1){return("unique.num must be a positive integer")}

#Identify numerical and categorical predictors
#Low unique-count numerics will be turned into factors
#Also identifies integer predictors

data <- as.data.frame(data)
if(length(colnames(data)) != ncol(data)){colnames(data) <- paste("p", c(1:ncol(data)), sep = "")}

types <- sapply(data, is.numeric)

numerics.index <- which(types)
factors.index <- c(1:ncol(data))[-numerics.index]

fake_numerics <- c()

for(i in 1:length(numerics.index)){

	if(length(unique(data[,numerics.index[i]])) <= unique.num){fake_numerics <- c(fake_numerics, i)}

}


if(length(fake_numerics) != 0){

	factors.index <- sort(c(factors.index, numerics.index[fake_numerics]))
	numerics.index <- numerics.index[-fake_numerics]

}

integers_only <- c()

for(i in 1:length(numerics.index)){

	index.temp <- which(!is.na(data[,numerics.index[i]]))

	if(length(which(data[index.temp, numerics.index[i]] %% 1 == 0)) >= 0.975*length(index.temp)){

		integers_only <- c(integers_only, numerics.index[i])

		}

}


if(length(factors.index) != 0){

	data[,factors.index] <- lapply(data[,factors.index], factor)
	data <- as.data.frame(data)

}


#Impute missing values

if(length(which(is.na(data))) != 0){

	require(missForest)

	cl <- makeCluster(detectCores() - 2) 
	clusterEvalQ(cl, library(foreach))
	registerDoParallel(cl)

	data.imputed <- missForest(data, maxiter = 10, ntree = 100, parallelize = c("forests"), verbose = TRUE)

	stopCluster(cl)

	data <- data.imputed$ximp

}

if(length(integers_only) != 0){data[,integers_only] <- round(round(data[,integers_only]))}


#Obtain a numerical equivalent of the original data

data_matrix <- matrix(nrow = nrow(data), ncol = ncol(data))
data_matrix[,numerics.index] <- as.matrix(data[,numerics.index])

if(length(factors.index) != 0){

	unique_vals <- list()

	for(i in 1:length(factors.index)){

		vals <- levels(data[,factors.index[i]])
		unique_vals[[i]] <- vals

		for(j in 1:length(vals)){

			data_matrix[which(data[,factors.index[i]] == vals[j]),factors.index[i]] <- j

		}

	}

}


#Obtain independent components via PCA to be used for the simulation

PCA <- prcomp(data_matrix, center = FALSE, scale = FALSE)

PC_matrix <- PCA$x
Transformation_matrix <- solve(PCA$rotation)

final_data <- as.data.frame(matrix(nrow = 1, ncol = ncol(data)))
colnames(final_data) <- colnames(data)

final_PC <- matrix(nrow = 1, ncol = ncol(data))


#Simulation

repeat{

if(nrow(final_data) > n){break}

require(EnvStats)

#Simulate PCs with respect to the ecdf of the original PC matrix

sim <- function(i){return(remp(n, PC_matrix[,i]))}

cl <- makeCluster(detectCores() - 2) 
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)

simulation_matrix <- foreach(i = 1:ncol(PC_matrix), .combine = "cbind", .packages = "EnvStats") %dopar% sim(i)

stopCluster(cl)

#Save the PC, then apply the inverse transform back to our original space

PC_mat <- simulation_matrix

simulation_matrix <- as.matrix(simulation_matrix)

simulation_matrix <- simulation_matrix %*% Transformation_matrix

new_data <- as.data.frame(matrix(nrow = n, ncol = ncol(data)))


#Declare a function to obtain the categorical values based on the simulate numerical ones

get_vals <- function(i){

out <- as.data.frame(matrix(nrow = n, ncol = 1))

vals <- unique_vals[[i]]
check <- c(1:length(vals))

for(j in 1:nrow(new_data)){

out[j,1] <- vals[order(abs(check - simulation_matrix[j, factors.index[i]]))[1]]

}

is_num <- as.numeric(vals)
if(length(which(is.na(is_num))) == 0){

	out[,1] <- as.numeric(out[,1])} else {

		out[,1] <- as.factor(out[,1])
	}

return(out)

}


#Obtain factors via the previously defined function

if(length(factors.index) != 0){

cl <- makeCluster(detectCores() - 2) 
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)

factors <- foreach(i = 1:length(factors.index), .combine = "cbind") %dopar% get_vals(i)

stopCluster(cl)

new_data[,factors.index] <- factors

}

new_data[,numerics.index] <- simulation_matrix[,numerics.index]

colnames(new_data) <- colnames(data)


#Remove observations that fall outside the original data's range (numerics only)

for(i in 1:length(numerics.index)){

	rng <- range(data[,numerics.index[i]])

	rmv <- which(new_data[,numerics.index[i]] < rng[1] | new_data[,numerics.index[i]] > rng[2])

	if(length(rmv) != 0){

		new_data <- new_data[-rmv,]
		simulation_matrix <- simulation_matrix[-rmv,]
		PC_mat <- PC_mat[-rmv,]

	}

}


#Save the results

final_data <- rbind(final_data, new_data)
final_PC <- rbind(final_PC, PC_mat)

print(paste("Values simulated so far: ", 100*min((nrow(final_data)-1) / n, 1), "% ...", sep = ""), quote = FALSE)

}



final_data <- final_data[-1,]
final_PC <- final_PC[-1,]


#Removing outliers
#We assume that the sum of the squared components of the scaled / centered PC matrix are distributed according to some unknown pdf
#Using fitdistr, we try to find the best pdf via mle / mme and use it to produce a two-sided CI at alpha = 0.05

if(outliers.remove == TRUE){

	#Scaling, centering and obtaining squared sums

	print("Removing outliers...", quote = FALSE)

	original_means <- apply(PC_matrix, 2, mean)
	original_sd <- apply(PC_matrix, 2, sd)

	radius <- function(x){return(sum(x^2))}

	temp_mat <- final_PC
	for(i in 1:ncol(temp_mat)){temp_mat[,i] <- (temp_mat[,i] - original_means[i]) / original_sd[i]}

	cl <- makeCluster(detectCores() - 2) 
	clusterEvalQ(cl, library(foreach))
	registerDoParallel(cl)

	radius_vec <- foreach(i = 1:nrow(temp_mat), .combine = "c") %dopar% radius(temp_mat[i,])

	stopCluster(cl)

	#Fitting distributions

	distributions <- c("norm", "lnorm", "exp", "cauchy", "gamma", "logis", "weibull", "invgamma", "pareto")

	method = c("mle", "mme", "mge")

	require(actuar)
	require(stats)

	fitdist_ <- function(i){

	require(fitdistrplus)
	require(stats)

	for(j in 1:length(method)){

		out <- try(fitdist(radius_vec, distr = distributions[i], method = method[j]))

			if(class(out) != "try-error"){break}

	}

	return(out)

	}


	cl <- makeCluster(detectCores() - 2)
	clusterEvalQ(cl, library(foreach))
	registerDoParallel(cl)

	fitted_models <- foreach(i = 1:length(distributions), .packages = c("fitdistrplus", "stats", "actuar")) %dopar% fitdist_(i)

	stopCluster(cl)

	rmv <- which(is.na(fitted_models))

	if(length(rmv) != 0){

		fitted_models <- fitted_models[-rmv]
		distributions <- distributions[-rmv]

	}

	names(fitted_models) <- distributions

	aic <- matrix(nrow = length(fitted_models), ncol = 1)
	for(i in 1:length(aic)){aic[i] <- fitted_models[[i]]$aic}

	rmv <- which(is.na(aic))

	if(length(rmv) != 0){

		fitted_models <- fitted_models[-rmv]
		distributions <- distributions[-rmv]
		aic <- aic[-rmv]

	}

	#Obtaining the best pdf and its parameters

	choice <- order(aic)[1]

	#Contructing the CI

	f <- get(paste("q", distributions[choice], sep = ""))

	args <- list()
	for(i in 1:length(fitted_models[[choice]]$estimate)){args[[i]] <- fitted_models[[choice]]$estimate[i]}

	names(args) <- names(fitted_models[[choice]]$estimate)

	args_low <- args
	args_low$p <- 0.025

	args_high <- args
	args_high$p <- 0.975

	vals <- c(do.call(f, args_low), do.call(f, args_high))

	rmv <- which(radius_vec < vals[1] | radius_vec > vals[2])

	if(length(rmv) != 0){

		final_data <- final_data[-rmv,]

		print(paste("Removed", length(rmv), "possible outliers..."), quote = FALSE)

	}

}

#Trim extra values is necessary

if(nrow(final_data) > n){final_data <- final_data[c(1:n),]}

return(final_data)

}






