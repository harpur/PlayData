###
# Simulate correlations
###


#These 2 functions simulate the correlation coefficient and p-value for N simulations of 2 sets of independant data 
#they restrict the correlation to only the top 95% of the data
#the first function (indep.cut.corr) is for 2 normally-distributed data sets. 
#the second function (correlated.cut.cor) is for 2 ndata sets that are  correlated with a coefficient provided


#required libraries ----
require(MASS)


#functions -----
indep.cut.corr <- function(N = 10000, cutoff =  0.95, s = 10000){
	#returns a vector of length N for the P-values of N simulations.
	ps = c()
	for(i in c(1:N)){
		x = rnorm(s)
		y = rnorm(s)
		p = cor.test(x[which(x > quantile(x,cutoff))], y[which(x > quantile(x,cutoff))])$p.value
		ps = c(ps,p)

	}
	return(ps) 
}


correlated.cut.cor <- function(N = 10000, cutoff =  0.95, s = 10000, corr = 0.17){
	#returns a dataframe of nrow = N for the P-values and coefficients of N simulations
	ps = c()
	stats = c()
	for(i in c(1:N)){
		out = as.data.frame(mvrnorm(s, mu = c(0,0), 
		                     Sigma = matrix(c(1,corr,corr,1),, ncol = 2), 
		                     empirical = TRUE))
		p = cor.test(out$V1[which(out$V1>quantile(out$V1,0.95))], out$V2[which(out$V1>quantile(out$V1,0.95))])
		ps = c(ps, p$p.value)
		stats = c(stats, p$estimate)
	}
	return(data.frame(cbind(stats, ps)))
}




#useage examples ------
x = indep.cut.corr()
length(x[x<0.05])/10000 #here, N = 10000
# [1] 0.0491

x = correlated.cut.cor()
nrow(x[which(x$ps<0.05 & x$stat < 0),])/10000 #here, N = 10000
# [1] 6e-04








