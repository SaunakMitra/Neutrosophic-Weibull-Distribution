library(ntsDists)
plot(pnsweibull(q=20,shape = c(1.0519,1.0553),scale = c(9.3370,9.4544)))
y=rnsweibull(n=10000,c(9.5544,10.3370),c(1.0519,1.0553))
b=rnsweibull(n=10000,c(1.0519,1.0553),c(9.5544,10.3370))
a<-dnsweibull(x=remission,scale = c(1.14,1.29),shape = c(9.65,10.91))
plot(ecdf(y[,1]),col="Blue")
lines(ecdf(y[,2]),col="red")
data(remission)
x<-seq(0,2,by=0.002)
plot(x,y)
hist(y,freq = FALSE)
lines(density(y[,1]),col="Blue")
lines(density(y[,2]),col="red")
plot(x,y[,1])
plot.new()
plot(density(y[,1]),col="Blue")
polygon(density(y[,1]),col="Blue")
polygon(density(y[,2]),col="white")
y
a
plot(density(a[,1]),col="Blue")
polygon(density(a[,1]),col="Blue")
polygon(density(a[,2]),col="white")


mean(y)
mean(y[,1])
mean(y[,2])
var(y)
var(y[,1])
var(y[,2])



mean(a)
mean(a[,1])
mean(a[,2])
var(a)
var(a[,1])
var(a[,2])


a[,1]


# Set seed for reproducibility
set.seed(123)

# Generate 100 observations from Weibull distribution
data <- rweibull(10000, shape =1.0519, scale = 9.3370);data

# Define the likelihood function for Weibull distribution
likelihood <- function(params, data) {
  scale <- params[1]
  shape <- params[2]
  -sum(dweibull(data, shape = shape, scale = scale, log = TRUE))
}

# Use optim function to obtain MLE of parameters
mle_result <- optim(c(2, 5), likelihood, data = data, method = "L-BFGS-B")

# Extract MLE estimates
mle_scale <- mle_result$par[1]
mle_shape <- mle_result$par[2]

# Print MLE estimates
cat("MLE of shape parameter:", mle_shape, "\n")
cat("MLE of scale parameter:", mle_scale, "\n")

# Create a grid of parameter values for the likelihood plot
grid_shape <- seq(1, 5, length.out = 100)
grid_scale <- seq(5, 15, length.out = 100)
likelihood_values <- matrix(0, nrow = length(grid_shape), ncol = length(grid_scale))

# Calculate likelihood values for the grid
for (i in seq_along(grid_shape)) {
  for (j in seq_along(grid_scale)) {
    likelihood_values[i, j] <- likelihood(c(grid_shape[i], grid_scale[j]), data)
  }
}

# Plot the two-dimensional likelihood plot
contour(grid_shape, grid_scale,likelihood_values, xlab = "Shape", ylab = "Scale",
        main = "Likelihood Plot for Weibull Distribution")

# Calculate mean failure time and compare with sample mean
mean_failure_time <- gamma(1+1/mle_shape)*mle_scale # Mean of Weibull distribution
sample_mean <- mean(data)

# Print mean values
cat("Mean failure time (MLE):", mean_failure_time, "\n")
cat("Sample mean:", sample_mean, "\n")
y[,1]






install.packages("EnvStats")
library(EnvStats)
eweibull(y[,1],method = "mle")
eweibull(y[,2],method = "mle")
c=gamma(1+1/ 1.062414)*9.687938;c
d=gamma(1+1/1.054294)*10.352736;d
mean(y[,1])
mean(y[,2])
gamma(1+2/ 1.062414)*93.85614
9.460933^2
168.8915-89.50925
var(y[,1])
var(y[,2])
gamma(1+2/1.054294)
10.352736^2
1.822533*107.1791
d^2
195.3374-102.7898


L<-9.687938;L
k<-1.062414;k
v<-(gamma(1+3/k)-3*gamma(1+1/k)*gamma(1+2/k)+2*(gamma(1+1/k))^3)*(L)^3
v1<-((gamma(1+2/k)-(gamma(1+1/k))^2))*(L)^2;v1
v/((v1)^(3/2))
L1<-10.3370;L1
k1<- 1.0553;k1
u<-(gamma(1+3/k1)-3*gamma(1+1/k1)*gamma(1+2/k1)+2*(gamma(1+1/k1))^3)*(L1)^3
u1<-((gamma(1+2/k1)-(gamma(1+1/k1))^2))*(L1)^2;u1
u/((u1)^(3/2))
g<-((12*gamma(1+2/k)*(gamma(1+1/k)^2)-6*(gamma(1+1/k)^4)-4*gamma(1+1/k)*gamma(1+3/k)+gamma(1+4/k)-3*(gamma(1+2/k)^2)))*(L)^4;g
h<-((12*gamma(1+2/k1)*(gamma(1+1/k1)^2)-6*(gamma(1+1/k1)^4)-4*gamma(1+1/k1)*gamma(1+3/k1)+gamma(1+4/k1)-3*(gamma(1+2/k1)^2)))*(L1)^4;h
(g/(v1^2))-3
(h/(u1^2))-3
L*(log(2)^(1/k))
L1*(log(2)^(1/k1))

