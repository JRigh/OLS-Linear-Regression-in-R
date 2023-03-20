#-----------------------------------------------------------------
# Sampling distribution of the OLS estimators in linear regression
#-----------------------------------------------------------------

# create synthetic data
set.seed(2022)
n <- 150
X <- matrix(c(rep(1,n), runif(n, min = 0, max = n)), ncol = 2, byrow = FALSE)
beta0 <- 1 ; beta1 <- 2.5 ; sigma <- 2
Beta <- matrix(c(beta0, beta1))
epsilon <- rnorm(n, sd = sigma) 

# OLS estimators
y <- X %*% Beta  + epsilon
Beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
Beta.hat
#        [,1]
# [1,] 0.9785
# [2,] 2.4987

matrix(c(coef(lm(y ~ X[,-1]))[[1]], coef(lm(y ~ X[,-1]))[[2]]))
#        [,1]
# [1,] 0.9785
# [2,] 2.4987

# Simulation study
set.seed(1986)
nsim = 10000
OLS <- matrix(rep(0, 2*nsim), ncol = 2, nrow = nsim)

for (i in 1:nsim){
  
  X <- matrix(c(rep(1,n), runif(n, min = 0, max = n)), ncol = 2, byrow = FALSE)
  epsilon <- rnorm(n, sd = sigma) 
  y <- X %*% Beta  + epsilon
  OLS[i, ] <- t(solve(t(X) %*% X) %*% t(X) %*% y)
  
}

head(OLS)
#        [,1]  [,2]
# [1,] 0.5326 2.505
# [2,] 0.4589 2.506
# [3,] 0.8918 2.499
# [4,] 1.0718 2.496
# [5,] 0.9344 2.501
# [6,] 1.0746 2.502

# Plotting
OLS <- data.frame(OLS)

p1<- ggplot(OLS, aes(x = X1)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  xlab(expression(hat(beta)[0])) +
  geom_density(lwd = 1, colour = "#a50101") + theme_minimal()

p2<- ggplot(OLS, aes(x = X2)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  xlab(expression(hat(beta)[1])) +
  geom_density(lwd = 1, colour = "#a50101") + theme_minimal()

p3<- ggplot(OLS, mapping = aes(x = X1, y = X2)) +
  geom_point(size = 0.6) + 
  xlab(expression(hat(beta)[0])) +   ylab(expression(hat(beta)[1])) +
  geom_jitter(alpha = 0.75, size = 0.6) + theme_minimal()

library(gridExtra)

grid.arrange(arrangeGrob(p1, p2), 
             arrangeGrob(p3, ncol=1), 
             ncol=2, widths=c(1,1))


#----
# end
#----

#-----------------------------------------------------------
# Confidence intervals for coefficients in linear regression
#-----------------------------------------------------------

# create synthetic data
set.seed(2022)
n <- 150
X <- matrix(c(rep(1,n), runif(n, min = 0, max = n)), ncol = 2, byrow = FALSE)
beta0 <- 1 ; beta1 <- 2.5 ; sigma <- 2 ;Beta <- matrix(c(beta0, beta1)) ;epsilon <- rnorm(n, sd = sigma) 

# Simulation study
set.seed(1986)
nsim = 10000
OLS <- matrix(rep(0, 2*nsim), ncol = 2, nrow = nsim)
sehat.beta0hat = sehat.beta1hat = numeric(nsim)
lowbeta0 = lowbeta1 = highbeta0 = highbeta1 = numeric(nsim)

for (i in 1:nsim){
  
  X <- matrix(c(rep(1,n), runif(n, min = 0, max = n)), ncol = 2, byrow = FALSE)
  epsilon <- rnorm(n, sd = sigma) 
  y <- X %*% Beta  + epsilon
  OLS[i, ] <- t(solve(t(X) %*% X) %*% t(X) %*% y)
  sehat.beta0hat[i] <- sqrt(((t(y-X %*% OLS[i, ]) %*% (y-X %*% OLS[i, ]))) / (n-(2+1))) * sqrt(diag(solve(t(X) %*% X))[1])
  sehat.beta1hat[i] <- sqrt(((t(y-X %*% OLS[i, ]) %*% (y-X %*% OLS[i, ]))) / (n-(2+1))) * sqrt(diag(solve(t(X) %*% X))[2])
  lowbeta0[i] <- OLS[i,1] - (qt(1-0.05/2, n-(2+1)) * sehat.beta0hat[i])
  lowbeta1[i] <- OLS[i,2] - (qt(1-0.05/2, n-(2+1)) * sehat.beta1hat[i])
  highbeta0[i] <- OLS[i,1] + (qt(1-0.05/2, n-(2+1)) * sehat.beta0hat[i])
  highbeta1[i] <- OLS[i,2] + (qt(1-0.05/2, n-(2+1)) * sehat.beta1hat[i])
}

data <- data.frame(x = 1:nsim, l0 = lowbeta0, b0 = OLS[,1], h0 = highbeta0,
                   l1 = lowbeta1, b1 = OLS[,2], h1 = highbeta1)


# plotting
library(ggplot2)

p1 <- ggplot(data[1:50, ], aes(x, OLS[1:50,1])) + 
  geom_point() + 
  geom_errorbar(aes(ymin = l0[1:50], ymax = h0[1:50])) +
  geom_hline(yintercept=1, linetype="dashed", color = "red") +
  labs(title = '95% confidence intervals for the intercept coefficients',
       subtitle = "The red horizontal line indicates the true value for the intercept",
       caption = "50 first entries of the artificial dataset") + xlab('')+ ylab('95% CI') +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10),
        plot.subtitle=element_text(size=8, face="italic", color="darkred"))


p2 <- ggplot(data[1:50, ], aes(x, OLS[1:50,2])) + 
  geom_point() + 
  geom_errorbar(aes(ymin = l1[1:50], ymax = h1[1:50])) +
  geom_hline(yintercept=2.5, linetype="dashed", color = "red") +
  labs(title = '95% confidence intervals for the slope coefficients',
       subtitle = "The red horizontal line indicates the true value for the slope",
       caption = "50 first entries of the artificial dataset") + xlab('')+ ylab('95% CI') +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10),
        plot.subtitle=element_text(size=8, face="italic", color="darkred"))


library(gridExtra)
grid.arrange(p1, p2, nrow = 1)


#----
# end
#----

