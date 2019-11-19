#Tutorial 1 R Code Luke Whitehill
#Exercise 2:
roots <- Mod(polyroot(c(1, -0.5, 0.8, 0.1, -0.2)))

print(roots)
print("Output of the Modpolyroot function is 1, 1, 2.23, 2.23. Because one or more of the roots are 1, the process is of unit root.")

#Exercise 3:
#Solving Gamma Matrix
A <- matrix(data=c(1,-0.6,0.9,-0.4,-0.6,1.9,-0.4,0,0.9,-1,1,0,-0.4,0.9,-0.6,1), nrow=4, ncol=4, byrow=TRUE)    
b <- matrix(data=c(1, 0, 0, 0), nrow=4, ncol=1, byrow=FALSE)
round(solve(A, b), 4)

#Exercise 4:
set.seed(123456)
x <- arima.sim(n = 1e3, list(ar = c(0.6, -0.9, 0.4)), innov=rnorm(1e3))
acf4 <- acf(x, lag.max = 20, main="ACF", ylab="", ylim=c(-1,1), lwd = 3, col="darkblue", ci.col = "coral1")
round(acf4$acf[1:5], 4)
