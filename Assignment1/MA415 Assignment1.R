#1a
1:20

#1b
20:1

#1c
c(1:20, 19:1)

#1d
temp <- c(4, 6, 3)

#1e
rep(temp, length = 30)

#1f
rep(temp, length = 31)

#1g
c(rep(4, 10), rep(6, 10), rep(3, 10))

#2
x <- seq(3, 6, by = 0.1)
y <- exp(x) * cos(x)
y

#3a
x1 <- seq(3, 36, by = 3)
x2 <- seq(1, 34, by = 3)
y <- (0.1^x1)*(0.2^x2)
y

#3b
x1 <- seq(1, 25, by = 1)
y <- (2^x1)/x1
y

#4a
i <- seq(10, 100)
s <- sum(i^3 + 4*i^2)
s

#4b
i <- seq(1, 25)
s <- sum((2^i/i)+(3^i/i^2))
s

#5a
l <- paste("label", 1:30)
l

#5b
l1 <- paste("fn", 1:30, seq="")
l1

#6
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace = T)

#a
x6a <- yVec[2:250] - xVec[1:249]
x6a

#b
x6b <- sin(yVec[1:249]/cos(2:250))
x6b

#c
x6c <- xVec[1:248] + 2*xVec[2:249] - xVec[3:250]
x6c

#d
n <- length(xVec)
x <- sum(exp(-xVec[-1])/(xVec[-n]+10))
x

#7a
x <- yVec[yVec > 600]
x

#7b
x <- which(yVec > 600)
x

#7c
x <- xVec[yVec > 600]
x

#7d
x <- sqrt(abs(xVec - mean(xVec)))
x

#7e
x <- sum(yVec > max(yVec) - 200)
x

#7f
x <- sum(xVec %% 2 == 0)
x

#7g
x <- xVec[order(yVec)]
x

#7h
x <- yVec[c(T, F, F)]
x

#8
x <- 1 + sum(cumprod(seq(2, 38, by = 2) / seq(3, 39, by = 2)))
x




