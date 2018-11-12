
 
# Poisson
x <- seq(0, 1, l = 100)
b0 = log(10)
b1 = 0.5

eta = b0 +b1*x # preditor de cada lambda
plot(eta - x)
lambda = exp(eta)

#simulando

set.seed(123)
y = rpois(100, lambda = lambda)
plot(y -x)

dados = data.frame("y" = y, "x" = x)
head(dados)

#log-verossimilhança
ll <- function(theta, y, x){
  lambda = exp(theta[1] + theta[2]*x)
  output <- -sum(dpois(y, lambda = lambda, log = TRUE))
  return(output)
}

# avaliar
ll(theta = c(log(10), 0.5), y = y, x = x)
ll(theta = c(log(10), 0), y = y, x = x)

# maximizando a função
grid_b0 <- seq(0.1, 5, l = 50)
grid_b1 <- seq(-1, 1, l = 50)
grid = expand.grid(grid_b0, grid_b1)
head(grid)

#avaliando em todos os pontos do grid
ll_valor <- c()
for(i in 1:2500){
  ll_valor[i] <- ll(theta = as.numeric(grid[i, ]), y = y, x = x)
}

image(grid_b0, grid_b1, matrix(ll_valor, 50, 50))
grid$ll_valor = ll_valor
grid[which.max(grid$ll_valor), ]

#otimizando numericamente
#colocar ll para retornar o negativo da ll antes
oo <- optim(par = c(2.54, 0), fn = ll, y = y, x = x, hessian = TRUE)
#hessian são os valores das funções score
str(oo)
oo$par
oo$value
oo$convergence
oo$message
inv_Io <- solve(oo$hessian)
inv_Io
ic_Max <- oo$par + qnorm(0.975)*sqrt(diag(inv_Io)) 
ic_Min <- oo$par - qnorm(0.975)*sqrt(diag(inv_Io))
cbind(ic_Min, oo$par, ic_Max)
oo$par
#------------------------------------------------------------------------------

# trabalho
#observações https://www.inf.ufsc.br/~andre.zibetti/probabilidade/exponencial.html

#Y-i|theta ~ Exp(lambda)

x <- seq(0, 10, l = 100)
b0 = log(5)
b1 = 0.5

eta = b0 +b1*x # preditor de cada lambda
plot(eta - x)
lambda = exp(eta)
lambda
summary(lambda)
set.seed(123)
y = rexp(100, rate = lambda)
plot(y, x)

head(y)
y

dados = data.frame("y" = y, "x" = x)
head(dados)

#log-verossimilhança
ll <- function(theta, y, x){
  lambda = exp(theta[1] + theta[2]*x)
  output <- -sum(dexp(y, rate = lambda, log = TRUE))
  return(output)
}

# avaliar
ll(theta = c(log(10), 0.5), y = y, x = x)
ll(theta = c(log(10), 0), y = y, x = x)

# maximizando a função
grid_b0 <- seq(0.1, 10, l = 50)
grid_b1 <- seq(-1, 1, l = 50)
grid = expand.grid(grid_b0, grid_b1)
head(grid)

#avaliando em todos os pontos do grid
ll_valor <- c()
for(i in 1:2500){
  ll_valor[i] <- ll(theta = as.numeric(grid[i, ]), y = y, x = x)
}

image(grid_b0, grid_b1, matrix(ll_valor, 50, 50))
grid$ll_valor = ll_valor
grid[which.max(grid$ll_valor), ]

#otimizando numericamente
#colocar ll para retornar o negativo da ll antes
oo <- optim(par = c(2.54, 0), fn = ll, y = y, x = x, hessian = TRUE)
#hessian são os valores das funções score
str(oo)
oo$par
oo$value
oo$convergence
oo$message
inv_Io <- solve(oo$hessian)
inv_Io
ic_Max <- oo$par + qnorm(0.975)*sqrt(diag(inv_Io)) 
ic_Min <- oo$par - qnorm(0.975)*sqrt(diag(inv_Io))
cbind(ic_Min, oo$par, ic_Max)
oo$par



## ll = pexp(y2, rate) - pexp(y1, rate)
## onde y1 = y - 0.5 e y2 = y + 0.5
# criando as observações censuradas
y1 <- y - 0.5
y2 <- y + 0.5
censu <- data.frame(y1, y2)
censu
#verossimilhança da censura
# função de verossimilhança
ll_censu <- c()
for(i in 1:100){
  ll_censu[i] <- pexp(censu$y2[i], rate = lambda) - pexp(censu$y1[i], rate = lambda)
}
plot(ll_censu2)
#log-verossimilhança
vero <- sum(log(ll_censu))
vero


ll_censu2 <- c()
for(i in 1:100){
  ll_censu2[i] <- pexp(censu$y2[i], rate = lambda[i]) - pexp(censu$y1[i], rate = lambda[i])
}
ll_censu2
#log-verossimilhança
log_vero <- -log(ll_censu2)
log_vero
plot(log_vero)
vero_ <- sum(log_vero)
vero_
vero2 <- sum(log(ll_censu2))
vero2

##fazer uma função q vai alterando os bettas
##x <- seq(0, 10, l = 100)
##b0 = log(10)
##b1 = 0.5

##eta = b0 +b1*x # preditor de cada lambda
##plot(eta - x)
##lambda = exp(eta)
##lambda