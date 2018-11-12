# Trabalho inferência
citation()
# Exponencial
x <- seq(0, 2, l = 100)
b0 = log(1)
b1 = -2
lambda = exp(b0 +b1*x)
eta = b0 +b1*x # preditor de cada lambda
plot(eta ~ x)
lambda = exp(eta)
eta
lambda
summary(lambda)
plot(lambda ~ x)
# Simulação das observações
set.seed(123)
y = rexp(100, rate = lambda)
plot(y ~ x)
hist(y)
summary(y)
y
plot(y)

# criando as observações censuradas
y1 <- y - 0.03
y2 <- y + 0.03
censu <- data.frame(y1, y2)
censu
plot(censu$y2)
#verossimilhança da censura
# função de verossimilhança
V_censu <- c()
for(i in 1:100){
  V_censu[i] <- pexp(censu$y2[i], rate = lambda[i]) - pexp(censu$y1[i], rate = lambda[i])
}
plot(V_censu)

#log-verossimill_censu <- log(V_censu)lhança

ll_censu
summary(ll_censu)
plot(-ll_censu)
curve(log, -11.680, -2.966)
# Verossimilhança
vero <- sum(-ll_censu)
vero

#estimando
#log-verossimilhança
ll <- function(theta, inferior, superior, x){
  lambda = exp(theta[1] + theta[2]*x)
  output <- -sum(log(pexp(superior, rate = lambda) - pexp(inferior, rate = lambda)))
  return(output)
}
curve(ll(theta = c(log(1), -2), inferior = censu$y1, superior = censu$y2, x = x))
# avaliar
bla <- ll(theta = c(log(1), -2), inferior = censu$y1, superior = censu$y2, x = x)
ll(theta = c(log(0.5), -0.5), inferior = censu$y1, superior = censu$y2, x = x)

# maximizando a função
grid_b0 <- seq(0.1, 10, l = 50)
grid_b1 <- seq(-1, 1, l = 50)
grid = expand.grid(grid_b0, grid_b1)
head(grid)

#avaliando em todos os pontos do grid
ll_valor <- c()
for(i in 1:2500){
  ll_valor[i] <- ll(theta = as.numeric(grid[i, ]), inferior = censu$y1, 
                    superior = censu$y2, x = x)
}

image(grid_b0, grid_b1, matrix(ll_valor, 50, 50))
grid$ll_valor = ll_valor
grid[which.max(grid$ll_valor), ]

#otimizando numericamente
#colocar ll para retornar o negativo da ll antes
oo <- optim(par = c(1, -3), fn = ll, inferior = censu$y1, 
            superior = censu$y2, x = x, hessian = TRUE)
#hessian são os valores das funções score
str(oo)
oo$par
oo$value
oo$convergence
oo$message
oo$hessian
inv_Io <- solve(oo$hessian)
inv_Io
ic_Max <- oo$par + qnorm(0.975)*sqrt(diag(inv_Io)) 
ic_Min <- oo$par - qnorm(0.975)*sqrt(diag(inv_Io))
cbind(ic_Min, oo$par, ic_Max)
oo$par






