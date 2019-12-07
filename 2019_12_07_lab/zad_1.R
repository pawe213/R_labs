# 1. przyklad rozkladow dyskretnych
# dwupunktowy, geometryczny, dwumianowy
# ujemny dwumianowy Pascala, hipergeometryczny, Poissona

# rozklad jednostajny
#a) gestosc
dunif(1 , min =1, max =3 )
#b )dystrybucja
punif(1.5, min =1, max =2)
#c) kwantyl
qunif(0.5, min =1, max =2)
#d) odchylenie losowe
set.seed(123)
runif(5, min = 1, max = 2)

# rozklad normalny
# ?dnorm
dnorm(0, mean = 0, sd = 1)
pnorm(0, mean = 0, sd = 1)
qnorm(0.5, mean = 0, sd = 1)

n1 <- rnorm(100, mean = 0, sd = 1)
mean(n1)

xs <- seq(-5, 5, 0.1)

#p(0.5<X<1.0) dla X
p1 <- pnorm(1, mean =0, sd = 1) - pnorm(0.5 , mean = 0, sd = 1)
p2 <- pnorm(0.5, mean = 0, sd =1, lower.tail = FALSE)
