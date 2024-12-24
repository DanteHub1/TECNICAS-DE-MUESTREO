####### TEMA: MUESTREO POR CONGLOMERADOS DE TAMAÑOS IGUALES #######
####### EJEMPLO #######

# Cargar librerías necesarias

library(readxl)
library(dplyr)
library(stats)

# Cargar los datos desde la hoja "MARCO MUESTRAL"

file_path <- "C:/CSVs/MMCongIgual_S5 - Sesion5.xlsx"
data <- read_excel(file_path, sheet = "MARCO MUESTRAL")

View(data)
names(data)

N <- length(data$N);N
n <- 5 # número de cong para la muestra piloto

# Seleccionar una muestra piloto de n=5 conglomerados

muestra_piloto<-data[sample(1:N,n,replace = FALSE),]
muestra_piloto
View(muestra_piloto)

# Definir el tamaño del conglomerado: M

M <- unique(muestra_piloto$mi)[1]
M

# CALCULAR LA MEDIA ESTIMADA DE LA MUESTRA PILOTO

media_estimada <- (1/(n*M))*sum(muestra_piloto$y)
media_estimada


# Vista de los primeros registros

head(data)

# Calcular los factores para calcular el tamaño de la muestra: ANOVA (MSB  y MSW)

conglomerado_1 <- c(2, 1,     2,     2,     1,     2,     1,     3,     2,     2 )
conglomerado_2 <- c(1, 2,     2,     1,     2,     2,     4,     1,     1,     1 )
conglomerado_3 <- c(2, 2,     3,     3,     2,     1,     1,     4,     3,     1 )
conglomerado_4 <- c(2, 1,     2,     1,     1,     5,     1,     4,     1,     2 )
conglomerado_5 <- c(2, 1,     2,     3,     2,     4,     2,     3,     1,     2 )

datos<-data.frame(
  conglomerado=factor(rep(1:5,each=10)),
  hogares=c(conglomerado_1,conglomerado_2,conglomerado_3,conglomerado_4,conglomerado_5)
)
print(datos)

# ANOVA

anova_resultado <- aov(hogares~conglomerado,data=datos)
anova_resultado

summary(anova_resultado)

MSB <- 0.520
MSW <- 1.042

SS <- (1/M) * ((M-1) * MSW + MSB); SS
S <- sqrt(SS); S
NM <- 400 * 10
nM <- 4 * 10
Vy_somb <- ((NM-nM) / NM) * (SS/nM); Vy_somb
Z <- qnorm(0.975); Z

# E<-Z*sqrt(Vy_somb);E=0.3192084

E <- Z * sqrt(Vy_somb); E
D <- E^2/Z^2; D

# a. Calcular el tamaño de la muestra de conglomerados 

n <- (N * S^2) / ((N-1) * D + S^2); n
n <- ceiling(n); n

# b. Estimar el número medio de periodicos comprados

N <- length(data$N);N
n <- 37

# Seleccionar una muestra aleatoria de n=37 conglomerados

muestra_seleccionada <- data[sample(1:N,n,replace = FALSE),]
muestra_seleccionada
View(muestra_seleccionada)

# Estimar la media

M <- unique((muestra_seleccionada$mi))[1]; M

# CALCULAR LA MEDIA ESTIMADA

media_estimada <- (1 / (n * M)) * sum(muestra_seleccionada$y)
media_estimada

# Calcuar la varianza de la media estimada

media_conglomerado <- muestra_seleccionada$y/M
media_conglomerado
f <- n/N; f

# La varianza de la media estimada

vme<-(1-f)/n*sum((media_conglomerado-mean(media_conglomerado))^2/(n-1));vme                                    

E<-Z*sqrt(vme);E                                     
Z
Li<-media_estimada-E
Ls<-media_estimada+E
ic <- c(Li, Ls); ic

# c. ESTIMAR EL TOTAL DE PERIODICOS COMPRADOS

total_estimado <- (N/n) * sum(muestra_seleccionada$y)
total_estimado

# Cálculo de la varianza del total poblacional estimada

vte <- N*N*vme; vte

E <- Z * sqrt(vte); E                                    

Li <- total_estimado-E
Ls <- total_estimado+E
ic <- c(Li, Ls); ic

# D. 

# Extraer solo las columnas de los hogares (donde están las observaciones de compras)

hogares_data <- muestra_seleccionada %>% select(starts_with("h"))

# Crear una matriz booleana donde TRUE indica hogares que compran 3 o más periódicos

hogares_informados <- hogares_data >= 3

# Calcular la proporción de hogares informados en cada conglomerado (promedio por fila)

prop_informados_por_conglomerado <- rowMeans(hogares_informados)
prop_informados_por_conglomerado

# Promedio de la proporción de hogares informados en la muestra

prop_informada <- mean(prop_informados_por_conglomerado); prop_informada

# Fracción de muestreo

f <- n / N; f

# Calcular la varianza muestral de la proporción de hogares informados en los conglomerados

S_P2 <- var(prop_informados_por_conglomerado); S_P2

# Varianza de P estimado

Var_P_est <- (1 - f) * S_P2 / n; Var_P_est

# Error de estimación al 98% de confianza

z_98 <- qnorm(0.99)
error_d <- z_98 * sqrt(Var_P_est); error_d

Li_prop <- prop_informada - error_d
Ls_prop <- prop_informada + error_d
ic_prop <- c(Li_prop, Ls_prop); ic_prop
