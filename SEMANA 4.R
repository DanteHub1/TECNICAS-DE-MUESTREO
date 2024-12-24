####### TEMA: MUESTREO ÓPTIMO - AFIJACIÓN PROPORCIONAL Y NEYMAN #######
####### EJEMPLO #######

# Definición de las puntuaciones de cada itinerario

itinerario_I <- c(80, 92, 68, 85, 72, 87, 85, 91, 90, 81, 62, 79, 
                  61, 83, 68, 52, 71, 61, 59, 42)
itinerario_II <- c(85, 82, 48, 75, 53, 73, 65, 78, 49, 69, 72, 81,
                   53, 59, 39, 32)
itinerario_III <- c(42, 32, 36, 31, 65, 29, 43, 19, 53, 14, 61, 31, 
                    42, 30, 39, 32)

# Tamaños de muestra

n1 <- 14  # Itinerario I
n2 <- 20  # Itinerario II
n3 <- 16  # Itinerario III

# Cálculo de las medias

media_I <- mean(itinerario_I)
media_II <- mean(itinerario_II)
media_III <- mean(itinerario_III)

# Calcular la media ponderada

n_total <- n1 + n2 + n3
media_total <- (media_I * n1 + media_II * n2 + media_III * n3) / n_total

# Cálculamos el error estándar

desv_est <- sqrt((var(itinerario_I) / n1) + (var(itinerario_II) / n2) + (var(itinerario_III) / n3))
error_estandar <- 1.96 * desv_est  # 1.96 para un nivel de confianza del 95%

# Resultados

print(media_total)
(error_estandar)

## B.
# Crear un gráfico de cajas

boxplot(itinerario_I, itinerario_II, itinerario_III,
        names = c("Itinerario I", "Itinerario II", "Itinerario III"),
        main = "Gráfico de Cajas de Puntuaciones por Itinerario",
        ylab = "Puntuaciones",
        col = c("blue", "red", "green"))

## C.
# Calcular la diferencia de medias

diferencia_medias <- media_I - media_II

# Realizar prueba t

prueba_t <- t.test(itinerario_I, itinerario_II)

# Resultados

diferencia_medias
prueba_t$p.value

## D.
# Calcular las varianzas

var_I <- var(itinerario_I)
var_II <- var(itinerario_II)
var_III <- var(itinerario_III)

# Tamaños de la población

N1 <- 55  # Itinerario I
N2 <- 80  # Itinerario II
N3 <- 65  # Itinerario III

# Tamaño de muestra total

n_total_50 <- 50

# Afijación de Neyman

n1_Neyman <- n_total_50 * (N1 * sqrt(var_I) / (N1 * sqrt(var_I) + N2 * sqrt(var_II) + N3 * sqrt(var_III)))
n2_Neyman <- n_total_50 * (N2 * sqrt(var_II) / (N1 * sqrt(var_I) + N2 * sqrt(var_II) + N3 * sqrt(var_III)))
n3_Neyman <- n_total_50 * (N3 * sqrt(var_III) / (N1 * sqrt(var_I) + N2 * sqrt(var_II) + N3 * sqrt(var_III)))

# Resultados

n1_Neyman
n2_Neyman
n3_Neyman

## E.
# Definir el límite de error y la varianza total

error_limite <- 4

# Cálculo de tamaños de muestra necesarios (afijación proporcional)

n1_proporcional <- ceiling((N1 * (var_I / (error_limite^2))) / (N1 + N2 + N3))
n2_proporcional <- ceiling((N2 * (var_II / (error_limite^2))) / (N1 + N2 + N3))
n3_proporcional <- ceiling((N3 * (var_III / (error_limite^2))) / (N1 + N2 + N3))

# Resultados

print(n1_proporcional)
print(n2_proporcional)
print(n3_proporcional)


## F.
# Usar el mismo método de Neyman para el nuevo límite de error

n1_Neyman_nuevo <- ceiling(n_total_50 * (N1 * sqrt(var_I) / (N1 * sqrt(var_I) + N2 * sqrt(var_II) + N3 * sqrt(var_III))))
n2_Neyman_nuevo <- ceiling(n_total_50 * (N2 * sqrt(var_II) / (N1 * sqrt(var_I) + N2 * sqrt(var_II) + N3 * sqrt(var_III))))
n3_Neyman_nuevo <- ceiling(n_total_50 * (N3 * sqrt(var_III) / (N1 * sqrt(var_I) + N2 * sqrt(var_II) + N3 * sqrt(var_III))))

# Resultados

n1_Neyman_nuevo
n2_Neyman_nuevo
n3_Neyman_nuevo

# Comparar resultados

comparacion <- data.frame(
  Proporcional = c(n1_proporcional, n2_proporcional, n3_proporcional),
  Neyman = c(n1_Neyman_nuevo, n2_Neyman_nuevo, n3_Neyman_nuevo)
)
print(comparacion)