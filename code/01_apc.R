# XVIII Jornadas Argentinas de Estudios de Población y 
# V Congreso Internacional de Población del Cono Sur,

# Curso Tendencias y perturbaciones de mortalidad - Análisis por 
# edad-periodo-cohorte 
# Instructor: Enrique Acosta
# Septiembre 23, 2025
# Córdoba, Argentina

rm(list=ls())
source("code/00_setup.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Obtener los datos para el análisis ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Defunciones y población por edad simple. Datos de WPP 2024:
dts <- read_rds("data/wpp_latam_Dx_Nx.rds")

head(dts)

unique(dts$year)
unique(dts$age)
unique(dts$country)

ct <- "El Salvador"
sx <- "t"
pmin <- 1950
pmax <- 2023
amin <- 10
amax <- 90

dts2 <- 
  dts %>% 
  filter(country == ct,
         sex == sx,
         year %in% pmin:pmax,
         age %in% amin:amax) %>% 
  as_tibble()

dts2

# Crear categoría de cohorte = período - edad
# Calcular tasas de mortalidad y su logaritmo
data <- 
  dts2 %>% 
  rename(A = age,
         P = year) %>% 
  mutate(C = P - A) %>% 
  select(country, P, C, A, Dx, Nx) %>% 
  mutate(Mx = 100000 * (Dx / Nx), 
         log_m = log(Mx))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Análisis visual: gráficas de edad-periodo-cohorte ==== 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # a. Edad-Período:observaciones conectadas por P
  p1 <- 
    data %>% 
    filter(Dx > 0) %>% 
    ggplot()+
    geom_line(aes(x = A, y = Mx, colour = P, group = P))+
    scale_y_log10()+
    scale_colour_viridis(option = "C")+
    theme_bw()+ 
    labs(title = paste0("Age-Period: ", unique(data$country)))
  
  
  # b. Edad-Cohorte:observaciones conectadas por C
  p2 <- 
    data %>% 
    filter(Dx > 0) %>% 
    arrange(C, A) %>% 
    ggplot()+
    geom_line(aes(x = A, y = Mx, group = C, colour = C))+
    scale_y_log10()+
    scale_colour_viridis(option = "C")+
    theme_bw() +
    labs(title = paste0("Age-Cohort: ", unique(data$country)))
  
  
  # c. Período-Edad: observaciones conectadas por A (según el periodo)
  p3 <- 
    data %>% 
    filter(Dx > 0) %>% 
    ggplot()+
    geom_line(aes(x = P, y = Mx, colour = A, group = A))+
    scale_y_log10()+
    scale_colour_viridis(option = "C")+
    theme_bw()+
    labs(title = paste0("Period-Age: ", unique(data$country)))
  
  
  # d. Cohorte-Edad: observaciones conectadas por A (según la cohorte)
  p4 <- 
    data %>% 
    filter(Dx > 0) %>% 
    ggplot()+
    geom_line(aes(x = C, y = Mx, colour = A, group = A))+
    scale_y_log10()+
    scale_colour_viridis(option = "C")+
    theme_bw()+
    labs(title = paste0("Cohort-Age: ", unique(data$country)))
  
  (p1+p2)/(p3+p4)
  
}


# Aún más informativo: combinar las tres variables en una superficie de Lexis 
# Vamos a graficar las tasas de mortalidad y los cambios de la mortalidad en 
# el tiempo

data

# Tasas de mortalidad:
data %>% 
  ggplot() +
  geom_tile(aes(x = P, y = A, fill = log(Dx/Nx))) +
  scale_fill_viridis(option = "C") +
  labs(title = paste0("Lexis mx: ", unique(data$country)),
       x = "Year", y = "Age")+
  coord_fixed(expand = 0)+
  theme_bw()


# Cambio en las tasas de mortalidad (mx) en el tiempo: para cada edad, calculamos 
# la diferencia relativa entre  años consecutivos, 

# Cálculo del cambio en el tiempo, para la misma edad: cociente entre la tasa 
# en el año t y la tasa en el año t-1, para la misma edad
db_per <-
  data %>%
  arrange(A, P) %>% 
  group_by(A) %>%
  mutate(ch = ((Mx / lag(Mx)) - 1) * 100) %>%
  ungroup() %>%
  drop_na()

db_per

# Una forma de hacer un plot rápido de cambio de mortalidad por periodo/cohorte
# (desde la pers´pectiva de un Lexis normal, se trata de cambios horizontales,
# de izquierda a derecha)
db_per %>% 
  ggplot()+
  geom_tile(aes(P, A, fill = ch))+
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0)+
  coord_fixed(expand = 0)

# Es útil y práctico pero no ideal: no se pueden observar bien los puntos de 
# cambio de mortalidad.
# La siguiente funcion (lexis_cambio) nos grafica una superficie de Lexis con 
# mejor esacala de color.
# La función la encuentran en el script 00_setup.R
h1 <- 
  lexis_cambio(db = dts, c = ct, s = sx, 
               amin = 0, amax = 90, ymin = 1950, ymax = 2023)
h1

# También podemos ver los cambios con la edad y las cohortes. En la siguiente 
# gráfica vamos a ver el cambio en la tasa de mortalidad de una edad a la 
# siguiente, para el mismo año. 
# (desde la pers´pectiva de un Lexis normal, se trata de cambios verticales, 
# de abajo hacia arriba)

v1 <- lexis_cambio_edad(db = dts, c = ct, s = sx, 
                        amin = 0, amax = 90, ymin = 1950, ymax = 2023)
v1


# Suavizamiento de datos ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hagamos lo mismo pero con los datos suavizados:

# Podemos suavizar nuestros datos en 2D usando P-splines. Son componentes 
# semiparamétricos que permiten ajustar los datos con una gran flexibilidad
# Hay un paquete muy bueno para esto por GC Camarda: MortalitySmooth. 
# Para más información, ver Camarda (2012) "MortalitySmooth: An R Package for 
# Smoothing Poisson Counts with P-Splines", en:
# https://www.jstatsoft.org/article/view/v050i01
# He creado una función (suaviz_2d) que simplifica este paso en 00_setup.R

dts_suav <- 
  suaviz_2d(db = dts, c = ct, s = sx, 
            amin = 0, amax = 90, ymin = 1950, ymax = 2023)

# Y le aplicamos la función que hicimos para la superficie de cambio en el tiempo
h2 <- 
  lexis_cambio(db = dts_suav, c = ct, s = sx, 
               amin = 0, amax = 90, ymin = 1950, ymax = 2023)
h2

# Veamos las dos versiones del cambio de la mortlidad en el tiempo (datos brutos 
# y suavizados)
h1+h2

v2 <- lexis_cambio_edad(db = dts_suav, c = ct, s = sx, 
                        amin = 0, amax = 90, ymin = 1950, ymax = 2023)

# Y las dos versiones del cambio de la mortlidad entre edades (datos brutos 
# y suavizados)
v1+v2

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Análisis estadístico: modelos de edad, período y cohorte ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data

# Ajustamos un modelo Poisson, incluyendo cada categoría de edad, periodo, y
# cohorte de forma categorial (cada categoria es una variable en el modelo)
# En el script 01b_apc_paso_a_paso.R encuentran una descripción paso a paso de
# como se ajustan diferentes modelos, de acuerdo a si se modela cada dimensión 
# de forma lineal (continua) o no lineal (categóricas). 
# Acá, por cuestioneslimitaciones de tiempo, saltamos directamente al modelo 
# que estima los efectos no lineales, bien sea con los efectos lineales de
# periodo y cohorte (drift) incluidos en alguna de las dos dimensiones, 
# o extraidos de forma separada (Holford, 1991)

# primero los efectos no lineales, manteniendo el drift en la dimensión de 
# periodo o en la dimensión de cohorte

m_apc <- glm(Dx ~ factor(A) + factor(P) + factor(C), 
             offset = log(Nx), family = poisson, data = data)
m_apc

# Además de las categorías de referencia (primera edad, primer año y primera cohorte)
# falta el coeficiente para la última cohorte ("NA")

# Así es como R (y otros programas como Stata) tratan la multicolinealidad: sacan una 
# variable para estimar el modelo. Esto es equivalente a imponer una restricción:
# los efectos de la primera y última cohorte son iguales a cero (=0). Al hacer esto, 
# removemos la pendiente de la dimensión de las cohortes (parecido a quitar la 
# tendencia, "detrended") y automaticamente el modelo identifica la pendiente 
# del período, por la interdependencia entre efectos lineales entre las tres 
# dimensiones. El drift queda asignado al período.  

# veamos esto graficando los coefficientes:
coef_apc <- extract_coeffs(m_apc)$coeffs

plot_apc <- 
  coef_apc %>% 
  ggplot()+
  geom_line(aes(value, effect))+
  facet_wrap(~tdim, scales = "free")+
  scale_y_log10()+
  theme_bw()

plot_apc


# Si ajustamos el modelo incluyendo primero las cohortes C 
# (A + C + P, en lugar de A + P + C), lo contrario ocurre. Veamos: 

m_acp <- glm(Dx ~ factor(A) + factor(C) + factor(P), 
             offset = log(Nx), family = poisson, data = data)
m_acp

coef_acp <- extract_coeffs(m_acp)$coeffs

plot_acp <- 
  coef_acp %>% 
  ggplot()+
  geom_line(aes(value, effect))+
  facet_wrap(~tdim, scales = "free")+
  scale_y_log10()+
  theme_bw()

plot_apc/plot_acp

# Cuántas veces más alta/baja es la mortalidad en cada período en comparación con 
# el período de referencia (1950)
# Cuántas veces más alta/baja es la mortalidad en cada cohorte en comparación con 
# las cohortes de referencia (1850 y 2023)



# Enfoque de Holford para ajustar APC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Separa los efectos lineales (drift) y no lineales (APC).
# Tanto los efectos de periodo como de cohorte serán "detrended".
# La categoría de referencia de P y C son, respectivamente, la primera y la última.

# Algunas pistas para manipular la fórmula en el modelo:
# El ajuste será el mismo, pero la interpretación de los coeficientes será
# más intuitiva:
# añadir un "-1" en la fórmula elimina el intercepto; en este caso,
# los coeficientes de edad pueden interpretarse directamente como tasas.
# "I()" nos permite manipular la fórmula; en este caso, para cambiar el
# periodo de referencia en la tendencia lineal (drift).

# tasas por edad desde la perspectiva de periodo 
apc_d <- glm(Dx ~ -1 + factor(A) + I(P-pmin) + factor(P) + factor(C), 
             offset = log(Nx), family = poisson, data = data)
apc_d


# tasas por edad desde la perspectiva de cohorte
# obtenemos la cohorte más antigua
cmin <- min(data$C)

acp_d <- glm(Dx ~  - 1  + factor(A) + I(C-cmin) + factor(C) + factor(P), 
             offset = log(Nx), family = poisson, data = data)
acp_d


coef_apc_d <- extract_coeffs(apc_d)$coeffs

coef_acp_d <- extract_coeffs(acp_d)$coeffs

plot_apc_d <- 
  coef_apc_d %>% 
  ggplot()+
  geom_line(aes(value, effect))+
  facet_wrap(~tdim, scales = "free")+
  scale_y_log10()+
  theme_bw()

plot_acp_d <-
  coef_acp_d %>% 
  ggplot()+
  geom_line(aes(value, effect))+
  facet_wrap(~tdim, scales = "free")+
  scale_y_log10()+
  theme_bw()

plot_apc_d/plot_acp_d

# de cuánto es el drift? o en otras palabras, cuál es el cambio secular de la 
# mortalidad en el tiempo (en variación porcentual anual)?
(extract_coeffs(apc_d)$drift - 1) * 100
(extract_coeffs(acp_d)$drift - 1) * 100


# ..............................................................................
# Método de Carstensen----
# ..............................................................................

# Además de separar efectos lineales (drift) y no lineales, elimina la tendencia
# de los efectos de período y de cohorte (suma/promedio = 0); 
# La referencia es la tendencia lineal de los cambios de mortalidad.
# Los resultados se interpretan en relación a la tendencia lineal: cuánto más 
# alta/baja es la mortalidad en un período o para una cohorte en relación con 
# la tendencia? O en otras palabras, cuánto diverge su mortalidad respecto a la 
# tendencia lineal?
# Las curvas de período y cohorte se interpretan como riesgos relativos (RR)
# en comparación con el promedio general de la tendencia lineal.

# Ventajas: 
# más flexibilidad en el modelado, permitiendo 
# - términos semiparamétricos (splines en lugar de factores)
# - diferentes formas de extraer la tendencia
# - mejores intervalos de confianza

# desventaja: 
# - mucho más complejo de modelar

# Pero Carstensen también hizo el paquete "Epi", que hace la vida mucho más fácil
# https://cran.r-project.org/web/packages/Epi/Epi.pdf

library(Epi)

# Se necesitan los siguientes nombres para las variables:
# A: edad
# P: año o período
# D: defunciones
# Y: población a riesgo/exposición

dt_carst <- 
  data %>% 
  rename(D = Dx,
         Y = Nx)

# Tasas de período:
apc_c <- 
  apc.fit(dt_carst, 
          model = "factor", 
          dr.extr = "1", 
          parm = "AdPC", 
          scale = 10^5)

apc_c # tasas (edad) + intervalos de confianza
# RR (período y cohorte) + intervalos de confianza

apc_c$Drift %>% as_tibble()

plot_carst(apc_c) 

# Tasas de cohorte:
acp_c <- 
  apc.fit(dt_carst, 
          model = "factor", 
          dr.extr = "1", 
          parm = "AdCP", 
          scale = 10^5)

plot_carst(acp_c) 

# Por último, podemos suavizar los efectos no linales de edad, período y cohorte 
# con splines en lugar de  utilizar variables categoricas.

apc_splines <- 
  apc.fit(dt_carst, 
          model = "bs", 
          #ref.p = 1950, # Si no indicamos un período de referencia, la referencia
          # es la tendencia lineal.
          npar = c(A = 10, P = 10, C = 15),
          dr.extr = "1", 
          parm = "AdPC", 
          scale = 10^5)

plot_carst(apc_splines)

