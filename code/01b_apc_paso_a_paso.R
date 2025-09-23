rm(list=ls())
source("code/00_setup.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Obtener los datos para el análisis ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Defunciones y población por edad simple. Datos de WPP 2024:
dts_in <- read_rds("data/wpp_latam_Dx_Nx.rds")

head(dts)

unique(dts$year)
unique(dts$age)
unique(dts$country)

dts <- 
  dts_in %>% 
  filter(age %in% 10:90)

ct <- "Costa Rica"
sx <- "t"
pmin <- min(dts$year)
pmax <- max(dts$year)

dts2 <- 
  dts %>% 
  filter(country == ct,
         sex == sx,
         year %in% pmin:pmax)

# Crear categoría de cohorte = período - edad
data <- 
  dts2 %>% 
  rename(A = age,
         P = year) %>% 
  mutate(C = P - A) %>% 
  select(country, P, C, A, Dx, Nx)


#...............................................................................
# Análisis estadístico: modelos de edad, período y cohorte----
#...............................................................................

head(data)

## Modelos lineales---- 
# Vamos agregando una variable a la vez
# 1) Edad:

m_A <- glm(Dx ~ A, offset = log(Nx_mod), 
           family = poisson, data = data)
summary(m_A)
# Los resultados por edad (A) indican que cada año adicional de edad implica un 
# aumento de la mortalidad de exp(coeff(A)) %
# exp(0.07082922) --> aumento de 7%

p_mA <- data %>%
  mutate(mx = (Dx/Nx_mod) * 100000,
         pred_a = predict(m_A) %>% exp() * 1e5/ Nx_mod) %>% 
  ggplot()+
  geom_line(aes(x = A, y = mx, col = P, group = factor(P)))+
  geom_line(aes(x = A, y = pred_a), col = "red")+
  scale_y_log10()+
  theme_bw()+
  labs(title = paste0("Modelo: edad - ", unique(data$country)))

# 2) Edad-Período:

m_AP <- glm(Dx ~ A + P, offset = log(Nx_mod), 
            family = poisson, data = data)
summary(m_AP)

p_mAP <- data %>% 
  mutate(mx = (Dx/Nx_mod) * 100000,
         pred_a = predict(m_A) %>% exp() * 1e5/ Nx_mod,
         pred_ap = predict(m_AP) %>% exp() * 1e5/ Nx_mod) %>% 
  ggplot()+
  geom_line(aes(x = A, y = mx, col = P, group = factor(P)))+
  geom_line(aes(x = A, y = pred_a), col = "red")+
  geom_line(aes(x = A, y = pred_ap, group = P), col = "blue", linewidth = .1)+
  scale_y_log10()+
  theme_bw()+
  labs(title = paste0("Modelo: edad-período - ", unique(data$country)))

# 3) Edad-Período-Cohorte:

m_APC <- glm(Dx ~ A + P + C, offset = log(Nx_mod), 
             family = poisson, data = data)

summary(m_APC)

# El modelo no da resultado para la variable Cohorte. Esto es por la relación 
# perfectamente lineal entre edad, período y cohorte (A = P - C)
# Esto es justamente el problema de identificación: el modelo no sabe a cuál 
# variable atribuirle los efectos




#...............................................................................
## Explorando efectos lineales y no lineales del modelo APC----
#...............................................................................

# Los efectos no lineales se fueden estimar al utilizar las variables como 
# variables categoricas

# 1) Edad (categ.):

m_a <- glm(Dx ~ factor(A), offset = log(Nx_mod), 
           family = poisson, data = data)
summary(m_a)

p_ma <- data %>%
  mutate(mx = (Dx/Nx_mod) * 100000,
         pred_a = predict(m_a) %>% exp() * 1e5/ Nx_mod) %>% 
  ggplot()+
  geom_line(aes(x = A, y = mx, col = P, group = factor(P)))+
  geom_line(aes(x = A, y = pred_a), col = "red")+
  scale_y_log10()+
  theme_bw()+
  labs(title = paste0("Modelo: edad - ", unique(data$country)))
# Este modelo resulta en valores que se parecen más a la estructura de la 
# mortalidad por edad, pero no refleja el cambio de la mortalidad en el tiempo



# 2) Edad-Período:
## 2a) Efecto lineal: suponiendo que los cambios en el tiempo son siempre de la 
#      misma magnitud 

m_ap_lnr <- glm(Dx ~ factor(A) + P, offset=log(Nx_mod), 
                family = poisson, data = data)
m_ap_lnr

p_ap_lnr <- 
  data %>%
  mutate(mx = (Dx/Nx_mod) * 100000,
         pred_ap_lnr = predict(m_ap_lnr) %>% exp() * 1e5/ Nx_mod) %>% 
  ggplot()+
  geom_line(aes(x = A, y = mx, col = P, group = factor(P)))+
  geom_line(aes(x = A, y = pred_ap_lnr, group = P), col = "red", linewidth = .1)+
  scale_y_log10()+
  theme_bw()+
  labs(title = paste0("Edad-Período (lineal): ", unique(data$country)))



## 2b) Efecto no lineal: ahora dejamos que el cambio de la mortalidad en el tiempo 
##     varie según el año: 
m_ap_nlr <- glm(Dx ~ factor(A) + factor(P), offset=log(Nx_mod), 
                family = poisson, data = data)
# m_ap_nlr

p_ap_nlr <- 
  data %>%
  mutate(mx = (Dx/Nx_mod) * 100000,
         pred_ap_nlr = predict(m_ap_nlr) %>% exp() * 1e5/ Nx_mod) %>% 
  ggplot()+
  geom_line(aes(x = A, y = mx, col = P, group = factor(P)))+
  geom_line(aes(x = A, y = pred_ap_nlr, group = P), col = "blue", linewidth = .1)+
  scale_y_log10()+
  theme_bw()+
  labs(title = paste0("Edad-Período (no lineal): ", unique(data$country)))
# p_ap_nlr


# 3) Edad-Cohorte:
## 3a) Efecto lineal: suponiendo que los cambios entre cohortes son siempre de 
#      la misma magnitud 
m_ac_lnr <- glm(Dx ~ factor(A) + C, offset=log(Nx_mod), 
                family = poisson, data = data)
# m_ac_lnr

p_ac_lnr <- 
  data %>%
  mutate(mx = (Dx/Nx_mod) * 100000,
         pred_ac_lnr = predict(m_ac_lnr) %>% exp() * 1e5/ Nx_mod) %>% 
  ggplot()+
  geom_line(aes(x = A, y = mx, col = P, group = factor(P)))+
  geom_line(aes(x = A, y = pred_ac_lnr, group = C), col = "red", linewidth = .1)+
  scale_y_log10()+
  theme_bw()+
  labs(title = paste0("Edad-Cohorte (lineal): ", unique(data$country)))
# p_ac_lnr


## 3b) Efecto no lineal: ahora dejamos que el cambio de la mortalidad entre
##     cohortes cambie 
m_ac_nlr <- glm(Dx ~ factor(A) + factor(C), offset=log(Nx_mod), 
                family = poisson, data = data)
# m_ac_nlr

p_ac_nlr <- 
  data %>%
  mutate(mx = (Dx/Nx_mod) * 100000,
         pred_ac_nlr = predict(m_ac_nlr) %>% exp() * 1e5/ Nx_mod) %>% 
  ggplot()+
  geom_line(aes(x = A, y = mx, col = P, group = factor(P)))+
  geom_line(aes(x = A, y = pred_ac_nlr, group = C), col = "blue", linewidth = .1)+
  scale_y_log10()+
  theme_bw()+
  labs(title = paste0("Edad-Cohorte (no lineal): ", unique(data$country)))
# p_ac_nlr



#...............................................................................
# ¿Qué hemos hecho hasta ahora?
#...............................................................................

p_mA # Efecto de la edad (lineal)
p_ma # Efecto de la edad (no lineal)
p_mAP # Efectos lineales de edad y período
p_ap_lnr # Efecto de la edad (no lineal) y el período (lineal)
p_ap_nlr # Efectos no lineales de la edad y del período
p_ac_lnr # Efecto de la edad (no lineal) y la cohorte (lineal)
p_ac_nlr # Efectos no lineales de la edad y de la cohorte


# ¿Qué tan similares son los modelos de los efectos lineales de período y de cohorte?
m_ap_lnr
m_ac_lnr

# Mismo fit, mismo AIC, los coeficientes de P y C son iguales...
# pero difieren los coeficientes para las categorías de edad.
# m_ap_lnr (edad-período): exp(coeffs) --> tasas de mortalidad por edad en el 
#                          año de referencia (óptica transversal)
# m_ac_lnr (edad-cohorte): exp(coeffs) --> tasas de mortalidad por edad en la
#                          cohorte de referencia (óptica longitudinal)

# El coeficiente del cambio de la mortalidad en el tiempo (P o C) captura 
# la suma de los efectos de período y de cohorte.
# Esto se conoce como un "Age-drift model" 
# --> DRIFT ES EL CAMBIO LINEAL DE LA MORTALIDAD EN EL TIEMPO

# Transformar el coeficiente del cambio en el tiempo ("drift", en escala log.) 
# para obtener el cambio anual de mortalidad en términos de porcentaje:

# (exp(coeff(P))-1) * 100
# (exp(coeff(C))-1) * 100
m_ap_lnr$coefficients
(exp(-m_ap_lnr[["coefficients"]][["P"]])-1)*100 
# DRIFT: cambio lineal de la mortalidad en el tiempo
#        suma de los efectos lineales de período y cohorte    

get_drift(m_ap_lnr)

# Si comparamos los modelos no lineales, m_ap_nlr y m_ac_nlr, esos van a dar 
# resultados diferentes (fit, AIC, coeficientes de período y de cohorte)



#...............................................................................
# Ahora si, intentemos hacer nuevamente un modelo APC con efectos no lineales
# porque el modelo lineal no resultó...
#...............................................................................

m_apc <- glm(Dx ~ factor(A) + factor(P) + factor(C), 
             offset = log(Nx_mod), family = poisson, data = data)
m_apc

# Además de las categorías de referencia (primera edad, primer año y primera cohorte)
# falta el coeficiente para la última cohorte ("NA")

# Así es como R (y otros programas) tratan la multicolinealidad: sacan una 
# variable para estimar el modelo.Esto es equivalente a imponer una restricción:
# los efectos de la primera y última cohorte son iguales (=0). Al hacer esto, 
# fijamos una pendiente para la cohorte (parecido a quitar la tendencia, "detrended")
# y automaticamente el modelo identifica la pendiente del período, por la relación
# entre las variables. El drift queda repartido entre período y cohorte.  
# Si hubieramos hecho el modelo A + C + P (en lugar de A + P + C) lo contrario 
# habría ocurrido. 


# Graficar los resultados del modelo: primero necesitamos extraer los coeficientes

coef_apc <- extract_coeffs(m_apc)$coeffs

coef_apc %>% 
  ggplot()+
  geom_line(aes(value, effect))+
  facet_wrap(~tdim, scales = "free")+
  scale_y_log10()+
  theme_bw()

# Cuántas veces más alta/baja es la mortalidad en cada período en comparación con 
# el período de referencia (1950)
# Cuántas veces más alta/baja es la mortalidad en cada cohorte en comparación con 
# las cohortes de referencia (1850 y 2023)



