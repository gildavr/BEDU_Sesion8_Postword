# Obtención de datos para el estudio

df<-read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

################################################################################
# Análisis y limpieza de los datos
################################################################################

str(df)
summary(df)

# La variable IA, Inseguridad Alimentica en el Hogar, determina si presenta o no Inseguridad Alimentica, donde  0 = "No presenta IA", 1 = "Presenta IA"
# El tipo de esta variable lo redefinimos como lógico para determinar si presenta o no Inseguridad Alimenticia

class(df$IA)
df$IA <- as.logical(df$IA)
class(df$IA)

# La variable refin,Recursos financieros distintos al ingreso laboral, determina si el hogar tiene o no igresos extra, donde  0 = "no", 1 = "sí"
# El tipo de esta variable lo redefinimos como lógico para determinar si el hogar tiene ingresos extra.

class(df$refin)
df$refin <- as.logical(df$refin)
class(df$refin)

# Revisamos los datos para evaluar si hay valores faltantes
complete.cases(df)
sum(complete.cases(df))
sum(!complete.cases(df))

# Observamos que solo 20280 de 40809 registros tienen todos los valores. 
# Revisamos los valores nulos por columna
sapply(df, function(x) sum(is.na(x)))

# Eliminamos los registros con valores núlos
df <- na.omit(df)


################################################################################
# 1. Planteamiento del problema del caso
################################################################################

# ¿Qué determinantes socio-económicos producen inseguridad alimenticia?

################################################################################
# 2-. Análisis descriptivo de la información.
################################################################################

# ¿Cuánto gastan los hogares en productos saludables?

# Comenzamos con métodos tabulares para sintetizar la información
# Creación de table de distribución de frecuencias para la variable ln_als

# número de clases
k = ceiling(1 + 3.3 * log10(length(df$ln_als))) 
# ancho de clases
ac = (max(df$ln_als) - min(df$ln_als)) / k 
# secuencia de clase para hacer los cortes de clase correspondientes.
bins <- seq(min(df$ln_als), max(df$ln_als), by = ac)
ln_als.clases <- cut(df$ln_als, breaks = bins, include.lowest = TRUE, dig.lab = 8)
# tabla de distribución de frecuencias
dist.freq <- table(ln_als.clases)
transform(dist.freq,
          rel.freq = prop.table(Freq),
          cum.freq = cumsum(prop.table(Freq)))

# Continuamos revisando las medidas descriptivas

# Medidas de tendencia central
ln_als.mean <- mean(df$ln_als)
mean(df$ln_als, trim = .25)

# mediana
median(df$ln_als)

# Moda
# install.packages("DescTools")
library(DescTools)
Mode(df$ln_als)

# Medidas de dispersión
var(df$ln_als)
ln_als.sd <- sd(df$ln_als)
# Desviación en el cuartil central
IQR(df$ln_als)

# Medidas de posición
(quantile(df$ln_als, probs = c(0.25,0.5,0.75)))
(quantile(df$ln_als, probs = seq(0.1,0.9, by = 0.1)))
(quantile(df$ln_als, probs = seq(0.01,0.99, by = 0.01)))

# Descripción Gráfica

# Histograma
hist(df$ln_als, include.lowest = FALSE)

# Densidad de probabilidad
curve(dnorm(x, mean = ln_als.mean, sd = ln_als.sd), from = min(df$ln_als), to = max(df$ln_als)
      ,col = "blue", main = "Densidad de Probabilidad Normal para ln_als"
      ,ylab = "f(gasto)", xlab = "gasto")



# ¿Cuánto gastan los hogares en productos no saludables?

# Comenzamos con métodos tabulares para sintetizar la información
# Creación de table de distribución de frecuencias para la variable ln_als

# número de clases
k = ceiling(1 + 3.3 * log10(length(df$ln_alns))) 
# ancho de clases
ac = (max(df$ln_alns) - min(df$ln_alns)) / k 
# secuencia de clase para hacer los cortes de clase correspondientes.
bins <- seq(min(df$ln_alns), max(df$ln_alns), by = ac)
ln_alns.clases <- cut(df$ln_alns, breaks = bins, include.lowest = TRUE, dig.lab = 8)
# tabla de distribución de frecuencias
dist.freq <- table(ln_alns.clases)
transform(dist.freq,
          rel.freq = prop.table(Freq),
          cum.freq = cumsum(prop.table(Freq)))

# Continuamos revisando las medidas descriptivas

# Medidas de tendencia central
ln_alns.mean <- mean(df$ln_alns)
mean(df$ln_alns, trim = .25)

# mediana
median(df$ln_alns)

# Moda
Mode(df$ln_alns)

# Medidas de dispersión
var(df$ln_alns)
ln_alns.sd <- sd(df$ln_alns)
# Desviación en el cuartil central
IQR(df$ln_alns)

# Medidas de posición
(quantile(df$ln_alns, probs = c(0.25,0.5,0.75)))
(quantile(df$ln_alns, probs = seq(0.1,0.9, by = 0.1)))
(quantile(df$ln_alns, probs = seq(0.01,0.99, by = 0.01)))

# Descripción Gráfica

# Histograma
hist(df$ln_alns, include.lowest = FALSE)

# Densidad de probabilidad
curve(dnorm(x, mean = ln_alns.mean, sd = ln_alns.sd), from = min(df$ln_alns), to = max(df$ln_alns)
      ,col = "blue", main = "Densidad de Probabilidad Normal para ln_alns"
      ,ylab = "f(gasto)", xlab = "gasto")



################################################################################
# 3. Calcula probabilidades que nos permitan entender el problema en México.
################################################################################

################################################################################
# 4. Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México
################################################################################

# ¿Existe una correlación entre el consumo de productos no saludables y el nivel socio-económico?
# TODO: crear matrices de correlación para ver si se persive una correlación entre las variables de nivel economico, ingreso extra y el consumo de productos saludables y no saludables
# TODO: crear gráficas de dispersión para observar correlación entre consumo de alimentos saludables y no saludables y el nivel socio económico

################################################################################
# 5. Estima un modelo de regresión, lineal o logístico, para identificiar los determinanres de la inseguridad alimentaria en México
################################################################################
# TODO: hacer modelos de regresión con la variable dependiente inseguridad alimenticia y las variables indepedndientes
# nivel socio-económico, area, recursos financieros adicionales, gastos en alimentos saludables y no saludables
