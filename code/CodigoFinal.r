##############################################################
# Factores que afectan la cantidad de check-ins diarios en los hoteles Caesars de Las Vegas.
# Business Analytics - Pontificia Universidad Javeriana
# Integrantes: Valeria Garcia y Valery Ramirez
# Fecha:  2025-11-14
##############################################################

# 1. CARGA Y PREPARACIÓN DE DATOS =============================================

install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "lubridate", "broom"))

# Cargar librerías necesarias
library(readxl)      # Para leer archivos Excel
library(dplyr)       # Para manipulación de datos
library(tidyr)       # Para limpieza de datos
library(ggplot2)     # Para visualizaciones
library(lubridate)   # Para manejo de fechas
library(broom)       # Para trabajar con modelos estadísticos

# Leer los datos
caesars_data <- read_excel("CaesarsXLS.xlsx")

# Examinar la estructura de los datos
str(caesars_data)
summary(caesars_data)

# Convertir la columna de fecha a formato Date
caesars_data <- caesars_data %>%
  mutate(Date = as.Date(Date))

# Asegurar que las variables dummy sean numéricas (0 o 1)
dummy_vars <- c("SuperBowl", "Valentines", "NY", "Easter", "LaborDay", "Xmas")
caesars_data <- caesars_data %>%
  mutate(across(all_of(dummy_vars), as.numeric))

# Verificar y manejar valores faltantes
sum(is.na(caesars_data)) # Contar valores faltantes
caesars_data <- na.omit(caesars_data) # Eliminar filas con valores faltantes

# 2. ESTADÍSTICAS DESCRIPTIVAS =============================================

# Crear directorio para resultados si no existe
dir.create("resultados", showWarnings = FALSE)

# Estadísticas descriptivas de check-ins
checkins_stats <- caesars_data %>%
  summarise(
    Promedio = mean(checkins),
    Min = min(checkins),
    Max = max(checkins),
    Desv_Est = sd(checkins)
  )
print("Estadísticas de Check-ins:")
print(checkins_stats)

# Guardar estadísticas descriptivas en CSV
write.csv(checkins_stats, "resultados/estadisticas_descriptivas.csv", row.names = FALSE)

# Promedio de check-ins por mes
checkins_por_mes <- caesars_data %>%
  mutate(Mes = month(Date, label = TRUE)) %>%
  group_by(Mes) %>%
  summarise(
    Promedio_Checkins = mean(checkins),
    N = n()
  )
print("\nPromedio de Check-ins por Mes:")
print(checkins_por_mes)
write.csv(checkins_por_mes, "resultados/promedios_por_mes.csv", row.names = FALSE)

# Promedio de check-ins por día de la semana
checkins_por_dia <- caesars_data %>%
  mutate(DiaSemana = wday(Date, label = TRUE)) %>%
  group_by(DiaSemana) %>%
  summarise(
    Promedio_Checkins = mean(checkins),
    N = n()
  )
print("\nPromedio de Check-ins por Día de la Semana:")
print(checkins_por_dia)
write.csv(checkins_por_dia, "resultados/promedios_por_dia.csv", row.names = FALSE)

# 3. VISUALIZACIONES =============================================

# Gráfico de línea temporal de check-ins
p1 <- ggplot(caesars_data, aes(x = Date, y = checkins)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Evolución de Check-ins a lo Largo del Tiempo",
       x = "Fecha",
       y = "Número de Check-ins") +
  theme_minimal()

# Guardar gráfico
ggsave("resultados/evolucion_checkins.png", p1, width = 12, height = 6, dpi = 300)

# Gráfico de barras de promedio de check-ins por mes
p2 <- caesars_data %>%
  mutate(Mes = month(Date, label = TRUE)) %>%
  group_by(Mes) %>%
  summarise(Promedio_Checkins = mean(checkins)) %>%
  ggplot(aes(x = Mes, y = Promedio_Checkins)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Promedio de Check-ins por Mes",
       x = "Mes",
       y = "Promedio de Check-ins") +
  theme_minimal()

# Guardar gráfico
ggsave("resultados/promedio_checkins_por_mes.png", p2, width = 10, height = 6, dpi = 300)

# Gráfico comparativo de check-ins en días con eventos vs sin eventos
eventos_comparacion <- caesars_data %>%
  mutate(Tiene_Evento = if_else(SuperBowl == 1 | Valentines == 1 | NY == 1 |
                               Easter == 1 | LaborDay == 1 | Xmas == 1,
                               "Con Evento", "Sin Evento")) %>%
  group_by(Tiene_Evento) %>%
  summarise(Promedio_Checkins = mean(checkins))

p3 <- ggplot(eventos_comparacion, aes(x = Tiene_Evento, y = Promedio_Checkins, fill = Tiene_Evento)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Check-ins: Días con Eventos vs Sin Eventos",
       x = "Tipo de Día",
       y = "Promedio de Check-ins") +
  theme_minimal() +
  scale_fill_manual(values = c("Con Evento" = "darkgreen", "Sin Evento" = "lightgreen"))

# Guardar gráfico
ggsave("resultados/comparacion_eventos.png", p3, width = 8, height = 6, dpi = 300)
write.csv(eventos_comparacion, "resultados/comparacion_eventos.csv", row.names = FALSE)

# 4. MODELO DE REGRESIÓN LINEAL SIMPLE =============================================

# Modelo de regresión lineal simple
modelo1 <- lm(checkins ~ `FIT ADR`, data = caesars_data)

# Mostrar resumen del modelo
summary(modelo1)

# Visualizar la relación
p4 <- ggplot(caesars_data, aes(x = `FIT ADR`, y = checkins)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relación entre Precio Promedio y Check-ins",
       x = "Precio Promedio (FIT ADR)",
       y = "Número de Check-ins") +
  theme_minimal()

# Guardar gráfico y resultados del modelo
ggsave("resultados/relacion_precio_checkins.png", p4, width = 10, height = 6, dpi = 300)
capture.output(summary(modelo1), file = "resultados/modelo1_resultados.txt")

# 5. MODELO DE REGRESIÓN LINEAL MÚLTIPLE =============================================

# Añadir variable de día de la semana
caesars_data <- caesars_data %>%
  mutate(DiaSemana = as.factor(wday(Date)))

# Modelo de regresión múltiple
modelo2 <- lm(checkins ~ `FIT ADR` + Casino + Group + SE + SuperBowl + 
              Valentines + NY + Easter + LaborDay + Xmas + DiaSemana,
              data = caesars_data)

# Mostrar resumen del modelo
summary(modelo2)

# Identificar variables significativas
coef_summary <- tidy(modelo2) %>%
  mutate(
    significativo = ifelse(p.value < 0.05, "Sí", "No")
  )

# Mostrar y guardar variables significativas
variables_significativas <- coef_summary %>%
      filter(significativo == "Sí") %>%
      select(term, estimate, p.value)
cat("\nVariables significativas (p < 0.05):\n")
print(variables_significativas)

# Guardar resultados del modelo y variables significativas
capture.output(summary(modelo2), file = "resultados/modelo2_resultados.txt")
write.csv(variables_significativas, "resultados/variables_significativas.csv", row.names = FALSE)

# 6. MODELO CON TRATAMIENTOS Y CONTROLES =============================================

# Crear variable de tratamiento
caesars_data <- caesars_data %>%
  mutate(Treatment = as.numeric(SuperBowl == 1 | Valentines == 1 | NY == 1 |
                               Easter == 1 | LaborDay == 1 | Xmas == 1))

# Modelo con tratamiento
modelo3 <- lm(checkins ~ Treatment + `FIT ADR` + Casino + Group + SE,
              data = caesars_data)

# Mostrar y guardar resumen del modelo
summary(modelo3)
capture.output(summary(modelo3), file = "resultados/modelo3_resultados.txt")

# Calcular el efecto promedio del tratamiento
treatment_effect <- caesars_data %>%
  group_by(Treatment) %>%
  summarise(
    Promedio_Checkins = mean(checkins),
    N = n()
  )

print("Efecto promedio del tratamiento:")
print(treatment_effect)
write.csv(treatment_effect, "resultados/efecto_tratamiento.csv", row.names = FALSE)

# 7. COMPARACIÓN DE MODELOS =============================================

# Crear función para extraer métricas
get_model_metrics <- function(model) {
  list(
    R2 = summary(model)$r.squared,
    R2_adj = summary(model)$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model),
    RMSE = sqrt(mean(model$residuals^2))
  )
}

# Obtener métricas para cada modelo
metrics1 <- get_model_metrics(modelo1)
metrics2 <- get_model_metrics(modelo2)
metrics3 <- get_model_metrics(modelo3)

# Crear data frame de comparación
comparacion_modelos <- data.frame(
  Modelo = c("Modelo Simple", "Modelo Múltiple", "Modelo Tratamiento"),
  R2 = c(metrics1$R2, metrics2$R2, metrics3$R2),
  R2_Ajustado = c(metrics1$R2_adj, metrics2$R2_adj, metrics3$R2_adj),
  AIC = c(metrics1$AIC, metrics2$AIC, metrics3$AIC),
  BIC = c(metrics1$BIC, metrics2$BIC, metrics3$BIC),
  RMSE = c(metrics1$RMSE, metrics2$RMSE, metrics3$RMSE)
)

# Mostrar y guardar tabla de comparación
print("Comparación de Modelos:")
print(comparacion_modelos)
write.csv(comparacion_modelos, "resultados/comparacion_modelos.csv", row.names = FALSE)

# Visualizar R² ajustado
p5 <- ggplot(comparacion_modelos, aes(x = Modelo, y = R2_Ajustado, fill = Modelo)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de R² Ajustado entre Modelos",
       x = "Modelo",
       y = "R² Ajustado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardar gráfico de comparación
ggsave("resultados/comparacion_r2_ajustado.png", p5, width = 10, height = 6, dpi = 300)