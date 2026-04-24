# Análisis de series temporales de influenza
# Apply different interpolator to predict flu cases

rm(list = ls())
set.seed(1)

# ── Librerías ─────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(lubridate)

# ── Cargar datos ──────────────────────────────────────────────

load("influenza_seasons.Rdata")

# ============================================================

#  1. REPORTE DIARIO: ¿HAY HUECOS EN LOS DATOS?

# ============================================================

daily_counts <- influenza_seasons %>%
  group_by(FECHA_SINTOMAS) %>%
  summarise(casos = n(), .groups = "drop") %>%
  arrange(FECHA_SINTOMAS)

# Secuencia completa de fechas

fechas_completas <- data.frame(
  FECHA_SINTOMAS = seq(
    min(influenza_seasons$FECHA_SINTOMAS, na.rm = TRUE),
    max(influenza_seasons$FECHA_SINTOMAS, na.rm = TRUE),
    by = "day"
  )
)

# Unir para detectar huecos

daily_full <- fechas_completas %>%
  left_join(daily_counts, by = "FECHA_SINTOMAS")

# Días sin reporte

dias_sin_reporte <- daily_full %>%
  filter(is.na(casos))

dias_sin_reporte

# Visualización

daily_counts2 <- influenza_seasons %>%
  group_by(TEMPORADA, FECHA_SINTOMAS) %>%
  summarise(casos = n(), .groups = "drop")

ggplot(daily_counts2, aes(x = FECHA_SINTOMAS, y = casos, color = TEMPORADA)) +
  geom_point() +
  labs(
    title = "Casos diarios por temporada",
    x = "Fecha de síntomas",
    y = "Casos"
  ) +
  theme_minimal()

#  OBSERVACIÓN:

# No todos los días tienen reporte → hay huecos en la serie

# ============================================================

#  2. REPORTE SEMANAL: ESTRUCTURA Y PERIODICIDAD

# ============================================================

# Verificar cobertura de semanas

influenza_seasons %>%
  group_by(TEMPORADA) %>%
  summarise(
    min_sem = min(SEMANA_RELATIVA),
    max_sem = max(SEMANA_RELATIVA),
    n_semanas = n_distinct(SEMANA_RELATIVA)
  )

# Conteo semanal

weekly_counts <- influenza_seasons %>%
  group_by(TEMPORADA, SEMANA_RELATIVA) %>%
  summarise(casos = n(), .groups = "drop")

# Comparación directa

ggplot(weekly_counts, aes(x = SEMANA_RELATIVA, y = casos, color = TEMPORADA)) +
  geom_line() +
  geom_point(size = 1) +
  labs(
    title = "Casos semanales por temporada",
    x = "Semana relativa (desde septiembre)",
    y = "Casos"
  ) +
  theme_minimal()

# Facet para ver estructura interna

ggplot(weekly_counts, aes(x = SEMANA_RELATIVA, y = casos)) +
  geom_line(color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  facet_wrap(~ TEMPORADA) +
  labs(
    title = "Estructura semanal por temporada",
    x = "Semana relativa",
    y = "Casos"
  ) +
  theme_minimal()

# OBSERVACIÓN:

# Existe un patrón recurrente → evidencia de periodicidad

# ============================================================

#  3. INTERPOLACIÓN (SIN IMPUTACIÓN PREVIA)

# ============================================================

# Seleccionar una temporada

datos_temp <- weekly_counts %>%
  filter(TEMPORADA == "2024-2025") %>%
  arrange(SEMANA_RELATIVA)

x <- datos_temp$SEMANA_RELATIVA
y <- datos_temp$casos

# Malla más fina

xout <- seq(min(x), max(x), by = 0.1)

# Interpolación lineal

interp_lin <- approx(x, y, xout = xout)

# Interpolación spline

interp_spl <- spline(x, y, xout = xout)

df_interp <- data.frame(
  x = xout,
  lineal = interp_lin$y,
  spline = interp_spl$y
)


df_plot <- bind_rows(
  datos_temp %>%
    transmute(x = SEMANA_RELATIVA,
              y = casos,
              metodo = "observado"),
  
  df_interp %>%
    transmute(x = x,
              y = lineal,
              metodo = "lineal"),
  
  df_interp %>%
    transmute(x = x,
              y = spline,
              metodo = "spline")
)

# Visualización
ggplot(df_plot, aes(x = x, y = y, color = metodo)) +
  geom_line(data = df_plot %>% filter(metodo != "observado"),
            linewidth = 1) +
  
  geom_point(data = df_plot %>% filter(metodo == "observado"),
             size = 2, color = "black") +
  
  labs(
    title = "Interpolación usando nodos observados",
    x = "Semana relativa",
    y = "Casos",
    color = "Método"
  ) +
  scale_color_manual(
    values = c(
      "lineal" = "blue",
      "spline" = "red"
    ),
    labels = c(
      "lineal" = "Lineal",
      "spline" = "Spline cúbico"
    )
  ) +
  theme_minimal()

# IDEA CLAVE:

# NO imputamos datos faltantes

# Usamos únicamente nodos observados

#QUE ERROR SE OBERVA

# ============================================================

# 4. ACTIVIDAD PARA EL ALUMNO

# ============================================================

# 1. Probar diferentes condiciones de frontera en splines

# 2. Intentar "pegar" dos temporadas suavemente

# 3. Comparar:

# - interpolación lineal vs spline

# - estabilidad del pico

# 4. ¿La periodicidad justifica un spline periódico?

# ============================================================

# 5. ANÁLISIS EXPLORATORIO (EXTENSIÓN)

# ============================================================

# Por grupo de edad

edad_counts <- influenza_seasons %>%
  group_by(GRUPO_EDAD, SEMANA_RELATIVA) %>%
  summarise(casos = n(), .groups = "drop")

ggplot(edad_counts, aes(x = SEMANA_RELATIVA, y = casos, color = GRUPO_EDAD)) +
  geom_line() +
  labs(
    title = "Casos por grupo de edad",
    x = "Semana relativa",
    y = "Casos"
  ) +
  theme_minimal()

# Por sector

sector_counts <- influenza_seasons %>%
  group_by(SECTOR, SEMANA_RELATIVA) %>%
  summarise(casos = n(), .groups = "drop")

ggplot(sector_counts, aes(x = SEMANA_RELATIVA, y = casos, color = SECTOR)) +
  geom_line() +
  labs(
    title = "Casos por sector",
    x = "Semana relativa",
    y = "Casos"
  ) +
  theme_minimal()




# ============================================================

#  ANÁLISIS POR SECTOR: SERIES RELATIVAS Y ACUMULADAS

# ============================================================

# ── 1. Conteo semanal por sector ─────────────────────────────

sector_weekly <- influenza_seasons %>%
  group_by(SECTOR, SEMANA_RELATIVA) %>%
  summarise(casos = n(), .groups = "drop")

# ============================================================

# OTRA INSPECCION: NORMALIZACIÓN (relativo al máximo)

# ============================================================

sector_relative <- sector_weekly %>%
  group_by(SECTOR) %>%
  mutate(
    max_sector = max(casos, na.rm = TRUE),
    casos_rel = casos / max_sector
  ) %>%
  ungroup()

# ── Visualización relativa ───────────────────────────────────

ggplot(sector_relative,
       aes(x = SEMANA_RELATIVA, y = casos_rel, color = SECTOR)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Dinámica relativa por sector",
    x = "Semana relativa",
    y = "Casos (normalizados por máximo)"
  ) +
  theme_minimal()

# INTERPRETACIÓN:

# - Todas las curvas están en escala [0,1]

# - Permite comparar forma (no magnitud)

# - ¿Quién sube antes? ¿quién tiene pico más ancho?

# ============================================================

# 🔹 3. ACUMULADOS

# ============================================================

sector_cumulative <- sector_weekly %>%
  arrange(SECTOR, SEMANA_RELATIVA) %>%
  group_by(SECTOR) %>%
  mutate(
    acumulado = cumsum(casos)
  ) %>%
  ungroup()

# ── Visualización acumulada ──────────────────────────────────

ggplot(sector_cumulative,
       aes(x = SEMANA_RELATIVA, y = acumulado, color = SECTOR)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Casos acumulados por sector",
    x = "Semana relativa",
    y = "Casos acumulados"
  ) +
  theme_minimal()

#  INTERPRETACIÓN:

# - La pendiente indica intensidad de transmisión

# - Curvas más empinadas → brotes más rápidos

# - Comparar momentos de desaceleración

# ============================================================

# 🔹 4. ACUMULADO NORMALIZADO (OPCIONAL, MUY POTENTE)

# ============================================================

sector_cum_rel <- sector_cumulative %>%
  group_by(SECTOR) %>%
  mutate(
    total = max(acumulado),
    acumulado_rel = acumulado / total
  ) %>%
  ungroup()

ggplot(sector_cum_rel,
       aes(x = SEMANA_RELATIVA, y = acumulado_rel, color = SECTOR)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Progreso acumulado relativo por sector",
    x = "Semana relativa",
    y = "Proporción acumulada"
  ) +
  theme_minimal()

# 👉 INTERPRETACIÓN:

# - Todas terminan en 1

# - Permite comparar velocidad de propagación

# - ¿Qué sector alcanza 50% más rápido?

