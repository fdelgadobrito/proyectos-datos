# ============================================================
# 03_OE3.R
# OE3. Modelar el comportamiento dinámico del sistema de inventario
# mediante cadenas de Markov en tiempo discreto, representando los niveles
# de stock como estados del sistema y calculando las probabilidades de
# quiebre asociadas a distintas políticas de reabastecimiento.
#
# Entrada: data/clean/ajuste_demanda.rds, data/clean/dlt_simulacion.rds
# Salida:  data/clean/markov_resultados.rds
#          output/tablas/tabla_8_6_validacion_markov.csv
#
# NOTA METODOLÓGICA: el par (s, S) usado aquí es SOLO un punto de partida
# heurístico, necesario para demostrar y validar la mecánica de la cadena.
# La búsqueda del par (s*, S*) que minimiza el costo total esperado sujeto
# al nivel de servicio objetivo es tarea del OE4 (script 04_OE4.R), que
# reutiliza exactamente esta misma función de construcción de matriz.
# ============================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

set.seed(2026)

CLEAN <- "data/clean"
TABLAS <- "output/tablas"

ajuste_demanda <- readRDS(file.path(CLEAN, "ajuste_demanda.rds"))
dlt <- read_csv(file.path(TABLAS, "tabla_8_5_dlt.csv"), show_col_types = FALSE)

# ------------------------------------------------------------
# Función auxiliar: discretizar la demanda mensual del modelo de dos partes
# a soporte entero {0, 1, 2, ..., Dmax}, para poder construir una matriz de
# transición de estados discretos.
# ------------------------------------------------------------
discretizar_demanda <- function(p_venta, dist_ganadora, par1, par2, d_max) {
  # P(D = 0) = 1 - p_venta (no hubo venta ese periodo)
  # P(D = d), d = 1..d_max-1: p_venta * [F(d+0.5) - F(d-0.5)]  (continuidad)
  # P(D >= d_max): resto de la masa (cola derecha agrupada en el último estado)
  pcdf <- switch(dist_ganadora,
    normal = function(x) pnorm(x, par1, par2),
    gamma  = function(x) pgamma(x, shape = par1, rate = par2),
    lnorm  = function(x) plnorm(x, par1, par2),
    constante = function(x) as.numeric(x >= par1)
  )
  probs <- numeric(d_max + 1)
  probs[1] <- 1 - p_venta
  for (d in 1:(d_max - 1)) {
    probs[d + 1] <- p_venta * (pcdf(d + 0.5) - pcdf(d - 0.5))
  }
  probs[d_max + 1] <- max(0, 1 - sum(probs[1:d_max]))
  probs / sum(probs)
}

# ------------------------------------------------------------
# 8.3.1 Definición del espacio de estados
# ------------------------------------------------------------
# El estado del sistema es el nivel de inventario a inicio de periodo
# (periodo = 1 mes, consistente con la granularidad de los datos de
# demanda). El espacio de estados es {0, 1, ..., S}, donde S (cantidad
# máxima de pedido) se fija, para esta demostración, en S = s_demo +
# demanda promedio mensual redondeada; s_demo se aproxima con el
# percentil 95 de la demanda durante el lead time (DLT) obtenida en el OE2
# (stock de seguridad informal, punto de partida para el OE4).

parametros_sku <- ajuste_demanda %>%
  dplyr::filter(!is.na(dist_ganadora)) %>%
  left_join(dlt %>% dplyr::select(sku, dlt_p95, dlt_media_val), by = "sku") %>%
  mutate(
    demanda_media_mensual = case_when(
      dist_ganadora == "gamma"     ~ p_venta * (par1 / par2),
      dist_ganadora == "lnorm"     ~ p_venta * exp(par1 + par2^2 / 2),
      dist_ganadora == "normal"    ~ p_venta * par1,
      dist_ganadora == "constante" ~ p_venta * par1
    ),
    s_demo = pmax(1, round(dlt_p95)),
    S_demo = pmax(s_demo + 1, round(s_demo + demanda_media_mensual))
  )

# ------------------------------------------------------------
# Construcción de la matriz de transición para una política (s, S) dada
# ------------------------------------------------------------
# version = "incorrecta": cadena de solo consumo, SIN lógica de reposición
#   (error de diseño del primer intento, ver 8.3.2)
# version = "correcta": incorpora la regla de reposición (s, S)
construir_matriz <- function(s, S, p_venta, dist_ganadora, par1, par2, version = "correcta") {
  n_estados <- S + 1  # estados 0..S
  P <- matrix(0, n_estados, n_estados)
  d_max <- S + 5  # cola de demanda agrupada más allá de esto
  dens_demanda <- discretizar_demanda(p_venta, dist_ganadora, par1, par2, d_max)

  for (i in 0:S) {
    if (version == "correcta" && i <= s) {
      # Se repone a S antes de que ocurra la demanda del periodo (el lead
      # time, de 7 a 30 días, es menor al periodo de revisión de 1 mes)
      inv_inicio <- S
    } else {
      # version "incorrecta": nunca se repone, o inventario por sobre s
      inv_inicio <- i
    }
    for (d in 0:d_max) {
      j <- max(inv_inicio - d, 0)
      prob_d <- dens_demanda[d + 1]
      P[i + 1, j + 1] <- P[i + 1, j + 1] + prob_d
    }
  }
  P
}

# ------------------------------------------------------------
# 8.3.2 Primer intento de matriz de transición (y su error de diseño)
# ------------------------------------------------------------
# Se ilustra con el SKU de mayor venta: se construye la cadena SIN regla
# de reposición (como si la sucursal nunca reordenara). El resultado es
# una cadena degenerada: el estado 0 se vuelve ABSORBENTE (una vez que el
# inventario llega a 0, no hay manera de volver a subir), y la
# distribución estacionaria colapsa a un 100% de probabilidad en el
# estado 0 -- un resultado sin utilidad práctica, porque no refleja que en
# la realidad la sucursal SÍ repone stock.

sku_ejemplo <- parametros_sku %>% arrange(desc(p_venta * S_demo)) %>% slice(1)
# usamos el mismo SKU de mayor venta que en OE2 si está disponible
if ("739-120-035-000" %in% parametros_sku$sku) {
  sku_ejemplo <- parametros_sku %>% dplyr::filter(sku == "739-120-035-000")
}

cat("=== 8.3.2 Primer intento (sin regla de reposición) - SKU", sku_ejemplo$sku, "===\n")
cat("s_demo =", sku_ejemplo$s_demo, " S_demo =", sku_ejemplo$S_demo, "\n")

P_incorrecta <- construir_matriz(
  sku_ejemplo$s_demo, sku_ejemplo$S_demo, sku_ejemplo$p_venta,
  sku_ejemplo$dist_ganadora, sku_ejemplo$par1, sku_ejemplo$par2,
  version = "incorrecta"
)
cat("Suma de la fila del estado 0 (P[0,0]):", round(P_incorrecta[1, 1], 4), "\n")
cat("ERROR DE DISEÑO: el estado 0 es absorbente (P[0,0] = 1), porque la matriz\n")
cat("no contempla que al llegar a s se repone stock. La cadena predice que,\n")
cat("tarde o temprano, el sistema se queda en 0 para siempre -- lo cual\n")
cat("contradice la operación real observada en los datos (la sucursal SÍ\n")
cat("sigue vendiendo mes a mes).\n\n")

# ------------------------------------------------------------
# 8.3.3 Corrección del diseño de la política
# ------------------------------------------------------------
cat("=== 8.3.3 Corrección: se incorpora la regla de reposición (s, S) ===\n")

P_correcta <- construir_matriz(
  sku_ejemplo$s_demo, sku_ejemplo$S_demo, sku_ejemplo$p_venta,
  sku_ejemplo$dist_ganadora, sku_ejemplo$par1, sku_ejemplo$par2,
  version = "correcta"
)
cat("Suma de filas (deben ser todas 1):", round(range(rowSums(P_correcta)), 6), "\n")
cat("P[0,0] tras la corrección:", round(P_correcta[1, 1], 4),
    "(ya no es absorbente: hay probabilidad de volver a S)\n\n")

# ------------------------------------------------------------
# 8.3.4 Cálculo de la distribución estacionaria
# ------------------------------------------------------------
# Se resuelve pi %*% P = pi, sum(pi) = 1, reemplazando una ecuación del
# sistema por la restricción de normalización (método estándar de solución
# lineal, sin necesidad de paquetes externos de cadenas de Markov).

calcular_estacionaria <- function(P) {
  n <- nrow(P)
  A <- t(P) - diag(n)
  A[n, ] <- 1
  b <- c(rep(0, n - 1), 1)
  pi_est <- tryCatch(solve(A, b), error = function(e) rep(NA_real_, n))
  pi_est[pi_est < 0 & pi_est > -1e-8] <- 0  # limpiar error numérico
  pi_est
}

pi_correcta <- calcular_estacionaria(P_correcta)
cat("=== 8.3.4 Distribución estacionaria (SKU", sku_ejemplo$sku, ") ===\n")
cat("P(estado = 0) [quiebre de stock] =", round(pi_correcta[1], 4), "\n")
cat("P(estado <= s) =", round(sum(pi_correcta[1:(sku_ejemplo$s_demo + 1)]), 4), "\n\n")

# ------------------------------------------------------------
# 8.3.5 Validación cruzada del modelo (vs. simulación de Monte Carlo)
# ------------------------------------------------------------
simular_politica_mc <- function(s, S, p_venta, dist_ganadora, par1, par2, n_periodos = 5000) {
  estado <- S
  historial <- numeric(n_periodos)
  rdist <- switch(dist_ganadora,
    normal = function(n) pmax(round(rnorm(n, par1, par2)), 0),
    gamma  = function(n) round(rgamma(n, shape = par1, rate = par2)),
    lnorm  = function(n) round(rlnorm(n, par1, par2)),
    constante = function(n) rep(round(par1), n)
  )
  ventas <- rbinom(n_periodos, 1, p_venta)
  cantidades <- rdist(n_periodos)
  demandas <- ventas * cantidades
  for (t in seq_len(n_periodos)) {
    if (estado <= s) estado <- S
    estado <- max(estado - demandas[t], 0)
    historial[t] <- estado
  }
  historial
}

hist_mc <- simular_politica_mc(
  sku_ejemplo$s_demo, sku_ejemplo$S_demo, sku_ejemplo$p_venta,
  sku_ejemplo$dist_ganadora, sku_ejemplo$par1, sku_ejemplo$par2
)
p_quiebre_mc <- mean(hist_mc == 0)
p_quiebre_analitico <- pi_correcta[1]

cat("=== 8.3.5 Validación cruzada (5,000 periodos simulados) ===\n")
cat("P(quiebre) analítico (cadena de Markov):", round(p_quiebre_analitico, 4), "\n")
cat("P(quiebre) simulado (Monte Carlo):      ", round(p_quiebre_mc, 4), "\n")
cat("Diferencia absoluta:", round(abs(p_quiebre_analitico - p_quiebre_mc), 4), "\n\n")

# ------------------------------------------------------------
# Aplicar la misma mecánica a los 69 SKU críticos, con su (s,S) heurístico,
# para dejar la tabla de validación completa (resto en anexo).
# ------------------------------------------------------------
validar_sku <- function(fila) {
  P <- construir_matriz(fila$s_demo, fila$S_demo, fila$p_venta,
                         fila$dist_ganadora, fila$par1, fila$par2, version = "correcta")
  pi_est <- calcular_estacionaria(P)
  p_quiebre_analitico <- pi_est[1]
  hist_mc <- simular_politica_mc(fila$s_demo, fila$S_demo, fila$p_venta,
                                  fila$dist_ganadora, fila$par1, fila$par2, n_periodos = 3000)
  p_quiebre_mc <- mean(hist_mc == 0)
  tibble(p_quiebre_analitico = p_quiebre_analitico, p_quiebre_mc = p_quiebre_mc,
         diferencia = abs(p_quiebre_analitico - p_quiebre_mc))
}

resultados_markov <- parametros_sku %>%
  rowwise() %>%
  mutate(val = list(validar_sku(pick(everything())))) %>%
  ungroup() %>%
  unnest(val)

cat("=== Validación cruzada sobre los", nrow(resultados_markov), "SKU críticos ===\n")
cat("Diferencia promedio |analítico - Monte Carlo|:", round(mean(resultados_markov$diferencia), 4), "\n")
cat("Diferencia máxima:", round(max(resultados_markov$diferencia), 4), "\n")

saveRDS(resultados_markov, file.path(CLEAN, "markov_resultados.rds"))
write_csv(
  resultados_markov %>% dplyr::select(sku, descripcion, s_demo, S_demo, p_quiebre_analitico, p_quiebre_mc, diferencia),
  file.path(TABLAS, "tabla_8_6_validacion_markov.csv")
)

cat("\n=== 03_OE3.R finalizado. Salidas en data/clean/ y output/tablas/ ===\n")
