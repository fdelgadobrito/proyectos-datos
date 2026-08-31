# ============================================================
# 05_OE5.R
# OE5. Evaluar la viabilidad técnica y económica de la propuesta de modelo
# de gestión de inventario, comparando el desempeño esperado de la
# política óptima propuesta frente a la situación actual mediante
# indicadores cuantificables (frecuencia de quiebres de stock, costo total
# de inventario y nivel de servicio), y entregando recomendaciones de
# implementación para la organización.
#
# CORRECCIÓN METODOLÓGICA CLAVE (respecto al diagnóstico ChatGPT original,
# punto 1 de la bitácora): la "situación actual" NO se aproxima con un
# escenario teórico sin stock de seguridad (S_actual <- mean(dlt)), sino
# con el STOCK FÍSICO REAL observado en el inventario de marzo 2026. Donde
# el SKU crítico no aparece en ese inventario, se documenta explícitamente
# que el conteo físico registró 0 unidades en esa fecha (evidencia
# empírica directa del problema de quiebres, no un supuesto).
# ============================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

set.seed(2026)

CLEAN <- "data/clean"
TABLAS <- "output/tablas"

politica_optima <- readRDS(file.path(CLEAN, "politica_optima.rds"))
inventario <- readRDS(file.path(CLEAN, "inventario_marzo2026.rds"))

# Funciones reutilizadas de 04_OE4.R (misma mecánica de la cadena de Markov)
discretizar_demanda <- function(p_venta, dist_ganadora, par1, par2, d_max, unidad = 1) {
  pcdf <- switch(dist_ganadora,
    normal = function(x) pnorm(x, par1, par2),
    gamma  = function(x) pgamma(x, shape = par1, rate = par2),
    lnorm  = function(x) plnorm(x, par1, par2),
    constante = function(x) as.numeric(x >= par1)
  )
  probs <- numeric(d_max + 1)
  probs[1] <- 1 - p_venta
  for (d in 1:(d_max - 1)) {
    lo <- (d - 0.5) * unidad; hi <- (d + 0.5) * unidad
    probs[d + 1] <- p_venta * (pcdf(hi) - pcdf(lo))
  }
  probs[d_max + 1] <- max(0, 1 - sum(probs[1:d_max]))
  probs / sum(probs)
}
construir_matriz <- function(s, S, p_venta, dist_ganadora, par1, par2, unidad = 1) {
  n_estados <- S + 1
  P <- matrix(0, n_estados, n_estados)
  d_max <- S + 5
  dens <- discretizar_demanda(p_venta, dist_ganadora, par1, par2, d_max, unidad)
  for (i in 0:S) {
    inv_inicio <- if (i <= s) S else i
    for (d in 0:d_max) {
      j <- max(inv_inicio - d, 0)
      P[i + 1, j + 1] <- P[i + 1, j + 1] + dens[d + 1]
    }
  }
  list(P = P, dens = dens, d_max = d_max)
}
calcular_estacionaria <- function(P, tol = 1e-10, max_iter = 2000) {
  n <- nrow(P)
  if (n <= 40) {
    A <- t(P) - diag(n); A[n, ] <- 1
    b <- c(rep(0, n - 1), 1)
    pi_est <- tryCatch(solve(A, b), error = function(e) NULL)
    if (!is.null(pi_est)) { pi_est[pi_est < 0 & pi_est > -1e-8] <- 0; return(pi_est) }
  }
  pi_est <- rep(1 / n, n)
  for (i in seq_len(max_iter)) {
    pi_nueva <- as.numeric(pi_est %*% P)
    if (max(abs(pi_nueva - pi_est)) < tol) { pi_est <- pi_nueva; break }
    pi_est <- pi_nueva
  }
  pi_est[pi_est < 0] <- 0
  pi_est / sum(pi_est)
}
costo_esperado_politica <- function(s, S, p_venta, dist_ganadora, par1, par2, K, h, p, unidad = 1) {
  if (S <= 0) S <- 1  # degenerar S=0 -> 1 estado mínimo (sin stock real disponible)
  m <- construir_matriz(s, S, p_venta, dist_ganadora, par1, par2, unidad)
  pi_est <- calcular_estacionaria(m$P)
  if (any(is.na(pi_est))) return(list(costo = NA_real_, nivel_servicio = NA_real_))
  prob_reponer <- sum(pi_est[1:(s + 1)])
  inv_efectivo <- ifelse(0:S <= s, S, 0:S)
  d_max <- S + 5; dens <- m$dens; d_vals <- 0:d_max
  demanda_insatisfecha_por_estado <- sapply(inv_efectivo, function(inv) sum(pmax(d_vals - inv, 0) * dens))
  inventario_esperado_por_estado  <- sapply(inv_efectivo, function(inv) sum(pmax(inv - d_vals, 0) * dens))
  E_quiebre_lotes <- sum(pi_est * demanda_insatisfecha_por_estado)
  E_inventario_lotes <- sum(pi_est * inventario_esperado_por_estado)
  E_quiebre_unidades <- E_quiebre_lotes * unidad
  E_inventario <- E_inventario_lotes * unidad
  costo_total <- K * prob_reponer + h * E_inventario + p * E_quiebre_unidades
  demanda_media_total <- sum(d_vals * dens)
  fill_rate <- 1 - (E_quiebre_lotes / max(demanda_media_total, 1e-6))
  list(costo = costo_total, nivel_servicio = fill_rate, prob_reponer = prob_reponer)
}

# ------------------------------------------------------------
# 8.5.1 Comparación situación actual versus política óptima
# ------------------------------------------------------------
comparacion <- politica_optima %>%
  left_join(inventario %>% dplyr::select(codigo, stock_sistema), by = c("sku" = "codigo")) %>%
  mutate(
    stock_actual_observado = replace_na(stock_sistema, 0),
    en_inventario_marzo2026 = !is.na(stock_sistema),
    s_actual = 0,           # sin proceso formal de reorden (ver 5.4 del informe base)
    S_actual = stock_actual_observado,
    unidad_actual = unidad_lote
  )

cat("=== 8.5.1 Reconstrucción de la situación actual (evidencia empírica) ===\n")
cat("SKU críticos con stock físico registrado en marzo 2026:", sum(comparacion$en_inventario_marzo2026), "de", nrow(comparacion), "\n")
cat("SKU críticos con 0 unidades físicas (ausentes del conteo => sin stock):",
    sum(!comparacion$en_inventario_marzo2026), "de", nrow(comparacion),
    " (", round(100 * mean(!comparacion$en_inventario_marzo2026), 0), "% del segmento crítico)\n", sep = "")

calc_actual <- function(fila) {
  if (fila$S_actual <= 0) {
    # Sin stock físico real (evidencia del inventario de marzo 2026): no
    # existe colchón de inventario que "amortigüe" la demanda. Se modela
    # directamente, sin pasar por la cadena de Markov (que exige S>=1 y
    # forzaría un colchón ficticio de 1 unidad, subestimando el costo real
    # de no tener stock). Toda la demanda del periodo se resuelve mediante
    # el mecanismo de costo de rotura (despacho directo parcial + venta
    # perdida), y el nivel de servicio desde stock local es 0%.
    demanda_media_mensual <- fila$p_venta * fila$demanda_media_positiva
    costo <- fila$p * demanda_media_mensual
    return(tibble(costo_actual = costo, nivel_servicio_actual = 0))
  }
  unidad_uso <- if (fila$S_actual > 300) fila$unidad_lote else 1
  S_en_unidad <- max(1, round(fila$S_actual / unidad_uso))
  res <- tryCatch(
    costo_esperado_politica(0, S_en_unidad, fila$p_venta, fila$dist_ganadora,
                             fila$par1, fila$par2, fila$K, fila$h, fila$p, unidad = unidad_uso),
    error = function(e) list(costo = NA_real_, nivel_servicio = NA_real_)
  )
  tibble(costo_actual = res$costo, nivel_servicio_actual = res$nivel_servicio)
}

comparacion <- comparacion %>%
  rowwise() %>%
  mutate(res_actual = list(calc_actual(pick(everything())))) %>%
  ungroup() %>%
  unnest(res_actual) %>%
  mutate(
    ahorro_costo_pct = (costo_actual - costo_opt) / costo_actual,
    mejora_servicio_pp = (nivel_servicio_opt - nivel_servicio_actual) * 100
  )

cat("\n=== Tabla 8.10: Comparación de desempeño (agregado, 69 SKU) ===\n")
cat("Costo esperado situación actual (suma mensual, $):", format(round(sum(comparacion$costo_actual, na.rm=TRUE)), big.mark = ","), "\n")
cat("Costo esperado política óptima (suma mensual, $): ", format(round(sum(comparacion$costo_opt, na.rm=TRUE)), big.mark = ","), "\n")
ahorro_agregado <- (sum(comparacion$costo_actual, na.rm=TRUE) - sum(comparacion$costo_opt, na.rm=TRUE)) / sum(comparacion$costo_actual, na.rm=TRUE)
cat("Ahorro agregado estimado:", round(ahorro_agregado * 100, 1), "%\n")
cat("Nivel de servicio promedio situación actual:", round(mean(comparacion$nivel_servicio_actual, na.rm=TRUE) * 100, 1), "%\n")
cat("Nivel de servicio promedio política óptima: ", round(mean(comparacion$nivel_servicio_opt, na.rm=TRUE) * 100, 1), "%\n")

write_csv(
  comparacion %>% dplyr::select(sku, descripcion, S_actual, en_inventario_marzo2026,
                          costo_actual, nivel_servicio_actual, s_opt, S_opt,
                          costo_opt, nivel_servicio_opt, ahorro_costo_pct, mejora_servicio_pp),
  file.path(TABLAS, "tabla_8_10_comparacion.csv")
)

# ------------------------------------------------------------
# 8.5.2 Análisis de sensibilidad CONJUNTA (beta x escenario de lead time)
# ------------------------------------------------------------
# Corrección respecto al diagnóstico ChatGPT (punto 6 de la bitácora): la
# sensibilidad se hace de forma CONJUNTA sobre beta y el escenario de lead
# time (no una variable a la vez), y sobre el set completo de 10 SKU de
# ejemplo (no un subconjunto arbitrario de tamaño distinto al resto del
# informe).

sku_top10 <- comparacion %>% arrange(desc(costo_opt)) %>% slice(1:10) %>% pull(sku)

recalcular_con_beta <- function(fila, beta_nuevo, factor_leadtime) {
  # El factor_leadtime escala el costo de rotura vía despacho directo
  # (aproxima el efecto de un lead time optimista/base/pesimista sobre la
  # capacidad de recuperar demanda: un lead time más corto hace más barata
  # y más viable la recuperación por despacho directo).
  p_nuevo <- beta_nuevo * (fila$costo_directo_unitario_ref * factor_leadtime) +
    (1 - beta_nuevo) * fila$costo_perdida_unitario_ref
  res <- costo_esperado_politica(fila$s_opt_idx, fila$S_opt_idx, fila$p_venta,
                                  fila$dist_ganadora, fila$par1, fila$par2,
                                  fila$K, fila$h, p_nuevo, unidad = fila$unidad_lote)
  tibble(beta = beta_nuevo, factor_leadtime = factor_leadtime,
         costo = res$costo, nivel_servicio = res$nivel_servicio)
}

parametros_costo_tbl <- read_csv(file.path(TABLAS, "tabla_8_8_costo_rotura_ajustado.csv"), show_col_types = FALSE)

parametros_costo_tbl <- read_csv(file.path(TABLAS, "tabla_8_8_costo_rotura_ajustado.csv"), show_col_types = FALSE)

base_sens <- comparacion %>%
  dplyr::filter(sku %in% sku_top10) %>%
  rename(costo_directo_unitario_ref = costo_directo_unitario,
         costo_perdida_unitario_ref = costo_perdida_unitario) %>%
  mutate(s_opt_idx = round(s_opt / unidad_lote), S_opt_idx = round(S_opt / unidad_lote))

grid_sensibilidad <- expand_grid(
  beta = c(0.15, 0.30, 0.45),
  factor_leadtime = c(0.7, 1.0, 1.5)  # optimista / base / pesimista
)

cat("\n=== 8.5.2 Análisis de sensibilidad conjunta (beta x escenario lead time) ===\n")
cat("(evaluado sobre los 10 SKU de mayor costo, misma política (s*,S*) del OE4)\n")

resultados_sensibilidad <- base_sens %>%
  rowwise() %>%
  reframe({
    fila <- pick(everything())
    bind_rows(lapply(seq_len(nrow(grid_sensibilidad)), function(k) {
      recalcular_con_beta(fila, grid_sensibilidad$beta[k], grid_sensibilidad$factor_leadtime[k]) %>%
        mutate(sku = fila$sku)
    }))
  })

resumen_sensibilidad <- resultados_sensibilidad %>%
  group_by(beta, factor_leadtime) %>%
  summarise(costo_total_10sku = sum(costo), .groups = "drop") %>%
  arrange(beta, factor_leadtime)

print(resumen_sensibilidad)
write_csv(resumen_sensibilidad, file.path(TABLAS, "tabla_8_11_8_12_sensibilidad_conjunta.csv"))

rango_costo <- range(resumen_sensibilidad$costo_total_10sku)
cat("\nRango de costo total (10 SKU) bajo todos los escenarios evaluados: $",
    format(round(rango_costo[1]), big.mark = ","), " - $",
    format(round(rango_costo[2]), big.mark = ","), "\n", sep = "")
cat("Variación relativa entre el escenario más favorable y el más adverso: ",
    round(100 * (rango_costo[2] - rango_costo[1]) / rango_costo[1], 1), "%\n", sep = "")

# ------------------------------------------------------------
# 8.5.4 / 8.5.5 Validación externa contra la literatura
# ------------------------------------------------------------
cat("\n=== 8.5.4-8.5.5 Validación externa (contraste no como garantía propia) ===\n")
cat("Ahorro agregado estimado en este proyecto: ", round(ahorro_agregado * 100, 1), "%\n", sep = "")
cat("Referencias de literatura (rangos de contexto comparable, NO garantía\n")
cat("de replicación del resultado propio -- ver Marco Teórico corregido):\n")
cat("  Chopra & Meindl / Nahmias & Olsen: 25%-40% en distribuidores industriales\n")
cat("  Silver et al.: 15%-20%\n")
cat("  Syntetos et al.: 32% / 18% según categoría de demanda\n")
cat("  Teunter et al.: hasta 87% en casos específicos de demanda muy intermitente\n")
if (ahorro_agregado >= 0.15 && ahorro_agregado <= 0.45) {
  cat("-> El resultado propio (", round(ahorro_agregado*100,1),
      "%) cae DENTRO del rango típico reportado en la literatura para\n", sep="")
  cat("   distribuidores industriales, lo que da plausibilidad al resultado\n")
  cat("   sin pretender que la cifra de la literatura valide la magnitud exacta.\n")
} else {
  cat("-> El resultado propio queda FUERA del rango típico de literatura:\n")
  cat("   se documenta como hallazgo a discutir (posible efecto de los supuestos\n")
  cat("   de costo, no necesariamente un error).\n")
}

cat("\n=== 05_OE5.R finalizado. Salidas en output/tablas/ ===\n")
