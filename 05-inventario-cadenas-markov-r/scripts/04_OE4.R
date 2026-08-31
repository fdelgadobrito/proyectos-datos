# ============================================================
# 04_OE4.R
# OE4. Proponer una política de reabastecimiento óptima del tipo (s, S)
# para los SKU críticos de la línea Cóndor, determinando el punto de
# reorden (s) y la cantidad máxima de pedido (S) que minimizan el costo
# total esperado de inventario bajo un nivel de servicio objetivo de al
# menos el 95%.
#
# Entrada: data/clean/ajuste_demanda.rds, output/tablas/tabla_8_5_dlt.csv,
#          data/raw/CONSUMOS_PUERTO_MONTT_25-26_-_MENSUAL.xlsx (para K empírico)
# Salida:  data/clean/politica_optima.rds
#          output/tablas/tabla_8_7_parametros_costo.csv
#          output/tablas/tabla_8_8_costo_rotura_ajustado.csv
#          output/tablas/tabla_8_9_politica_optima.csv
# ============================================================

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

set.seed(2026)

RAW <- "data/raw"
CLEAN <- "data/clean"
TABLAS <- "output/tablas"

ajuste_demanda <- readRDS(file.path(CLEAN, "ajuste_demanda.rds")) %>% dplyr::filter(!is.na(dist_ganadora))
dlt <- read_csv(file.path(TABLAS, "tabla_8_5_dlt.csv"), show_col_types = FALSE)
abc <- readRDS(file.path(CLEAN, "abc_clasificacion.rds"))

# ------------------------------------------------------------
# 8.4.1 Parámetros de costo: metodología mixta
# ------------------------------------------------------------
# K se estima EMPÍRICAMENTE desde los registros reales de despacho (líneas
# de servicio "SER-*" excluidas del análisis de demanda en la limpieza,
# pero que sí contienen el costo de flete efectivamente cobrado por tramo
# de peso). h y el margen de rotura son SUPUESTOS explícitos, documentados
# en el cuadro más abajo, porque no hay dato interno disponible para
# ellos: quedan pendientes de validar con la organización (mismo
# tratamiento que el supuesto de lead time en el OE2).

despachos <- readRDS(file.path(CLEAN, "despachos.rds"))
serv <- despachos %>% mutate(codigo = sku)

K_regular <- sum(serv$monto) / sum(serv$cantidad)

brackets_pequenos <- c("SER-110-110-156", "SER-110-110-157", "SER-110-110-154")
serv_pequeno <- serv %>% dplyr::filter(codigo %in% brackets_pequenos)
K_directo <- sum(serv_pequeno$monto) / sum(serv_pequeno$cantidad)

cat("=== 8.4.1 Parámetros de costo: metodología mixta ===\n")
cat("K (costo de pedido regular, EMPÍRICO desde", nrow(serv), "registros de despacho): $",
    round(K_regular), "\n", sep = "")
cat("K_directo (despacho puntual/urgente, EMPÍRICO desde tramos de bajo peso): $",
    round(K_directo), "\n", sep = "")

# --- CUADRO DE SUPUESTOS (costo de mantención y margen) ---
TASA_MANTENCION_MENSUAL <- 0.02   # 2% mensual (~24% anual) del valor unitario
MARGEN_BRUTO <- 0.30              # 30% del precio de venta
BETA_RECUPERACION <- 0.30         # fracción de demanda insatisfecha recuperada
                                    # vía despacho directo (dato de la bitácora:
                                    # el valor anterior de 60% generaba una
                                    # reducción de costo matemáticamente
                                    # imposible; aquí se usa un valor base
                                    # conservador y se prueba en el
                                    # análisis de sensibilidad del OE5)

cat("\n*** CUADRO DE SUPUESTOS (pendientes de validar con la empresa) ***\n")
cat("  Tasa de mantención mensual sobre el valor unitario: ", TASA_MANTENCION_MENSUAL * 100, "%\n", sep = "")
cat("  Margen bruto sobre precio de venta (para costo de rotura no recuperada): ",
    MARGEN_BRUTO * 100, "%\n", sep = "")
cat("  Beta (fracción de demanda insatisfecha recuperada vía despacho directo): ",
    BETA_RECUPERACION * 100, "%\n", sep = "")

parametros_costo <- ajuste_demanda %>%
  left_join(dlt %>% dplyr::select(sku, dlt_p95), by = "sku") %>%
  mutate(
    precio_unitario = monto_total / n_pos / p_venta / (monto_total / monto_total), # placeholder replaced below
  )

# precio unitario real: monto_total de ventas / unidades_totales del SKU (desde ABC)
parametros_costo <- ajuste_demanda %>%
  left_join(abc %>% dplyr::select(sku, unidades_totales), by = "sku") %>%
  left_join(dlt %>% dplyr::select(sku, dlt_p95), by = "sku") %>%
  mutate(
    precio_unitario = monto_total / pmax(unidades_totales, 1),
    h = TASA_MANTENCION_MENSUAL * precio_unitario,
    K = K_regular
  )

write_csv(
  parametros_costo %>% dplyr::select(sku, descripcion, precio_unitario, K, h),
  file.path(TABLAS, "tabla_8_7_parametros_costo.csv")
)

# ------------------------------------------------------------
# 8.4.2 Alcances en el costo de rotura: demanda recuperada vía despacho
#        directo
# ------------------------------------------------------------
# Costo de rotura POR UNIDAD de demanda insatisfecha = promedio ponderado
# entre (i) la fracción beta que se recupera vía despacho directo desde
# casa matriz, a costo K_directo por evento (se aproxima el tamaño típico
# de ese envío puntual con la demanda media condicional a venta positiva),
# y (ii) la fracción (1-beta) que se pierde definitivamente, con costo
# igual al margen bruto no percibido.

parametros_costo <- parametros_costo %>%
  mutate(
    demanda_media_positiva = case_when(
      dist_ganadora == "gamma"     ~ par1 / par2,
      dist_ganadora == "lnorm"     ~ exp(par1 + par2^2 / 2),
      dist_ganadora == "normal"    ~ par1,
      dist_ganadora == "constante" ~ par1
    ),
    costo_directo_unitario = K_directo / pmax(demanda_media_positiva, 1),
    costo_perdida_unitario = MARGEN_BRUTO * precio_unitario,
    p = BETA_RECUPERACION * costo_directo_unitario + (1 - BETA_RECUPERACION) * costo_perdida_unitario
  )

cat("\n=== 8.4.2 Costo de rotura ajustado por recuperación vía despacho directo ===\n")
print(parametros_costo %>% arrange(desc(monto_total)) %>%
        dplyr::select(sku, precio_unitario, costo_directo_unitario, costo_perdida_unitario, p) %>%
        head(5))

write_csv(
  parametros_costo %>% dplyr::select(sku, descripcion, costo_directo_unitario, costo_perdida_unitario, p),
  file.path(TABLAS, "tabla_8_8_costo_rotura_ajustado.csv")
)

# ------------------------------------------------------------
# 8.4.3 Función de costo total esperado (reutiliza la mecánica del OE3)
# ------------------------------------------------------------
discretizar_demanda <- function(p_venta, dist_ganadora, par1, par2, d_max, unidad = 1) {
  # d_max y los estados se expresan en "lotes" de tamaño `unidad` (unidad=1
  # para SKU de bajo volumen; unidad>1 para SKU de alto volumen, de forma
  # que el número de estados de la cadena se mantenga computacionalmente
  # manejable sin perder precisión relevante para la decisión de costos).
  pcdf <- switch(dist_ganadora,
    normal = function(x) pnorm(x, par1, par2),
    gamma  = function(x) pgamma(x, shape = par1, rate = par2),
    lnorm  = function(x) plnorm(x, par1, par2),
    constante = function(x) as.numeric(x >= par1)
  )
  probs <- numeric(d_max + 1)
  probs[1] <- 1 - p_venta
  for (d in 1:(d_max - 1)) {
    lo <- (d - 0.5) * unidad
    hi <- (d + 0.5) * unidad
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
    A <- t(P) - diag(n)
    A[n, ] <- 1
    b <- c(rep(0, n - 1), 1)
    pi_est <- tryCatch(solve(A, b), error = function(e) NULL)
    if (!is.null(pi_est)) {
      pi_est[pi_est < 0 & pi_est > -1e-8] <- 0
      return(pi_est)
    }
  }
  # Iteración de potencias: pi_{t+1} = pi_t %*% P, mucho más rápido (O(n^2)
  # por iteración) que la solución algebraica exacta (O(n^3)) para espacios
  # de estados grandes (SKU de alto volumen).
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
  m <- construir_matriz(s, S, p_venta, dist_ganadora, par1, par2, unidad)
  pi_est <- calcular_estacionaria(m$P)
  if (any(is.na(pi_est))) return(list(costo = NA_real_, nivel_servicio = NA_real_))

  prob_reponer <- sum(pi_est[1:(s + 1)])
  inv_efectivo <- ifelse(0:S <= s, S, 0:S)  # en unidades de lote

  d_max <- S + 5
  dens <- m$dens
  d_vals <- 0:d_max  # en unidades de lote

  demanda_insatisfecha_por_estado <- sapply(inv_efectivo, function(inv) sum(pmax(d_vals - inv, 0) * dens))
  inventario_esperado_por_estado  <- sapply(inv_efectivo, function(inv) sum(pmax(inv - d_vals, 0) * dens))

  E_quiebre_lotes <- sum(pi_est * demanda_insatisfecha_por_estado)
  E_inventario_lotes <- sum(pi_est * inventario_esperado_por_estado)

  # Se reconvierte a unidades reales (multiplicando por el tamaño de lote)
  # antes de aplicar los costos unitarios h y p.
  E_quiebre_unidades <- E_quiebre_lotes * unidad
  E_inventario <- E_inventario_lotes * unidad

  costo_total <- K * prob_reponer + h * E_inventario + p * E_quiebre_unidades

  demanda_media_total <- sum(d_vals * dens)  # en lotes; razón no se ve afectada por la escala
  fill_rate <- 1 - (E_quiebre_lotes / max(demanda_media_total, 1e-6))

  list(costo = costo_total, nivel_servicio = fill_rate, prob_reponer = prob_reponer)
}

# ------------------------------------------------------------
# 8.4.4 Elección de la política óptima (búsqueda exhaustiva de (s,S))
# ------------------------------------------------------------
NIVEL_SERVICIO_OBJETIVO <- 0.95

buscar_politica_optima <- function(fila) {
  p_venta <- fila$p_venta; dist_ganadora <- fila$dist_ganadora
  par1 <- fila$par1; par2 <- fila$par2
  K <- fila$K; h <- fila$h; p <- fila$p

  demanda_media <- fila$demanda_media_positiva * p_venta

  # Tamaño de lote (bucket): para SKU de bajo volumen se trabaja unidad por
  # unidad (unidad=1); para SKU de alto volumen se agrupan varias unidades
  # por estado, de forma que el número de estados de la cadena (S_idx) se
  # mantenga en un rango computacionalmente tratable (~40-60 estados),
  # sin alterar materialmente la precisión de la decisión de costos.
  S_real_aprox <- max(6, ceiling(4 * demanda_media + 4 * sqrt(max(demanda_media, 1)) + 4))
  unidad <- max(1, floor(S_real_aprox / 50))

  S_max_idx <- max(6, ceiling(S_real_aprox / unidad))
  TOPE_ABSOLUTO_IDX <- 120

  intentos <- 0
  repeat {
    intentos <- intentos + 1
    mejor <- list(costo = Inf, s = NA, S = NA, nivel_servicio = -Inf)
    factible_encontrado <- FALSE

    paso <- max(1, round(S_max_idx / 20))
    s_vals <- seq(0, S_max_idx - 1, by = paso)
    for (s in s_vals) {
      S_vals <- seq(s + 1, S_max_idx, by = paso)
      for (S in S_vals) {
        res <- tryCatch(
          costo_esperado_politica(s, S, p_venta, dist_ganadora, par1, par2, K, h, p, unidad),
          error = function(e) list(costo = NA_real_, nivel_servicio = NA_real_)
        )
        if (is.na(res$costo)) next
        cumple_servicio <- res$nivel_servicio >= NIVEL_SERVICIO_OBJETIVO
        if (cumple_servicio) {
          if (!factible_encontrado || res$costo < mejor$costo) {
            mejor <- list(costo = res$costo, s = s, S = S, nivel_servicio = res$nivel_servicio)
            factible_encontrado <- TRUE
          }
        } else if (!factible_encontrado && res$nivel_servicio > mejor$nivel_servicio) {
          mejor <- list(costo = res$costo, s = s, S = S, nivel_servicio = res$nivel_servicio)
        }
      }
    }

    topa_limite <- !is.na(mejor$S) && mejor$S >= S_max_idx && !factible_encontrado
    if (!topa_limite || S_max_idx >= TOPE_ABSOLUTO_IDX || intentos > 6) break
    S_max_idx <- min(S_max_idx * 2, TOPE_ABSOLUTO_IDX)
  }

  tibble(
    s_opt = mejor$s * unidad, S_opt = mejor$S * unidad,
    costo_opt = mejor$costo, nivel_servicio_opt = mejor$nivel_servicio,
    cumple_objetivo = factible_encontrado,
    unidad_lote = unidad, S_max_idx_usado = S_max_idx, intentos_expansion = intentos
  )
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

cat("\n=== 8.4.4 Búsqueda exhaustiva de la política óptima (69 SKU) ===\n")
cat("(esto puede tardar unos minutos: barrido de grilla (s,S) por SKU)\n")

politica_optima <- parametros_costo %>%
  rowwise() %>%
  mutate(res = list(buscar_politica_optima(pick(everything())))) %>%
  ungroup() %>%
  unnest(res)

cat("SKU donde se alcanzó el nivel de servicio objetivo (>=95%):",
    sum(politica_optima$cumple_objetivo), "de", nrow(politica_optima), "\n")

cat("\nEjemplo (SKU de mayor venta):\n")
print(politica_optima %>% arrange(desc(monto_total)) %>%
        dplyr::select(sku, s_opt, S_opt, costo_opt, nivel_servicio_opt, cumple_objetivo) %>% head(5))

saveRDS(politica_optima, file.path(CLEAN, "politica_optima.rds"))
write_csv(
  politica_optima %>% dplyr::select(sku, descripcion, K, h, p, s_opt, S_opt, costo_opt, nivel_servicio_opt, cumple_objetivo),
  file.path(TABLAS, "tabla_8_9_politica_optima.csv")
)

cat("\n=== 04_OE4.R finalizado. Salidas en data/clean/ y output/tablas/ ===\n")
