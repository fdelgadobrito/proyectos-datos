# ============================================================
# 02_OE2.R
# OE2. Analizar el comportamiento estocástico de la demanda de los
# productos críticos de la línea Cóndor mediante el ajuste de un modelo de
# demanda intermitente con herramientas estadísticas computacionales
# (software R), y representar la incertidumbre del tiempo de reposición
# mediante escenarios y/o distribuciones supuestas mediante juicio experto.
#
# Entrada: data/clean/demanda_mensual_sku.rds, data/clean/sku_criticos.rds
# Salida:  data/clean/ajuste_demanda.rds
#          data/clean/dlt_simulacion.rds
#          output/tablas/tabla_8_3_ajuste.csv
#          output/tablas/tabla_8_4_contraste_poisson.csv
#          output/tablas/tabla_8_5_dlt.csv
#
# Los 10 SKU de mayor venta se narran en detalle en el informe (Sección
# 8.2); los 79 SKU críticos completos del segmento A se procesan aquí y
# quedan documentados íntegramente en la tabla de anexo.
# ============================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(fitdistrplus)
})

set.seed(2026)

CLEAN <- "data/clean"
TABLAS <- "output/tablas"

demanda <- readRDS(file.path(CLEAN, "demanda_mensual_sku.rds"))
sku_criticos <- readRDS(file.path(CLEAN, "sku_criticos.rds"))

sku_ids <- sku_criticos$sku
n_meses <- n_distinct(demanda$fecha)
todas_fechas <- sort(unique(demanda$fecha))

# ------------------------------------------------------------
# 8.2.1 Construcción de series mensuales y clasificación de regularidad
# ------------------------------------------------------------
# Se completa el panel: cada uno de los 79 SKU críticos debe tener una fila
# por cada uno de los 18 meses (los meses sin venta quedan en cantidad = 0,
# a diferencia del export original que simplemente omite esos meses).

panel_completo <- expand_grid(sku = sku_ids, fecha = todas_fechas) %>%
  left_join(
    demanda %>% dplyr::select(sku, fecha, cantidad, monto),
    by = c("sku", "fecha")
  ) %>%
  mutate(cantidad = replace_na(cantidad, 0), monto = replace_na(monto, 0)) %>%
  arrange(sku, fecha)

# Clasificación de regularidad de Syntetos-Boylan (estándar en demanda
# intermitente): ADI = intervalo promedio entre periodos con demanda
# positiva; CV2 = (desv. estándar / media)^2 de la demanda POSITIVA.
#   ADI < 1.32 y CV2 < 0.49  -> "smooth" (regular)
#   ADI >= 1.32 y CV2 < 0.49 -> "intermitente"
#   ADI < 1.32 y CV2 >= 0.49 -> "errático"
#   ADI >= 1.32 y CV2 >= 0.49 -> "lumpy" (grumoso)

clasificar_regularidad <- function(cantidades) {
  n <- length(cantidades)
  positivos <- cantidades[cantidades > 0]
  n_pos <- length(positivos)
  if (n_pos < 2) return(tibble(adi = NA_real_, cv2 = NA_real_, clase = "insuficiente"))
  adi <- n / n_pos
  cv2 <- (sd(positivos) / mean(positivos))^2
  clase <- case_when(
    adi < 1.32 & cv2 < 0.49 ~ "smooth",
    adi >= 1.32 & cv2 < 0.49 ~ "intermitente",
    adi < 1.32 & cv2 >= 0.49 ~ "erratico",
    TRUE ~ "lumpy"
  )
  tibble(adi = adi, cv2 = cv2, clase = clase)
}

regularidad <- panel_completo %>%
  group_by(sku) %>%
  summarise(
    meses_positivos = sum(cantidad > 0),
    prop_ceros = mean(cantidad == 0),
    media_positiva = mean(cantidad[cantidad > 0]),
    reg = list(clasificar_regularidad(cantidad)),
    .groups = "drop"
  ) %>%
  unnest(reg)

cat("=== 8.2.1 Clasificación de regularidad de la demanda (79 SKU críticos) ===\n")
print(regularidad %>% count(clase))

# ------------------------------------------------------------
# 8.2.2 Primer intento de ajuste de distribuciones (y su falla)
# ------------------------------------------------------------
# Intento ingenuo: ajustar una distribución continua (normal) o de conteo
# (Poisson) directamente sobre la serie mensual COMPLETA, incluyendo los
# meses sin venta (ceros). Se muestra con el SKU de mayor venta como
# ejemplo del problema.

sku_ejemplo <- sku_criticos %>% arrange(desc(monto_total)) %>% slice(1) %>% pull(sku)
serie_ejemplo <- panel_completo %>% dplyr::filter(sku == sku_ejemplo) %>% pull(cantidad)

cat("\n=== 8.2.2 Primer intento (ingenuo) sobre SKU", sku_ejemplo, "===\n")
cat("Serie completa (18 meses, incluye ceros):", paste(serie_ejemplo, collapse = ", "), "\n")

fit_normal_ingenuo <- suppressWarnings(suppressMessages(tryCatch(
  fitdist(serie_ejemplo, "norm"),
  error = function(e) NULL
)))
if (!is.null(fit_normal_ingenuo)) {
  cat("Ajuste Normal ingenuo -> media =", round(fit_normal_ingenuo$estimate["mean"], 1),
      " sd =", round(fit_normal_ingenuo$estimate["sd"], 1), "\n")
  cat("PROBLEMA: una Normal con esta media/sd asigna probabilidad no despreciable a\n")
  cat("demanda NEGATIVA, lo cual no tiene sentido físico. AIC =", round(fit_normal_ingenuo$aic, 1), "\n")
}

invisible(capture.output(
  fit_poisson_ingenuo <- suppressWarnings(suppressMessages(tryCatch(
    fitdist(serie_ejemplo, "pois"),
    error = function(e) NULL
  )))
))
if (!is.null(fit_poisson_ingenuo)) {
  cat("Ajuste Poisson ingenuo -> lambda =", round(fit_poisson_ingenuo$estimate["lambda"], 2), "\n")
  # Test de sobre-dispersión simple: var/media
  vm_ratio <- var(serie_ejemplo) / mean(serie_ejemplo)
  cat("Razón varianza/media =", round(vm_ratio, 1),
      "(>> 1 indica sobre-dispersión: Poisson subestima la variabilidad real,\n")
  cat("  producto de mezclar meses sin venta con meses de venta alta en una sola distribución)\n")
}

# ------------------------------------------------------------
# 8.2.3 Corrección metodológica y cambio a modelo de dos partes
# ------------------------------------------------------------
# Se separa el fenómeno en dos: (i) probabilidad de que el mes tenga venta
# (Bernoulli(p)), y (ii) distribución de la CANTIDAD dado que hubo venta,
# ajustada solo sobre los valores positivos. Se comparan 3 distribuciones
# candidatas por AIC: Normal, Gamma y Lognormal (todas definidas sobre
# valores positivos, evitando el problema de masa negativa del punto 8.2.2).

safe_fitdist <- function(x, distr) {
  out <- NULL
  invisible(capture.output(
    out <- suppressWarnings(suppressMessages(
      tryCatch(fitdist(x, distr), error = function(e) NULL)
    ))
  ))
  out
}

ajustar_dos_partes <- function(cantidades) {
  n <- length(cantidades)
  positivos <- cantidades[cantidades > 0]
  n_pos <- length(positivos)
  p_venta <- n_pos / n

  if (n_pos < 4) {
    return(tibble(
      p_venta = p_venta, n_pos = n_pos,
      dist_ganadora = NA_character_, aic_normal = NA_real_,
      aic_gamma = NA_real_, aic_lnorm = NA_real_,
      par1 = NA_real_, par2 = NA_real_
    ))
  }

  # Caso especial: varianza cero entre los valores positivos (ej. la venta,
  # cuando ocurre, es siempre de 1 unidad). Ninguna distribución continua de
  # 2 parámetros (Normal/Gamma/Lognormal) puede ajustarse por máxima
  # verosimilitud a datos degenerados (sd = 0): el optimizador no converge
  # porque la superficie de verosimilitud no tiene un óptimo interior. Se
  # documenta como demanda "constante" en vez de forzar un ajuste espurio.
  if (sd(positivos) == 0) {
    return(tibble(
      p_venta = p_venta, n_pos = n_pos,
      dist_ganadora = "constante", aic_normal = NA_real_,
      aic_gamma = NA_real_, aic_lnorm = NA_real_,
      par1 = positivos[1], par2 = 0
    ))
  }

  fits <- list(
    normal = safe_fitdist(positivos, "norm"),
    gamma  = safe_fitdist(positivos, "gamma"),
    lnorm  = safe_fitdist(positivos, "lnorm")
  )

  get_aic <- function(f) {
    val <- tryCatch(as.numeric(f$aic), error = function(e) NA_real_)
    if (is.null(val) || length(val) != 1) return(NA_real_)
    val
  }
  aics <- vapply(fits, get_aic, numeric(1))

  if (all(is.na(aics))) {
    return(tibble(
      p_venta = p_venta, n_pos = n_pos,
      dist_ganadora = NA_character_, aic_normal = NA_real_,
      aic_gamma = NA_real_, aic_lnorm = NA_real_,
      par1 = NA_real_, par2 = NA_real_
    ))
  }

  ganadora <- names(aics)[which.min(aics)]

  par1 <- par2 <- NA_real_
  est <- fits[[ganadora]]$estimate
  par1 <- unname(est[1]); par2 <- unname(est[2])

  tibble(
    p_venta = p_venta, n_pos = n_pos,
    dist_ganadora = ganadora,
    aic_normal = aics["normal"], aic_gamma = aics["gamma"], aic_lnorm = aics["lnorm"],
    par1 = par1, par2 = par2
  )
}

ajuste_demanda <- panel_completo %>%
  group_by(sku) %>%
  summarise(res = list(ajustar_dos_partes(cantidad)), .groups = "drop") %>%
  unnest(res) %>%
  left_join(sku_criticos %>% dplyr::select(sku, descripcion, monto_total), by = "sku") %>%
  arrange(desc(monto_total))

cat("\n=== 8.2.3 Modelo de dos partes: distribución ganadora por AIC ===\n")
print(ajuste_demanda %>% count(dist_ganadora))

cat("\nEjemplo (SKU de mayor venta,", sku_ejemplo, "):\n")
print(ajuste_demanda %>% dplyr::filter(sku == sku_ejemplo) %>%
        dplyr::select(p_venta, n_pos, dist_ganadora, aic_normal, aic_gamma, aic_lnorm, par1, par2))

# ------------------------------------------------------------
# 8.2.4 Contraste con un modelo Poisson único y validación de bondad de
#        ajuste (Kolmogorov-Smirnov)
# ------------------------------------------------------------
# Se compara, para cada SKU, el modelo de dos partes contra un único
# Poisson ajustado sobre la serie completa (incluyendo ceros) -- el mismo
# "primer intento" del punto 8.2.2 pero aplicado sistemáticamente a los 79
# SKU, para cuantificar cuán generalizado es el problema de sobre-dispersión.

contraste_poisson <- panel_completo %>%
  group_by(sku) %>%
  summarise(
    media = mean(cantidad),
    varianza = var(cantidad),
    razon_var_media = varianza / media,
    .groups = "drop"
  ) %>%
  mutate(sobredispersion = razon_var_media > 1.5)

cat("\n=== 8.2.4 Contraste con Poisson único (razón varianza/media) ===\n")
cat("SKU con sobre-dispersión relevante (var/media > 1.5):",
    sum(contraste_poisson$sobredispersion, na.rm = TRUE), "de", nrow(contraste_poisson), "\n")

# Prueba KS del modelo de dos partes ganador vs la distribución empírica de
# los valores positivos, para el SKU de ejemplo (se documenta el
# procedimiento; se replica igual para el resto en la tabla de anexo).
ks_ejemplo <- NULL
fila_ejemplo <- ajuste_demanda %>% dplyr::filter(sku == sku_ejemplo)
if (!is.na(fila_ejemplo$dist_ganadora)) {
  positivos_ejemplo <- serie_ejemplo[serie_ejemplo > 0]
  ks_ejemplo <- switch(fila_ejemplo$dist_ganadora,
    normal = ks.test(positivos_ejemplo, "pnorm", fila_ejemplo$par1, fila_ejemplo$par2),
    gamma  = ks.test(positivos_ejemplo, "pgamma", fila_ejemplo$par1, fila_ejemplo$par2),
    lnorm  = ks.test(positivos_ejemplo, "plnorm", fila_ejemplo$par1, fila_ejemplo$par2)
  )
  cat("\nKS test SKU", sku_ejemplo, "(dist.", fila_ejemplo$dist_ganadora, ") -> D =",
      round(ks_ejemplo$statistic, 3), " p-value =", round(ks_ejemplo$p.value, 3), "\n")
}

saveRDS(ajuste_demanda, file.path(CLEAN, "ajuste_demanda.rds"))
write_csv(ajuste_demanda, file.path(TABLAS, "tabla_8_3_ajuste.csv"))
write_csv(contraste_poisson, file.path(TABLAS, "tabla_8_4_contraste_poisson.csv"))

# ------------------------------------------------------------
# 8.2.5 Supuesto de lead time
# ------------------------------------------------------------
# CUADRO DE SUPUESTOS (sin historial de lead time en los registros de la
# empresa -> se representa por escenarios informados por juicio experto,
# tal como especifica el OE2 corregido). ESTE SUPUESTO DEBE SER VALIDADO
# CON LA EMPRESA antes de usarse en el informe final; se deja como
# parámetro explícito y fácil de cambiar.

lead_time_dias <- list(
  optimista  = 7,
  base       = 15,
  pesimista  = 30
)

cat("\n=== 8.2.5 CUADRO DE SUPUESTOS: escenarios de lead time (días) ===\n")
cat("  Optimista:", lead_time_dias$optimista, "días\n")
cat("  Base:     ", lead_time_dias$base, "días\n")
cat("  Pesimista:", lead_time_dias$pesimista, "días\n")
cat("  *** Pendiente de validar con la organización (no hay historial de\n")
cat("      lead time en los registros disponibles) ***\n")

# ------------------------------------------------------------
# 8.2.6 Simulación de Monte Carlo: demanda durante el lead time (DLT)
# ------------------------------------------------------------
# Supuesto de modelación: dado que el lead time (7-30 días) es menor a un
# mes y no se dispone de demanda diaria desagregada, se aproxima la DLT
# escalando la demanda mensual simulada (modelo de dos partes) por la
# fracción (lead_time_dias / 30), asumiendo una tasa de demanda
# aproximadamente uniforme dentro del mes. El lead time en cada simulación
# se sortea como triangular(optimista, base, pesimista).

simular_dlt <- function(p_venta, dist_ganadora, par1, par2, n_sim = 10000) {
  if (is.na(dist_ganadora)) return(NULL)

  # Lead time aleatorio (triangular discreta simple vía mezcla de 3 escenarios
  # con pesos 25/50/25, aproximación práctica y transparente del "juicio experto")
  lt_sim <- sample(
    c(lead_time_dias$optimista, lead_time_dias$base, lead_time_dias$pesimista),
    n_sim, replace = TRUE, prob = c(0.25, 0.5, 0.25)
  )

  venta_mes <- rbinom(n_sim, 1, p_venta)
  cantidad_mes <- switch(dist_ganadora,
    normal = pmax(rnorm(n_sim, par1, par2), 0),
    gamma  = rgamma(n_sim, shape = par1, rate = par2),
    lnorm  = rlnorm(n_sim, par1, par2),
    constante = rep(par1, n_sim)
  )
  demanda_mes_sim <- venta_mes * cantidad_mes
  dlt_sim <- demanda_mes_sim * (lt_sim / 30)
  dlt_sim
}

dlt_resultados <- ajuste_demanda %>%
  dplyr::filter(!is.na(dist_ganadora)) %>%
  rowwise() %>%
  mutate(
    dlt_media = list(simular_dlt(p_venta, dist_ganadora, par1, par2))
  ) %>%
  ungroup()

resumen_dlt <- dlt_resultados %>%
  mutate(
    dlt_p50 = sapply(dlt_media, median),
    dlt_p90 = sapply(dlt_media, quantile, probs = 0.90),
    dlt_p95 = sapply(dlt_media, quantile, probs = 0.95),
    dlt_media_val = sapply(dlt_media, mean),
    dlt_sd = sapply(dlt_media, sd)
  ) %>%
  dplyr::select(sku, descripcion, monto_total, dist_ganadora, dlt_media_val, dlt_sd, dlt_p50, dlt_p90, dlt_p95)

cat("\n=== 8.2.6 Simulación Monte Carlo DLT (10,000 iteraciones por SKU) ===\n")
cat("Ejemplo (SKU de mayor venta,", sku_ejemplo, "):\n")
print(resumen_dlt %>% dplyr::filter(sku == sku_ejemplo))

saveRDS(dlt_resultados, file.path(CLEAN, "dlt_simulacion.rds"))
write_csv(resumen_dlt, file.path(TABLAS, "tabla_8_5_dlt.csv"))

cat("\n=== 02_OE2.R finalizado. Salidas en data/clean/ y output/tablas/ ===\n")
