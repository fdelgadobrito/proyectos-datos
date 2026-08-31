import sys
sys.path.insert(0, "/home/claude/proyecto_R/scripts")
from make_code_shot import make_shot

OUT = "/home/claude/proyecto_R/output/figuras"

figs = []

# ---------- Figura 8.1 ----------
figs.append(("fig_8_1_extraccion.png", "00_limpieza.R", """desplanar_jerarquia <- function(path, hoja = 1) {
  raw <- read_excel(path, sheet = hoja, col_names = FALSE)
  names(raw) <- c("etiqueta", "monto", "cantidad")
  periodo_actual <- NA_character_
  for (i in seq_len(nrow(raw))) {
    etiqueta <- raw$etiqueta[i]
    if (is.na(etiqueta)) next
    indent <- nchar(etiqueta) - nchar(str_trim(etiqueta, side = "left"))
    texto  <- str_trim(etiqueta)
    if (texto %in% c("Total", "Subtotal")) next
    if (str_detect(texto, "^PUERTO MONTT$")) next
    # Nivel de periodo: indentacion baja, no empieza con "["
    if (indent <= 6 && !str_starts(texto, "\\\\[")) {
      periodo_actual <- texto
      next
    }
    # Nivel SKU: "[codigo] descripcion"
    if (str_starts(texto, "\\\\[")) {
      codigo <- str_match(texto, "^\\\\[([^\\\\]]+)\\\\]")[, 2]
      descripcion <- str_trim(str_remove(texto, "^\\\\[[^\\\\]]+\\\\]\\\\s*"))
      filas[[i]] <- tibble(periodo = periodo_actual, sku = codigo,
                            descripcion = descripcion,
                            monto = as.numeric(raw$monto[i]),
                            cantidad = as.numeric(raw$cantidad[i]))
    }
  }
}"""))

# ---------- Figura 8.2 ----------
figs.append(("fig_8_2_exclusiones.png", "00_limpieza.R", """# (a)+(b) Exclusion de lineas de servicio (SER-*, SERV-*)
es_servicio <- function(sku) str_starts(sku, "SER-") | str_starts(sku, "SERV-")
excl_servicio <- mensual_raw %>% filter(es_servicio(sku))
log_msg("Exclusion de lineas de servicio: ", nrow(excl_servicio), " registros.")
mensual_prod <- mensual_raw %>% filter(!es_servicio(sku))

# (c) Registros sin marca identificable -> revision manual
mensual_prod <- mensual_prod %>%
  mutate(marca = case_when(
    str_detect(descripcion, regex("GORILA", ignore_case = TRUE)) ~ "GORILA",
    str_detect(descripcion, regex("DIFEMAT", ignore_case = TRUE)) ~ "DIFEMAT",
    str_starts(sku, "DIF-") ~ "DIFEMAT",
    TRUE ~ NA_character_
  ))
sin_marca <- mensual_prod %>% filter(is.na(marca))
log_msg("Registros sin marca identificable: ", nrow(sin_marca))
# Regla de negocio: se reclasifican como GORILA (no se descartan)
mensual_prod <- mensual_prod %>% mutate(marca = if_else(is.na(marca), "GORILA", marca))

>> Resultado real:
>> Exclusion de lineas de servicio (SER-*, SERV-*): 113 registros.
>> Registros sin marca identificable: 3  (SKU 820-200-050-000)"""))

# ---------- Figura 8.3 ----------
figs.append(("fig_8_3_validacion.png", "00_limpieza.R", """n_nulos <- sum(is.na(mensual_prod$monto) | is.na(mensual_prod$cantidad))
log_msg("Valores nulos en monto/cantidad: ", n_nulos)

dup_exactos <- mensual_prod %>%
  count(periodo, sku, monto, cantidad) %>% filter(n > 1)
log_msg("Duplicados exactos: ", sum(dup_exactos$n - 1))

# Variantes de sufijo (-I/-P/-C) del mismo producto fisico: se agregan por suma
mensual_prod <- mensual_prod %>% mutate(sku_base = str_remove(sku, "-(I|P|C)$"))

nombre_inconsistente <- mensual_prod %>%
  distinct(sku_base, descripcion) %>%
  count(sku_base) %>% filter(n > 1)
log_msg("SKU con mas de una descripcion distinta: ", nrow(nombre_inconsistente))

>> Resultado real:
>> Valores nulos en monto/cantidad: 0
>> Duplicados exactos: 0
>> SKU-mes con variantes de sufijo (se agregan): 29
>> SKU con mas de una descripcion distinta registrada: 16"""))

# ---------- Figura 8.4 ----------
figs.append(("fig_8_4_abc.png", "01_OE1.R", """resumen_sku <- demanda %>%
  group_by(sku, marca) %>%
  summarise(descripcion = first(descripcion),
            monto_total = sum(monto, na.rm = TRUE),
            unidades_totales = sum(cantidad, na.rm = TRUE),
            meses_con_venta = n_distinct(fecha[cantidad > 0]),
            .groups = "drop") %>%
  arrange(desc(monto_total)) %>%
  mutate(monto_acum = cumsum(monto_total),
         pct_acum = monto_acum / sum(monto_total),
         categoria_abc = case_when(
           pct_acum <= 0.80 ~ "A",
           pct_acum <= 0.95 ~ "B",
           TRUE ~ "C"))

MIN_MESES_CON_VENTA <- 4  # corregido desde 3 (ver bitacora de correcciones)
sku_criticos <- resumen_sku %>%
  filter(marca == "GORILA", categoria_abc == "A") %>%
  filter(meses_con_venta >= MIN_MESES_CON_VENTA)

>> Resultado real: 126 SKU Gorila categoria A -> 69 SKU criticos tras el
>> filtro de calidad de datos (57 excluidos por < 4 meses con venta)"""))

# ---------- Figura 8.5 ----------
figs.append(("fig_8_5_regularidad.png", "02_OE2.R", """clasificar_regularidad <- function(cantidades) {
  n <- length(cantidades)
  positivos <- cantidades[cantidades > 0]
  n_pos <- length(positivos)
  if (n_pos < 2) return(tibble(adi = NA, cv2 = NA, clase = "insuficiente"))
  adi <- n / n_pos                                   # intervalo entre ventas
  cv2 <- (sd(positivos) / mean(positivos))^2         # variabilidad relativa
  clase <- case_when(
    adi < 1.32 & cv2 < 0.49 ~ "smooth",
    adi >= 1.32 & cv2 < 0.49 ~ "intermitente",
    adi < 1.32 & cv2 >= 0.49 ~ "erratico",
    TRUE ~ "lumpy")
  tibble(adi = adi, cv2 = cv2, clase = clase)
}

>> Resultado real (69 SKU criticos):
>> lumpy: 26   intermitente: 27   erratico: 12   smooth: 4
>> (ninguna clase de demanda es "regular" en la mayoria del segmento)"""))

# ---------- Figura 8.6 ----------
figs.append(("fig_8_6_primer_intento.png", "02_OE2.R", """# PRIMER INTENTO (ingenuo): ajustar Normal/Poisson sobre la serie COMPLETA
serie_ejemplo  # 0, 20, 0, 14, 30, 34, 1, 20, 8, 7, 0, 0, 0, 42, 8, 12, 8, 0

fit_normal_ingenuo <- fitdist(serie_ejemplo, "norm")
# media = 11.3   sd = 12.7
# PROBLEMA: esta Normal asigna probabilidad no despreciable a demanda
# NEGATIVA, lo cual no tiene sentido fisico.

fit_poisson_ingenuo <- fitdist(serie_ejemplo, "pois")
# lambda = 11.33
vm_ratio <- var(serie_ejemplo) / mean(serie_ejemplo)
# razon varianza/media = 15  (>>1 = sobre-dispersion severa)

>> La sobre-dispersion se repite en 75 de 79 SKU evaluados: mezclar meses
>> sin venta con meses de venta alta en una sola distribucion no funciona."""))

# ---------- Figura 8.7 ----------
figs.append(("fig_8_7_dos_partes.png", "02_OE2.R", """ajustar_dos_partes <- function(cantidades) {
  positivos <- cantidades[cantidades > 0]
  p_venta <- length(positivos) / length(cantidades)

  if (sd(positivos) == 0) {
    # Caso especial: demanda constante (siempre 1 unidad). Ninguna
    # distribucion continua puede ajustarse por MLE a datos degenerados.
    return(tibble(dist_ganadora = "constante", par1 = positivos[1], par2 = 0))
  }

  fits <- list(normal = safe_fitdist(positivos, "norm"),
               gamma  = safe_fitdist(positivos, "gamma"),
               lnorm  = safe_fitdist(positivos, "lnorm"))
  aics <- vapply(fits, get_aic, numeric(1))
  ganadora <- names(aics)[which.min(aics)]
  # ... se guardan los parametros de la distribucion ganadora (par1, par2)
}

>> Resultado real (69 SKU): lognormal 40 | gamma 21 | normal 6 | constante 2"""))

# ---------- Figura 8.8 ----------
figs.append(("fig_8_8_contraste.png", "02_OE2.R", """contraste_poisson <- panel_completo %>%
  group_by(sku) %>%
  summarise(media = mean(cantidad), varianza = var(cantidad),
            razon_var_media = varianza / media, .groups = "drop") %>%
  mutate(sobredispersion = razon_var_media > 1.5)

# Prueba de bondad de ajuste Kolmogorov-Smirnov del modelo ganador
ks_ejemplo <- ks.test(positivos_ejemplo, "pgamma", par1, par2)

>> Resultado real:
>> SKU con sobre-dispersion relevante (var/media > 1.5): 66 de 69
>> KS test SKU de mayor venta (dist. gamma) -> D = 0.155  p-value = 0.935
>> (p-value alto = no se rechaza el ajuste; la gamma es consistente
>>  con los datos observados)"""))

# ---------- Figura 8.9 ----------
figs.append(("fig_8_9_montecarlo_dlt.png", "02_OE2.R", """# CUADRO DE SUPUESTOS: escenarios de lead time (dias) -- sin historial
# interno, se representan por juicio experto (pendiente de validar)
lead_time_dias <- list(optimista = 7, base = 15, pesimista = 30)

simular_dlt <- function(p_venta, dist_ganadora, par1, par2, n_sim = 10000) {
  lt_sim <- sample(c(lead_time_dias$optimista, lead_time_dias$base,
                      lead_time_dias$pesimista),
                    n_sim, replace = TRUE, prob = c(0.25, 0.5, 0.25))
  venta_mes <- rbinom(n_sim, 1, p_venta)
  cantidad_mes <- rgamma(n_sim, shape = par1, rate = par2)  # (u otra dist.)
  demanda_mes_sim <- venta_mes * cantidad_mes
  dlt_sim <- demanda_mes_sim * (lt_sim / 30)   # escalado al lead time
  dlt_sim
}

>> Se simulan 10,000 iteraciones por SKU para obtener P50/P90/P95 de la DLT"""))

# ---------- Figura 8.10 ----------
figs.append(("fig_8_10_discretizacion.png", "03_OE3.R", """discretizar_demanda <- function(p_venta, dist_ganadora, par1, par2, d_max) {
  pcdf <- switch(dist_ganadora,
    normal = function(x) pnorm(x, par1, par2),
    gamma  = function(x) pgamma(x, shape = par1, rate = par2),
    lnorm  = function(x) plnorm(x, par1, par2),
    constante = function(x) as.numeric(x >= par1))
  probs <- numeric(d_max + 1)
  probs[1] <- 1 - p_venta                      # P(D=0): mes sin venta
  for (d in 1:(d_max - 1)) {
    probs[d + 1] <- p_venta * (pcdf(d + 0.5) - pcdf(d - 0.5))  # P(D=d)
  }
  probs[d_max + 1] <- max(0, 1 - sum(probs[1:d_max]))  # cola agrupada
  probs / sum(probs)
}

>> El espacio de estados {0,...,S} representa el nivel de inventario a
>> inicio de cada periodo (1 mes, igual granularidad que la demanda)"""))

# ---------- Figura 8.11 ----------
figs.append(("fig_8_11_primer_intento_markov.png", "03_OE3.R", """# PRIMER INTENTO: matriz SIN regla de reposicion
construir_matriz(s, S, ..., version = "incorrecta")
#   inv_inicio <- i     # nunca se repone, cualquiera sea el estado i

P_incorrecta <- construir_matriz(s_demo, S_demo, ..., version = "incorrecta")
P_incorrecta[1, 1]
>> [1] 1

>> ERROR DE DISEÑO: el estado 0 es ABSORBENTE (P[0,0] = 1). La cadena
>> predice que, tarde o temprano, el sistema se queda en 0 para siempre.
>> Esto contradice los datos reales: la sucursal SI sigue vendiendo mes
>> a mes. Faltaba incorporar la logica de reposicion (s, S)."""))

# ---------- Figura 8.12 ----------
figs.append(("fig_8_12_correccion_validacion.png", "03_OE3.R", """# CORRECCION: se incorpora la regla de reposicion
for (i in 0:S) {
  if (version == "correcta" && i <= s) {
    inv_inicio <- S   # se repone a S (lead time < periodo de revision)
  } else {
    inv_inicio <- i
  }
  # ... se distribuye la probabilidad de demanda sobre inv_inicio
}

P_correcta[1, 1]
>> [1] 0.0708      # ya no es absorbente

# Validacion cruzada: cadena analitica vs 5,000 periodos de Monte Carlo
>> P(quiebre) analitico:  0.0846
>> P(quiebre) simulado:   0.0828   (diferencia: 0.0018)
>> Sobre los 69 SKU: diferencia promedio 0.71%, maxima 3.65%"""))

# ---------- Figura 8.13 ----------
figs.append(("fig_8_13_estacionaria.png", "03_OE3.R", """calcular_estacionaria <- function(P, tol = 1e-10, max_iter = 2000) {
  n <- nrow(P)
  if (n <= 40) {
    # Solucion algebraica exacta: pi %*% P = pi, sum(pi) = 1
    A <- t(P) - diag(n)
    A[n, ] <- 1
    b <- c(rep(0, n - 1), 1)
    return(solve(A, b))
  }
  # Iteracion de potencias (mas eficiente para SKU de alto volumen,
  # donde el espacio de estados puede superar 40-50 estados)
  pi_est <- rep(1 / n, n)
  repeat {
    pi_nueva <- as.numeric(pi_est %*% P)
    if (max(abs(pi_nueva - pi_est)) < tol) break
    pi_est <- pi_nueva
  }
  pi_est / sum(pi_est)
}"""))

# ---------- Figura 8.14 ----------
figs.append(("fig_8_14_costo_K_empirico.png", "04_OE4.R", """# K se estima EMPIRICAMENTE desde los registros reales de despacho
# (lineas "SER-*" excluidas de la demanda, pero con costo real de flete)
serv <- raw_serv %>% filter(str_starts(texto, "\\\\[SER-")) %>%
  mutate(monto = as.numeric(monto), cantidad = as.numeric(cantidad))

K_regular <- sum(serv$monto) / sum(serv$cantidad)

brackets_pequenos <- c("SER-110-110-156","SER-110-110-157","SER-110-110-154")
K_directo <- sum(serv$monto[serv$codigo %in% brackets_pequenos]) /
             sum(serv$cantidad[serv$codigo %in% brackets_pequenos])

>> K (costo de pedido regular, empirico, 112 despachos):  $10,910
>> K_directo (despacho puntual/urgente, tramos pequenos): $7,015"""))

# ---------- Figura 8.15 ----------
figs.append(("fig_8_15_costo_rotura_ajustado.png", "04_OE4.R", """# CUADRO DE SUPUESTOS (pendientes de validar con la empresa)
TASA_MANTENCION_MENSUAL <- 0.02   # 2% mensual sobre el valor unitario
MARGEN_BRUTO <- 0.30              # 30% del precio de venta
BETA_RECUPERACION <- 0.30         # fraccion recuperada via despacho directo
                                   # (antes 60%: generaba una reduccion de
                                   #  costo matematicamente imposible)

parametros_costo <- parametros_costo %>%
  mutate(
    costo_directo_unitario = K_directo / demanda_media_positiva,
    costo_perdida_unitario = MARGEN_BRUTO * precio_unitario,
    p = BETA_RECUPERACION * costo_directo_unitario +
        (1 - BETA_RECUPERACION) * costo_perdida_unitario
  )"""))

# ---------- Figura 8.16 ----------
figs.append(("fig_8_16_busqueda_optima.png", "04_OE4.R", """NIVEL_SERVICIO_OBJETIVO <- 0.95

for (s in s_vals) {
  for (S in S_vals) {
    res <- costo_esperado_politica(s, S, p_venta, dist_ganadora,
                                    par1, par2, K, h, p, unidad)
    cumple_servicio <- res$nivel_servicio >= NIVEL_SERVICIO_OBJETIVO
    if (cumple_servicio && res$costo < mejor$costo) {
      mejor <- list(costo = res$costo, s = s, S = S,
                     nivel_servicio = res$nivel_servicio)
    }
  }
}
# Si el optimo topa el limite superior de la grilla sin cumplir el
# objetivo, se duplica el rango de busqueda y se reintenta (adaptativo)

>> Resultado real: 67 de 69 SKU alcanzan el nivel de servicio objetivo (95%)"""))

# ---------- Figura 8.17 ----------
figs.append(("fig_8_17_comparacion_sensibilidad.png", "05_OE5.R", """# Situacion actual reconstruida con evidencia real (inventario marzo 2026),
# NO con un supuesto teorico de "cero stock de seguridad"
comparacion <- politica_optima %>%
  left_join(inventario, by = c("sku" = "codigo")) %>%
  mutate(S_actual = replace_na(stock_sistema, 0),
         s_actual = 0)   # sin proceso formal de reorden

>> Costo esperado situacion actual (mensual, 69 SKU):  $3,395,938
>> Costo esperado politica optima  (mensual, 69 SKU):  $1,820,648
>> Ahorro agregado estimado: 46.4%
>> Nivel de servicio: 28.3% (actual) -> 95.4% (optimo)

# Sensibilidad conjunta (beta x escenario de lead time), no una variable
# a la vez: rango de costo entre $903,197 y $963,783 (10 SKU, +/-6.7%)"""))

for fname, titulo, code in figs:
    make_shot(fname, titulo, code, f"{OUT}/{fname}")

print("\\nTotal figuras generadas:", len(figs))
