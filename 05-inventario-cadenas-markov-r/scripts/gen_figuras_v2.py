import sys
sys.path.insert(0, "/home/claude/proyecto_R/scripts")
from make_code_shot import make_shot

OUT = "/home/claude/proyecto_R/output/figuras"

figs = []

# ---------- Figura 8.3 (NUEVA: carga de datos ya limpios + validacion en R) ----------
figs.append(("fig_8_3_validacion.png", "00_limpieza.R", """# Los datos ya vienen limpios y estructurados desde Excel/Power Query:
# tabla plana, columnas tipificadas, y la marca (linea) ya etiquetada
# por el sistema de origen en la columna LINEA.2 -- ya no hace falta
# inferir la marca por texto ni una funcion de parsing jerarquico.

mensual_raw <- read_excel(ARCHIVO, sheet = "Analisis de facturas (mensual)")

mensual <- mensual_raw %>%
  transmute(
    sku = str_trim(ID_SKU),
    descripcion = str_trim(NOMBRE),
    marca = case_when(
      str_detect(LINEA.2, regex("GORILA", ignore_case = TRUE)) ~ "GORILA",
      str_detect(LINEA.2, regex("DIFEMAT", ignore_case = TRUE)) ~ "DIFEMAT",
      TRUE ~ NA_character_),
    monto = as.numeric(SUM_VENTA),
    cantidad = as.numeric(CANTIDAD),
    fecha = as.Date(MES_VENTA.1)
  )

# Validacion de calidad (igual rigor que antes, ahora sobre datos limpios)
n_nulos <- sum(is.na(mensual$monto) | is.na(mensual$cantidad) | is.na(mensual$fecha))
dup_exactos <- mensual %>% count(fecha, sku, monto, cantidad) %>% filter(n > 1)

>> Resultado real:
>> Filas leidas de la hoja ya limpia: 1,771
>> Registros SIN marca reconocida en LINEA.2: 0  (antes: 3, con parsing manual)
>> Valores nulos en monto/cantidad/fecha: 0
>> Duplicados exactos: 0"""))

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

MIN_MESES_CON_VENTA <- 4  # minimo para un ajuste MLE de 2 parametros confiable
sku_criticos <- resumen_sku %>%
  filter(marca == "GORILA", categoria_abc == "A") %>%
  filter(meses_con_venta >= MIN_MESES_CON_VENTA)

>> Resultado real: 126 SKU Gorila categoria A -> 70 SKU criticos tras el
>> filtro de calidad de datos (56 excluidos por < 4 meses con venta)"""))

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

>> Resultado real (70 SKU criticos):
>> lumpy: 26   intermitente: 28   erratico: 12   smooth: 4
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

>> La sobre-dispersion se repite en 67 de 70 SKU evaluados: mezclar meses
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
}

>> Resultado real (70 SKU): lognormal 40 | gamma 22 | normal 6 | constante 2"""))

# ---------- Figura 8.8 ----------
figs.append(("fig_8_8_contraste.png", "02_OE2.R", """contraste_poisson <- panel_completo %>%
  group_by(sku) %>%
  summarise(media = mean(cantidad), varianza = var(cantidad),
            razon_var_media = varianza / media, .groups = "drop") %>%
  mutate(sobredispersion = razon_var_media > 1.5)

# Prueba de bondad de ajuste Kolmogorov-Smirnov del modelo ganador
ks_ejemplo <- ks.test(positivos_ejemplo, "pgamma", par1, par2)

>> Resultado real:
>> SKU con sobre-dispersion relevante (var/media > 1.5): 67 de 70
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
}

P_correcta[1, 1]
>> [1] 0.0708      # ya no es absorbente

# Validacion cruzada: cadena analitica vs 5,000 periodos de Monte Carlo
>> P(quiebre) analitico:  0.0846
>> P(quiebre) simulado:   0.0828   (diferencia: 0.0018)
>> Sobre los 70 SKU: diferencia promedio 0.76%, maxima 3.17%"""))

# ---------- Figura 8.13 ----------
figs.append(("fig_8_13_estacionaria.png", "03_OE3.R", """calcular_estacionaria <- function(P, tol = 1e-10, max_iter = 2000) {
  n <- nrow(P)
  if (n <= 40) {
    A <- t(P) - diag(n)
    A[n, ] <- 1
    b <- c(rep(0, n - 1), 1)
    return(solve(A, b))
  }
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
# (hoja "DESPACHOS" de la base consolidada, ya separada de la demanda
# de producto en la limpieza de datos)
despachos <- readRDS("data/clean/despachos.rds")

K_regular <- sum(despachos$monto) / sum(despachos$cantidad)

brackets_pequenos <- c("SER-110-110-156","SER-110-110-157","SER-110-110-154")
peq <- despachos %>% filter(sku %in% brackets_pequenos)
K_directo <- sum(peq$monto) / sum(peq$cantidad)

>> K (costo de pedido regular, empirico, 110 despachos):  $10,910
>> K_directo (despacho puntual/urgente, tramos pequenos): $7,015"""))

# ---------- Figura 8.15 ----------
figs.append(("fig_8_15_costo_rotura_ajustado.png", "04_OE4.R", """# CUADRO DE SUPUESTOS (pendientes de validar con la empresa)
TASA_MANTENCION_MENSUAL <- 0.02   # 2% mensual sobre el valor unitario
MARGEN_BRUTO <- 0.30              # 30% del precio de venta
BETA_RECUPERACION <- 0.30         # fraccion recuperada via despacho directo

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

>> Resultado real: 68 de 70 SKU alcanzan el nivel de servicio objetivo (95%)"""))

# ---------- Figura 8.17 ----------
figs.append(("fig_8_17_comparacion_sensibilidad.png", "05_OE5.R", """# Situacion actual reconstruida con evidencia real (inventario marzo 2026),
# NO con un supuesto teorico de "cero stock de seguridad"
comparacion <- politica_optima %>%
  left_join(inventario, by = c("sku" = "codigo")) %>%
  mutate(S_actual = replace_na(stock_sistema, 0),
         s_actual = 0)   # sin proceso formal de reorden

>> Costo esperado situacion actual (mensual, 70 SKU):  $3,499,075
>> Costo esperado politica optima  (mensual, 70 SKU):  $1,921,696
>> Ahorro agregado estimado: 45.1%
>> Nivel de servicio: 27.9% (actual) -> 95.4% (optimo)

# Sensibilidad conjunta (beta x escenario de lead time): rango de costo
# entre $954,764 y $1,020,125 (10 SKU, +/-6.8%)"""))

for fname, titulo, code in figs:
    make_shot(fname, titulo, code, f"{OUT}/{fname}")

print("\\nTotal figuras regeneradas:", len(figs))
