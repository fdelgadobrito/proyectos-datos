# ============================================================
# 00_limpieza.R  (v2 — migrado a BASE_DE_DATOS_CONSOLIDADA.xlsx)
# Gestión de Inventario - Cliente Confidencial S.A. (Sucursal Sur)
#
# Este script reemplaza la versión anterior (00_limpieza_v1_hierarquico.R.bak),
# que parseaba el export jerárquico de tabla dinámica original. La base
# consolidada que reemplaza esa fuente ya viene en formato tabular plano,
# con la demanda de servicio (despachos) ya separada y la marca (línea)
# ya etiquetada por el sistema de origen — por lo que ya no hace falta la
# función de parsing por indentación ni la clasificación de marca por
# texto de la descripción (ambas eliminadas de este script).
#
# Fuente de entrada (data/raw/):
#   - BASE_DE_DATOS_CONSOLIDADA.xlsx, hojas:
#       "Análisis de facturas (mensual)"  -> demanda mensual por SKU
#       "Análisis de facturas (ANUAL)"    -> validación cruzada de totales
#       "DESPACHOS"                        -> costo real de despacho (para OE4)
#       "PRODUCTOS SUCURZAL MARZO"        -> inventario físico marzo 2026
#       "PRODUCTOS SUCURSAL JUNIO"        -> estimación propia de la sucursal
#
# Salidas (data/clean/): mismas que la v1, para no romper los scripts 01-05
#   - demanda_mensual_sku.rds / .csv
#   - inventario_marzo2026.rds
#   - mix_scq.rds
#   - despachos.rds   <- NUEVO: reemplaza el parsing manual de SER-* que
#                        antes vivía dentro de 04_OE4.R
#   - log_limpieza.txt
# ============================================================

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

Sys.setlocale("LC_ALL", "C.UTF-8")
options(stringsAsFactors = FALSE)

RAW  <- "data/raw"
CLEAN <- "data/clean"
ARCHIVO <- file.path(RAW, "BASE_DE_DATOS_CONSOLIDADA.xlsx")
dir.create(CLEAN, showWarnings = FALSE, recursive = TRUE)

log_con <- file(file.path(CLEAN, "log_limpieza.txt"), open = "wt")
log_msg <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_con)
}

log_msg("=== 00_limpieza.R v2 — fuente: BASE_DE_DATOS_CONSOLIDADA.xlsx ===")

# ------------------------------------------------------------
# PASO 1. Demanda mensual (hoja "Análisis de facturas (mensual)")
# ------------------------------------------------------------
# La hoja ya viene plana y pre-agregada por SKU x mes (se verificó: 0
# combinaciones SKU+mes duplicadas). Trae 2 columnas vacías al final
# (arrastre de fórmulas de Excel) que se descartan. La marca (línea) ya
# viene etiquetada por el sistema de origen en la columna LINEA.2 — no
# hace falta inferirla desde el texto de la descripción como en la v1.

mensual_raw <- read_excel(ARCHIVO, sheet = "Análisis de facturas (mensual)")
names(mensual_raw) <- make.names(names(mensual_raw), unique = TRUE)
log_msg("Filas leídas de 'Análisis de facturas (mensual)': ", nrow(mensual_raw))

mensual <- mensual_raw %>%
  transmute(
    sku = str_trim(ID_SKU),
    descripcion = str_trim(NOMBRE),
    marca = case_when(
      str_detect(LINEA.2, regex("CONDOR", ignore_case = TRUE)) ~ "CONDOR",
      str_detect(LINEA.2, regex("ANDINA", ignore_case = TRUE)) ~ "ANDINA",
      TRUE ~ NA_character_
    ),
    monto = as.numeric(SUM_VENTA),
    cantidad = as.numeric(CANTIDAD),
    fecha = as.Date(MES_VENTA.1)
  )

sin_marca <- mensual %>% filter(is.na(marca))
log_msg("Registros sin marca reconocida en LINEA.2: ", nrow(sin_marca))
if (nrow(sin_marca) > 0) log_msg("  SKU afectados: ", paste(unique(sin_marca$sku), collapse = ", "))
# A diferencia de la v1 (donde 3 registros de un SKU no traían el sufijo de
# marca en el texto y había que reclasificarlos manualmente), aquí la marca
# viene de una columna dedicada del sistema de origen: no se esperan casos
# sin marca. Si aparecieran, se documentan pero no se descartan (se
# asignan a CONDOR por ser la línea de foco del proyecto), igual que en la v1.
mensual <- mensual %>% mutate(marca = if_else(is.na(marca), "CONDOR", marca))

# ------------------------------------------------------------
# PASO 2. Validación de calidad de datos
# ------------------------------------------------------------
log_msg("")
log_msg("=== Validación de calidad de datos ===")
n_nulos <- sum(is.na(mensual$monto) | is.na(mensual$cantidad) | is.na(mensual$fecha))
log_msg("Valores nulos en monto/cantidad/fecha: ", n_nulos)

dup_exactos <- mensual %>% count(fecha, sku, monto, cantidad) %>% filter(n > 1)
log_msg("Duplicados exactos (mismo periodo+SKU+monto+cantidad): ", sum(dup_exactos$n - 1))

# Variantes de sufijo de código (-I/-P/-C): igual que en la v1, se agregan
# por suma bajo el código base, porque en el negocio son el mismo producto
# físico despachado con distinta condición de protección.
mensual <- mensual %>% mutate(sku_base = str_remove(sku, "-(I|P|C)$"))

multi_variante <- mensual %>% count(fecha, sku_base) %>% filter(n > 1)
log_msg("SKU-mes con más de una variante de sufijo (se agregan por suma): ", nrow(multi_variante))

nombre_inconsistente <- mensual %>% distinct(sku_base, descripcion) %>% count(sku_base) %>% filter(n > 1)
log_msg("SKU con más de una descripción distinta registrada: ", nrow(nombre_inconsistente))

# ------------------------------------------------------------
# PASO 3. Consolidación del panel mensual (agregado por sku_base x mes)
# ------------------------------------------------------------
demanda_mensual <- mensual %>%
  group_by(fecha, sku_base, marca) %>%
  summarise(
    descripcion = first(descripcion),
    monto = sum(monto, na.rm = TRUE),
    cantidad = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(sku = sku_base) %>%
  mutate(anio = as.integer(format(fecha, "%Y")), mes_num = as.integer(format(fecha, "%m"))) %>%
  arrange(sku, fecha)

# El mes en curso (julio 2026) sigue incompleto a la fecha de este análisis:
# se excluye del panel, igual que en la v1.
mes_actual <- as.Date("2026-07-01")
incompleto <- demanda_mensual %>% filter(fecha == mes_actual)
log_msg("")
log_msg("=== Consolidación del universo de análisis ===")
log_msg("Mes en curso excluido por estar incompleto (julio 2026): ", nrow(incompleto), " registros SKU.")
demanda_mensual <- demanda_mensual %>% filter(fecha < mes_actual)

log_msg("Ventana temporal final del panel: ", format(min(demanda_mensual$fecha), "%Y-%m"),
        " a ", format(max(demanda_mensual$fecha), "%Y-%m"),
        " (", n_distinct(demanda_mensual$fecha), " meses).")
log_msg("SKU únicos en el panel: ", n_distinct(demanda_mensual$sku))
log_msg("Registros finales (SKU x mes) en el panel limpio: ", nrow(demanda_mensual))
log_msg("  de los cuales CONDOR: ", sum(demanda_mensual$marca == "CONDOR"),
        " | ANDINA: ", sum(demanda_mensual$marca == "ANDINA"))

saveRDS(demanda_mensual, file.path(CLEAN, "demanda_mensual_sku.rds"))
write.csv(demanda_mensual, file.path(CLEAN, "demanda_mensual_sku.csv"), row.names = FALSE)

# ------------------------------------------------------------
# PASO 4. Validación cruzada contra la hoja "Análisis de facturas (ANUAL)"
# ------------------------------------------------------------
# Nota: esta hoja no distingue año por SKU (2 filas por SKU sin columna de
# periodo cuando hubo venta en ambos años). No se usa para nada estructural
# del análisis, solo como chequeo de consistencia del total agregado.
anual_raw <- read_excel(ARCHIVO, sheet = "Análisis de facturas (ANUAL)")
names(anual_raw) <- make.names(names(anual_raw), unique = TRUE)
total_mensual <- sum(demanda_mensual$monto, na.rm = TRUE) +
  sum(mensual$monto[mensual$fecha == mes_actual], na.rm = TRUE)  # incluye el mes excluido para comparar universo completo
total_anual <- sum(as.numeric(anual_raw$SUM_VENTA), na.rm = TRUE)
log_msg("")
log_msg("=== Validación cruzada contra hoja ANUAL ===")
log_msg("Total venta (hoja mensual, ventana completa incl. julio 2026): $", format(round(total_mensual), big.mark = ","))
log_msg("Total venta (hoja anual): $", format(round(total_anual), big.mark = ","))
log_msg("Diferencia: ", round(100 * abs(total_mensual - total_anual) / total_anual, 2),
        "% (diferencia menor esperable entre ambas hojas; no afecta el análisis, que usa la hoja mensual)")

# ------------------------------------------------------------
# PASO 5. Despachos (hoja "DESPACHOS") — para estimación empírica de K en el OE4
# ------------------------------------------------------------
despachos_raw <- read_excel(ARCHIVO, sheet = "DESPACHOS")
names(despachos_raw) <- make.names(names(despachos_raw), unique = TRUE)
despachos <- despachos_raw %>%
  transmute(
    sku = str_trim(ID_SKU),
    descripcion = str_trim(NOMBRE),
    valor_unitario = as.numeric(VALOR_UNITARIO),
    cantidad = as.numeric(CANTIDAD),
    monto = as.numeric(SUM_VENTA),
    fecha = as.Date(MES_VENTA.1)
  )
log_msg("")
log_msg("=== Despachos (para K empírico en OE4) ===")
log_msg("Registros de despacho: ", nrow(despachos))
log_msg("Total monto despachos: $", format(round(sum(despachos$monto)), big.mark = ","))
log_msg("Total unidades (n despachos): ", sum(despachos$cantidad))
log_msg("K empírico (todos los tramos): $", format(round(sum(despachos$monto) / sum(despachos$cantidad)), big.mark = ","))
saveRDS(despachos, file.path(CLEAN, "despachos.rds"))

# ------------------------------------------------------------
# PASO 6. Inventario físico (hoja "PRODUCTOS SUCURZAL MARZO")
# ------------------------------------------------------------
inv_raw <- read_excel(ARCHIVO, sheet = "PRODUCTOS SUCURZAL MARZO")
names(inv_raw) <- make.names(names(inv_raw), unique = TRUE)
inventario <- inv_raw %>%
  transmute(
    codigo = str_trim(ID_SKU),
    descripcion = str_trim(NOMBRE),
    marca = if_else(str_detect(LINEA.3, regex("ANDINA", ignore_case = TRUE)), "ANDINA", "CONDOR"),
    stock_sistema = as.numeric(STOCK_SIS_ACTUAL),
    stock_fisico = as.numeric(STOCK_SUC_ACTUAL),
    diferencia = stock_fisico - stock_sistema
  )
log_msg("")
log_msg("=== Inventario físico marzo 2026 ===")
log_msg("SKU en inventario: ", nrow(inventario))
log_msg("SKU con diferencia sistema vs físico != 0: ", sum(inventario$diferencia != 0, na.rm = TRUE))
saveRDS(inventario, file.path(CLEAN, "inventario_marzo2026.rds"))

# ------------------------------------------------------------
# PASO 7. Mix de productos de la sucursal (hoja "PRODUCTOS SUCURSAL JUNIO")
# ------------------------------------------------------------
mix_raw <- read_excel(ARCHIVO, sheet = "PRODUCTOS SUCURSAL JUNIO")
names(mix_raw) <- make.names(names(mix_raw), unique = TRUE)
mix <- mix_raw %>%
  transmute(
    codigo = str_trim(ID_SKU),
    descripcion = str_trim(NOMBRE),
    stock_sucursal = as.numeric(STOCK_SUC_ACTUAL),
    stock_casa_matriz = as.numeric(STOCK_CASAMA_ACTUAL),
    ventas_12m = as.numeric(VENTA_12MESES),
    promedio_mensual = as.numeric(VENTA_MEDIA_MENSUAL),
    estacionalidad = ESTACIONALIDAD,
    demanda_est_6m = as.numeric(DEMANDA_ES_6MESES)
  )
log_msg("")
log_msg("=== Mix de productos de la sucursal (estimación propia) ===")
log_msg("SKU con estimación propia: ", nrow(mix))
saveRDS(mix, file.path(CLEAN, "mix_scq.rds"))

log_msg("")
log_msg("=== Limpieza finalizada (v2, base consolidada). Archivos guardados en data/clean/ ===")
close(log_con)
