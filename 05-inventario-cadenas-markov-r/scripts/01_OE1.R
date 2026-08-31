# ============================================================
# 01_OE1.R
# OE1. Diagnosticar el estado actual de la gestión de inventario de las
# líneas Cóndor y ANDINA en la sucursal Sur, mediante el
# levantamiento y sistematización de los registros de ventas e inventario
# disponibles y la clasificación ABC del portafolio, identificando dentro
# de la línea Cóndor los productos de mayor criticidad que serán objeto
# del análisis detallado en las fases siguientes.
#
# Entrada: data/clean/demanda_mensual_sku.rds (generado por 00_limpieza.R)
# Salida:  data/clean/abc_clasificacion.rds
#          data/clean/sku_criticos.rds   <- universo que sigue a OE2-OE5
#          output/tablas/tabla_8_1_abc.csv
# ============================================================

suppressMessages({
  library(dplyr)
  library(readr)
})

CLEAN <- "data/clean"
TABLAS <- "output/tablas"
dir.create(TABLAS, showWarnings = FALSE, recursive = TRUE)

demanda <- readRDS(file.path(CLEAN, "demanda_mensual_sku.rds"))

# ------------------------------------------------------------
# 8.1.6 Clasificación ABC
# ------------------------------------------------------------
# Se aplica el principio de Pareto sobre el VALOR DE VENTAS acumulado
# (monto, en pesos) de todo el portafolio (Cóndor + Andina) durante la
# ventana de 18 meses, tal como especifica el OE1 corregido: el diagnóstico
# ABC se hace sobre AMBAS marcas; la modelación detallada de fases
# posteriores se acota luego a los SKU críticos de Cóndor (ver 5.1 Alcance).

resumen_sku <- demanda %>%
  group_by(sku, marca) %>%
  summarise(
    descripcion = first(descripcion),
    monto_total = sum(monto, na.rm = TRUE),
    unidades_totales = sum(cantidad, na.rm = TRUE),
    meses_con_venta = n_distinct(fecha[cantidad > 0]),
    .groups = "drop"
  ) %>%
  arrange(desc(monto_total)) %>%
  mutate(
    monto_acum = cumsum(monto_total),
    pct_acum = monto_acum / sum(monto_total),
    categoria_abc = case_when(
      pct_acum <= 0.80 ~ "A",
      pct_acum <= 0.95 ~ "B",
      TRUE ~ "C"
    )
  )

cat("=== Clasificación ABC (Cóndor + Andina, 18 meses) ===\n")
print(resumen_sku %>% count(marca, categoria_abc))

cat("\nSKU categoría A por marca:\n")
print(resumen_sku %>% filter(categoria_abc == "A") %>% count(marca))

saveRDS(resumen_sku, file.path(CLEAN, "abc_clasificacion.rds"))
write_csv(resumen_sku, file.path(TABLAS, "tabla_8_1_abc.csv"))

# ------------------------------------------------------------
# 8.1.7 Selección de SKU críticos: el stock en cero / meses con venta
# ------------------------------------------------------------
# El alcance del proyecto acota la modelación detallada a los SKU
# categoría A de la línea CONDOR (ver 5.1 y 2.5-2.6 de la bitácora de
# correcciones). Dentro de esos SKU A, se excluyen los que tienen muy
# pocos meses con venta positiva (< 3 de 18) porque no permiten un ajuste
# estadístico mínimamente confiable en el OE2 (criterio que responde
# directamente al punto 5 del diagnóstico ChatGPT: "SKU con 2
# observaciones tratado con el mismo rigor que el resto" -> aquí se
# excluye explícitamente en vez de forzarlo).

# Umbral de meses con venta positiva mínimos para que un SKU se considere
# "crítico" y pase a las fases de modelación (OE2-OE5).
#
# Justificación metodológica: en un primer corte se usó el umbral mínimo
# posible para no perder SKU de la categoría A (>=3 de 18 meses). Sin
# embargo, al ejecutar el OE2 (ajuste de distribuciones de probabilidad
# sobre los valores positivos de demanda), se determinó que un ajuste por
# máxima verosimilitud con solo 3 observaciones no entrega parámetros
# mínimamente confiables (la variabilidad estimada queda dominada por el
# ruido muestral, y en 12 de los 79 SKU el optimizador ni siquiera
# convergía). Se corrige entonces el criterio de corte del OE1 para que sea
# consistente con el requisito mínimo del OE2: se exige un mínimo de 4
# observaciones positivas (>=4 de 18 meses), que es el mínimo aceptado en la
# literatura de ajuste de distribuciones de 2 parámetros por MLE (se
# necesitan al menos "parámetros + 2" observaciones para que la estimación
# no sea degenerada). Este ajuste se documenta explícitamente porque cambia
# el tamaño del universo de SKU críticos que se reporta en el OE1.
MIN_MESES_CON_VENTA <- 4

candidatos_A_condor <- resumen_sku %>%
  filter(marca == "CONDOR", categoria_abc == "A")

sku_criticos <- candidatos_A_condor %>%
  filter(meses_con_venta >= MIN_MESES_CON_VENTA)

excluidos_por_escasez <- candidatos_A_condor %>%
  filter(meses_con_venta < MIN_MESES_CON_VENTA)

cat("\n=== Selección de SKU críticos (categoría A, línea Cóndor) ===\n")
cat("SKU categoría A en Cóndor:", nrow(candidatos_A_condor), "\n")
cat("Excluidos por < ", MIN_MESES_CON_VENTA, " meses con venta positiva: ",
    nrow(excluidos_por_escasez), "\n", sep = "")
if (nrow(excluidos_por_escasez) > 0) {
  print(excluidos_por_escasez %>% select(sku, descripcion, meses_con_venta))
}
cat("SKU críticos que continúan a OE2-OE5:", nrow(sku_criticos), "\n")

saveRDS(sku_criticos, file.path(CLEAN, "sku_criticos.rds"))
write_csv(sku_criticos, file.path(TABLAS, "tabla_8_2_sku_criticos.csv"))

cat("\n=== 01_OE1.R finalizado. Salidas en data/clean/ y output/tablas/ ===\n")
