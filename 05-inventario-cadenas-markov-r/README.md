# Proyecto R — Gestión de Inventario Cliente Confidencial S.A. (Sucursal Sur)

## Versión 2.0 — migrado a la base de datos consolidada

Esta versión reemplaza la fuente de datos original (export jerárquico de tabla
dinámica) por `BASE_DE_DATOS_CONSOLIDADA.xlsx`, cuya limpieza estructural
(aplanar la tabla dinámica, tipificar columnas, dividir el campo código+
descripción, calcular precio unitario) se hizo en Power Query (Excel) antes de
continuar en R. El script `00_limpieza.R` v1 (que hacía ese parsing jerárquico
a mano) queda respaldado en `00_limpieza_v1_hierarquico.R.bak` para referencia.

Cambios de resultado relevantes v1 → v2 (69 → 70 SKU críticos, por ajuste del
mismo criterio de calidad de datos sobre una fuente más limpia): ver
`output/tablas/` para el detalle completo.

## Estructura

```
scripts/        Scripts de análisis, en orden de ejecución
output/tablas/  Tablas de resultados (.csv) usadas en el informe
output/figuras/ Capturas de código y de Power Query usadas en el informe (.png)
```

> Los datos originales del cliente (`data/raw`, `data/clean`) no se incluyen en
> este repo por confidencialidad. Los scripts se dejan como referencia del
> proceso de análisis.

## Orden de ejecución

```bash
Rscript scripts/00_limpieza.R   # Limpieza y consolidación de datos (OE1, parte 1)
Rscript scripts/01_OE1.R        # Clasificación ABC y selección de SKU críticos
Rscript scripts/02_OE2.R        # Ajuste de demanda y simulación de lead time
Rscript scripts/03_OE3.R        # Cadena de Markov y validación cruzada
Rscript scripts/04_OE4.R        # Política óptima (s*, S*) — puede tardar unos minutos
Rscript scripts/05_OE5.R        # Comparación situación actual vs. óptima y sensibilidad
```

Cada script lee las salidas del anterior desde `data/clean/`, por lo que deben
ejecutarse en este orden la primera vez. Los supuestos de negocio (lead time,
tasa de mantención, margen, beta de recuperación) están documentados como
constantes explícitas al inicio de cada script y marcados como "CUADRO DE
SUPUESTOS" — pendientes de validar con la organización.

## Paquetes R requeridos

readxl, dplyr, tidyr, stringr, fitdistrplus, markovchain, ggplot2, openxlsx, readr

