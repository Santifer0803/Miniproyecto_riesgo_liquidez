# Miniproyecto_riesgo_liquidez
## Secciones 2.1, 2.2 y 2.3
### 1. Ruta del código y responsabilidades

El `app.R` contiene el bloque de carga y limpieza original y llama al archivo
`R/liquidez_funciones.R`. 

### 2. Relación de las hojas

- `Datos_Contables`: identificador `ID_REGISTRO` y saldos a cada cierre.
- `Catalogo_Usado`: `CUENTA_CONTABLE` y denominación oficial.
- `Pagos_Contractuales`: `ID_REGISTRO` y fechas/importes de capital e interés.
- `Observaciones`: una observación por entidad/moneda/cierre, con comportamiento histórico de 30 días.
- `Tipo_Cambio`: información de conversión disponible, no utilizada al sumar brechas por moneda.


### 3. Clasificación de cuentas

`clasificar_catalogo` añade:

- `clase_contable`: primer dígito 1 activo, 2 pasivo, 3 patrimonio, 6 contingente.
- `rubro_liquidez`: agrupación por tres primeros dígitos según la cuenta del catálogo.
- `tratamiento_general`: documentación resumida del tratamiento previsto.

Hay 20 cuentas de activo, 12 de pasivo, 2 de patrimonio y 2 contingentes.
Esta clasificación contable sigue CONASSIF 6-18, art. 26.

### 4. Vencimientos y restricciones

`preparar_saldos` calcula días desde `periodo` hasta `fecha_vencimiento` y la
banda. Muestra además un atributo `disponibilidad`.

Bandas acordadas: **1–8, 9–15, 16–30, 31–89, 90–180 y 181+ días**.
Las posiciones sin vencimiento se identifican como **Sin fecha**.

### 5. Flujos de efectivo y brechas

`preparar_flujos` enlaza cada pago con la clase contable:

- Activos: entradas; pasivos: salidas.
- Patrimonio y contingentes: no se convierten en entradas/salidas contractuales.
- Importe contractual: `monto_capital + monto_interes`.
- Una salida vencida se coloca en banda 1–8; un cobro vencido se consulta
  por separado, sin incluirlo en la proyección como entrada recibida.

`resumir_bandas` produce, para cada banda:

`brecha = entradas - salidas`

`brecha acumulada = suma de brechas hasta esa banda`

Esto es una brecha **antes de cobertura**: el dinero en caja, una línea de
financiamiento recibida y la venta de inversiones no se añaden todavía.

### 6. Versión ajustada por comportamiento

`calcular_tasas` utiliza la observación de los 30 días previos a cada cierre:

- tasa de retiro = retiros a la vista / saldo inicial de depósitos a la vista;
- tasa de renovación = principal renovado / vencimientos de depósitos a plazo;
- tasa de cobro = cobros recibidos / cobros programados;
- tasa de uso de líneas = desembolsos de líneas / límite inicial otorgado.

Se acota cada tasa al intervalo [0,1]. 

### 7. Tablas obtenidas

`crear_modelo_liquidez` devuelve una lista:

| Objeto | Contenido |
| --- | --- |
| `catalogo` | Las 36 cuentas, clase, rubro y tratamiento |
| `saldos` | Saldos únicos, categoría, vencimiento y disponibilidad |
| `tasas` | Cuatro tasas y datos de financiamiento recibido |
| `flujos` | Pagos individuales y desembolsos contingentes estimados |
| `bandas` | Entradas, salidas, brechas y acumulados de ambas metodologías |
| `rubros` | Entradas/salidas detalladas por cuenta y banda |
| `estructura` | Saldos agrupados por cuenta y disponibilidad |
| `vencimientos` | Concentración de saldos por banda |
| `tabla_maestra` | Una fila por cuenta y corte; saldo único y seis bandas de flujos |
| `verificaciones` | Controles de integridad estructural |

La `tabla_maestra` evita repetir el saldo por cada banda de flujo. Si se suman
directamente columnas de saldos en `flujos`, se obtiene doble conteo.

### 8. Cómo presentar el dashboard

**Resumen (2.1):** seleccionar entidad A/B/C/D/E, fecha, moneda, vista
contractual o ajustada; interpretar el primer déficit acumulado y comparar
la brecha a 30 días en relación con los activos de cada entidad.

**Estructura (2.2):** identificar disponibilidades, encaje, inversiones,
créditos, depósitos y financiamientos. Mostrar en qué plazos vencen y cuáles
son los recursos restringidos o sin fecha definida.

**Flujos (2.3):** explicar entradas y salidas por cuenta, las seis bandas,
la diferencia entre brecha por banda y acumulada y los efectos de utilizar
historial de retiros, renovación, recuperación y desembolso de líneas.

**Metodología:** mostrar clasificación, trazabilidad y supuestos. Los CSV
permiten exportar la selección para conciliaciones con otros módulos.

### Fuente de la clasificación Reglamento de Información Financiera,
CONASSIF 6-18, artículo 26 y anexos oficiales, accesibles desde
[https://www.sugef.fi.cr/normativa/NormativaTransversal.aspx](https://www.sugef.fi.cr/normativa/normativa_transversal/documentos/CONASSIF%206-18%20%28v25%208%20enero%202026%29.pdf)
