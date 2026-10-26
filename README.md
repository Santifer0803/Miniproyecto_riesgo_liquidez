# Miniproyecto_riesgo_liquidez

## 1. Ruta del código y responsabilidades

El `app.R` contiene el bloque de carga y limpieza original y llama al archivo
`R/liquidez_funciones.R`. El Rmd hace exactamente lo mismo. Las funciones
compartidas calculan una sola versión de los datos; la interfaz únicamente
filtra por entidad, cierre y moneda y visualiza los resultados.

No conviene introducir nuevas fórmulas distintas dentro de Shiny: deben ir
primero en el archivo de funciones y validarse desde el Rmd.

## 2. Relación de las hojas

- `Datos_Contables`: identificador `ID_REGISTRO` y saldos a cada cierre.
- `Catalogo_Usado`: `CUENTA_CONTABLE` y denominación oficial.
- `Pagos_Contractuales`: `ID_REGISTRO` y fechas/importes de capital e interés.
- `Observaciones`: una observación por entidad/moneda/cierre, con comportamiento histórico de 30 días.
- `Tipo_Cambio`: información de conversión disponible, no utilizada al sumar brechas por moneda.

La carga original anexa pagos y tipos de cambio al objeto `datos_contables`. Para
sumar saldos, la función `preparar_saldos` recupera una sola fila por
`id_registro`; utiliza `datos_contrato` para los flujos, sin sumar de nuevo
el mismo saldo.

Control de la base suministrada: 4.020 IDs contables únicos; 3.720 pagos
contractuales con ID único; 36 cuentas; 60 combinaciones de fecha, entidad
y moneda. No se deben convertir estas cantidades en reglas generales de otras bases.

## 3. Clasificación de cuentas

`clasificar_catalogo` añade:

- `clase_contable`: primer dígito 1 activo, 2 pasivo, 3 patrimonio, 6 contingente.
- `rubro_liquidez`: agrupación por tres primeros dígitos según la cuenta del catálogo.
- `tratamiento_general`: documentación resumida del tratamiento previsto.

Hay 20 cuentas de activo, 12 de pasivo, 2 de patrimonio y 2 contingentes.
Esta clasificación contable sigue CONASSIF 6-18, art. 26. El tratamiento
para liquidez es una interpretación académica, no un criterio automático
regulatorio para ICL ni IFNE.

## 4. Vencimientos y restricciones

`preparar_saldos` calcula días desde `periodo` hasta `fecha_vencimiento` y la
banda. Muestra además un atributo `disponibilidad`. El efectivo está disponible
al corte; el encaje no se presume libre; el terreno no tiene cobro contractual;
las inversiones restringidas o con saldo comprometido necesitan revisión de
su fecha de liberación y fin de compromiso. El estado de los activos no
modifica automáticamente los saldos contables.

Bandas acordadas: **1–8, 9–15, 16–30, 31–89, 90–180 y 181+ días**.
Las posiciones sin vencimiento se identifican como **Sin fecha**.

## 5. Flujos de efectivo y brechas

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

## 6. Versión ajustada por comportamiento

`calcular_tasas` utiliza la observación de los 30 días previos a cada cierre:

- tasa de retiro = retiros a la vista / saldo inicial de depósitos a la vista;
- tasa de renovación = principal renovado / vencimientos de depósitos a plazo;
- tasa de cobro = cobros recibidos / cobros programados;
- tasa de uso de líneas = desembolsos de líneas / límite inicial otorgado.

Se acota cada tasa al intervalo [0,1]. Para los movimientos dentro de
los próximos 30 días se aplican los supuestos:

- depósitos a la vista: importe contractual × tasa de retiro;
- depósitos a plazo: principal × (1 − tasa de renovación) + intereses;
- cobros de créditos: importe contractual × tasa de cobro;
- créditos con mora: no se reconocen como entrada cobrable en el ajustado;
- inversiones restringidas aún no liberadas: no se reconocen como entrada
  utilizable en el ajustado;
- líneas concedidas no utilizadas: desembolso contingente estimado igual a
  saldo no utilizado × tasa de utilización; se ubica en día 1.

Se mantienen los flujos contractuales para vencimientos superiores a 30 días,
ya que no hay tasas históricas de esos horizontes. Los resultados son
supuestos didácticos sujetos a validación por el equipo y su profesor.

No se aplica una transformación automática de las líneas de financiamiento
recibidas a ingresos de efectivo: su disponibilidad y costo pertenecen al
módulo de cobertura. Las tasas faltantes aparecen como datos no observables.

## 7. Tablas obtenidas

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

## 8. Cómo presentar el dashboard

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

## 9. Verificación numérica externa

Para la base **sin modificar**, entidad **A**, fecha **2026-04-30**, moneda
**MN**, la suma independiente de pagos contractuales de la banda **1–8** da,
redondeada al colón:

- Entradas: ₡2.869.896.156.
- Salidas: ₡57.766.659.206.
- Brecha: −₡54.896.763.050.

Se calculó leyendo los pagos contractuales, enlazando por ID y clasificando
los códigos, sin usar el código R. Sirve como prueba de conciliación. Si se
modifica la base de datos, esta referencia también puede cambiar.

## 10. Alcance y limitaciones pendientes

La herramienta no calcula aún faltantes residuales, costos de venta,
encaje aprovechable, ICL, IFNE, VaR, LVaR ni tensión. Esos análisis deberán
consumir los resultados de flujos, saldos y recursos sin contarlos dos veces.

**Fuente de la clasificación:** Reglamento de Información Financiera,
CONASSIF 6-18, artículo 26 y anexos oficiales, accesibles desde
https://www.sugef.fi.cr/normativa/NormativaTransversal.aspx