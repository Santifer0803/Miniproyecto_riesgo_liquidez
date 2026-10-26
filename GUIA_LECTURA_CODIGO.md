# Guía de lectura del código documentado

Los archivos `.R` contienen explicaciones extensas junto a cada operación relevante. Esta guía sirve para ubicarse rápidamente.

## Qué archivo contiene qué

- `R/liquidez_funciones.R`: metodología y cálculos financieros, independientes de Shiny. No crea gráficos ni controles.
- `dashboard/app.R`: carga de las cinco hojas, transformaciones iniciales heredadas, invocación del motor, interfaz y salidas reactivas.

## Cómo leer el flujo completo

1. `app.R` lee el Excel y conserva las transformaciones originales del equipo.
2. `source(...)` carga las funciones; `crear_modelo_liquidez()` construye una lista de tablas.
3. `clasificar_catalogo()` identifica clase y rubro por el código y la denominación.
4. `preparar_saldos()` construye saldos únicos y marca vencimientos y restricciones.
5. `calcular_tasas()` calcula tasas históricas de 30 días.
6. `preparar_flujos()` clasifica pagos y estima la utilización de líneas concedidas.
7. `resumir_bandas()` agrega entradas/salidas y calcula las brechas y su acumulación.
8. `resumir_rubros()`, `resumir_estructura()` y `resumir_vencimientos()` generan vistas de detalle.
9. `crear_tabla_maestra()` combina un único saldo por cuenta con columnas de flujos por banda.
10. `ui` define componentes visibles y `server` filtra tablas y las dibuja. La UI no cambia las fórmulas.

## Fórmulas y unidades

- Días residuales = fecha de pago − fecha de corte.
- Flujo contractual = principal programado + interés programado.
- Brecha por banda = entradas − salidas.
- Brecha acumulada = suma de brechas desde la primera banda hasta la actual.
- Retiro histórico = retiros 30 días / saldo inicial a la vista.
- Renovación histórica = principal renovado / principal vencido 30 días.
- Cobro histórico = cobros recibidos / cobros programados.
- Uso de líneas = desembolsos 30 días / límite otorgado al inicio.

Los datos se calculan en la moneda original: MN en colones y ME en dólares. Dividir entre 1e6 en la interfaz expresa millones de ESA moneda; no es una conversión cambiaria. La brecha no incluye caja inicial ni recursos de cobertura.

## Supuestos académicos que deben explicarse

- El código contable se clasifica por su primer dígito; rubro_liquidez es una agrupación analítica específica del catálogo suministrado.
- Las salidas vencidas se sitúan en 1–8 días; los cobros vencidos no se presumen recuperados.
- El uso de tasas históricas está limitado a flujos futuros dentro de los primeros 30 días.
- Créditos marcados en mora y ciertas inversiones restringidas se llevan a cero en las entradas ajustadas.
- Las líneas concedidas originan una salida contingente estimada en día 1; las recibidas permanecen como fuente potencial fuera de las entradas contractuales.
- Cuando falta la tasa, el código aplica respaldos definidos en `preparar_flujos()`: recuperación/retiro 100 %, renovación 0 %, utilización de líneas 0 % (esta última con nota de falta de datos). Son decisiones del ejercicio y deben revisarse antes de interpretarlas como observaciones.

## Puntos a revisar antes de ampliar el modelo

- La función de mora marca la cartera como morosa si `fecha_primer_pago_impago` tiene dato, sin evaluar antigüedad ni cura posterior.
- Los cobros de inversiones con liberación posterior al pago se excluyen del ajustado, pero no se reprograman automáticamente.
- Los cobros contractuales vencidos quedan fuera de las seis bandas y se reportan por separado.
- La comparación entre entidades usa la brecha acumulada ajustada de 30 días dividida entre activos: es una métrica didáctica, no ICL/IFNE.
- La gráfica de barras de flujos de `app.R` no contiene la opción `categoryarray` en esta versión adjuntada, aunque la línea de acumuladas sí. Ese cambio visual puede hacerse después sin afectar cálculos.
- La interfaz no recalcula el modelo al cambiar entidad/corte/moneda: filtra `modelo`. Un futuro parámetro editable que cambie fórmulas exigirá hacer el cálculo reactivo.

## Distinción rápida de responsabilidades

- Comentarios que dicen **cálculo**: transforman números y producen tasas, flujos, saldos o brechas.
- Comentarios que dicen **datos**: importan, relacionan, validan o filtran registros.
- Comentarios que dicen **presentación**: cambian textos, estilos, gráficos o tablas sin modificar los resultados del motor.
- Comentarios que dicen **supuesto**: decisiones académicas que podrían revisarse con el profesor o al implementar módulos adicionales.

## Garantía de esta entrega

Esta edición agrega exclusivamente comentarios explicativos y cambia el nombre de entrega `liquidez_funciones(1).R` a `liquidez_funciones.R`. Las líneas ejecutables de ambos archivos fueron verificadas automáticamente contra los dos archivos adjuntos por el usuario y permanecen idénticas. No se validó la ejecución integral en una instalación de R en esta sesión.
