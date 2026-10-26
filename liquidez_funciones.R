# Funciones del módulo 2.1–2.3. No modifican los objetos de origen.
# Cada fecha de corte y moneda se analiza por separado.
# Fuente para las clases: CONASSIF 6-18, art. 26:
# https://www.sugef.fi.cr/normativa/NormativaTransversal.aspx
# Las ponderaciones de comportamiento y los plazos son supuestos académicos.

# Configuración | Las bandas son intervalos de días RESIDUALES desde cada cierre.
# No equivalen a fechas del calendario: '1-8' significa de uno a ocho días posteriores
# al cierre, no los días 1 al 8 del mes. Este vector determina también el orden
# de las brechas acumuladas y el orden que debe respetarse en los gráficos.
# 'Vencido/exigible' y 'Sin fecha' se informan aparte de estas seis bandas.
bandas_liquidez <- c("1-8", "9-15", "16-30", "31-89", "90-180", "181+")

# Función: clasificar_banda(dias)
# Rol: transformación temporal, no cálculo monetario.
# Entrada: vector numérico de días entre fecha futura y cierre; puede contener NA.
# Salida: vector de etiquetas con seis bandas, más 'Vencido/exigible' o 'Sin fecha'.
# case_when() evalúa las condiciones de arriba hacia abajo; por eso 'dias <= 15'
# representa los días 9-15, ya que los días 1-8 se capturaron previamente.
clasificar_banda <- function(dias) {
  dplyr::case_when(
# Si no existe fecha, no se inventa una fecha contractual ni una banda futura.
    is.na(dias) ~ "Sin fecha",
# Plazos cero o negativos: la obligación ya es exigible o el cobro ya venció.
    dias <= 0 ~ "Vencido/exigible",
# El orden de condiciones evita que las bandas se traslapen.
    dias <= 8 ~ "1-8",
    dias <= 15 ~ "9-15",
    dias <= 30 ~ "16-30",
    dias <= 89 ~ "31-89",
    dias <= 180 ~ "90-180",
# Los días mayores que 180 entran en la banda abierta de largo plazo.
    TRUE ~ "181+"
  )
}

# Función: tasa_acotada(numerador, denominador)
# Rol: cálculo de proporciones históricas observadas durante 30 días.
# Entrada: dos vectores numéricos de la misma longitud o reciclables por R.
# Salida: tasa en [0,1] si el denominador existe y es positivo; en caso contrario NA.
# La restricción pmax(0, ...) y pmin(1, ...) impide tasas negativas o superiores a 100%.
# Es un supuesto de tratamiento de datos: NO demuestra que la tasa real no pueda
# superar el 100% bajo otra definición de exposición.
tasa_acotada <- function(numerador, denominador) {
# Un denominador cero o faltante no se interpreta como tasa cero.
  dplyr::if_else(!is.na(denominador) & denominador > 0,
# pmax impone el mínimo 0; pmin impone el máximo 1; NA_real_ mantiene tipo numérico.
                 pmin(1, pmax(0, numerador / denominador)), NA_real_)
}

# Función: clasificar_catalogo(datos_catalogo)
# Rol: clasificación contable y etiquetado analítico de cada cuenta.
# Entrada: catálogo original con cuenta_contable y denominacion_oficial.
# Salida: catálogo enriquecido con clase_contable, rubro_liquidez y tratamiento_general.
# Fuente de la CLASE: art. 26 del CONASSIF 6-18 (primer dígito del código).
# Fuente del RUBRO: agrupación académica por prefijos y denominaciones de ESTE catálogo.
# tratamiento_general es una descripción para interpretar el saldo: no mueve efectivo.
# Nota: esta función no cambia datos_catalogo en el entorno original; genera otra tabla.
clasificar_catalogo <- function(datos_catalogo) {
  datos_catalogo |>
    dplyr::mutate(
# Los códigos pasan de factor a texto; NO son cantidades para sumar.
      cuenta_contable = as.character(cuenta_contable),
# Los tres primeros dígitos permiten identificar el nivel de cuenta/rubro.
      prefijo = substr(cuenta_contable, 1, 3),
# Cálculo de etiqueta normativa: primer dígito del Plan de Cuentas Homologado.
      clase_contable = dplyr::case_when(
        substr(cuenta_contable, 1, 1) == "1" ~ "Activo",
        substr(cuenta_contable, 1, 1) == "2" ~ "Pasivo",
        substr(cuenta_contable, 1, 1) == "3" ~ "Patrimonio",
        substr(cuenta_contable, 1, 1) == "6" ~ "Contingente",
# Un código desconocido queda marcado, para revisión explícita posterior.
        TRUE ~ "Revisar"
      ),
# Clasificación de uso analítico: distingue qué tipo de activo/pasivo es cada cuenta.
      rubro_liquidez = dplyr::case_when(
# Disponibilidades: caja y bóvedas; saldo existente, no cobro futuro.
        prefijo == "111" ~ "Caja y bóvedas",
# Encaje: reservas registradas en BCCR, potencialmente sujetas a exigencias.
        prefijo == "112" ~ "Encaje en el BCCR",
# Estas familias agrupan inversiones negociables; su venta no se presume.
        prefijo %in% c("121", "122", "123") ~ "Inversiones",
# Esta familia se identifica como restringida aun si posee fecha de vencimiento.
        prefijo == "125" ~ "Inversiones restringidas",
# Cartera: derechos de cobro, con posible riesgo de mora.
        prefijo %in% c("131", "132") ~ "Cartera de crédito",
# Activo real no financiero: no representa entrada contractual en este modelo.
        prefijo == "171" ~ "Terrenos",
# Obligaciones a la vista: exigibles, aunque sus retiros observados sean parciales.
        prefijo == "211" ~ "Depósitos a la vista",
# Depósitos pactados a plazo: puede modelarse renovación del principal.
        prefijo == "213" ~ "Depósitos a plazo",
# Financiamiento obtenido: su amortización genera salida contractual.
        prefijo %in% c("222", "232") ~ "Financiamiento recibido",
        prefijo == "242" ~ "Otras cuentas por pagar",
        prefijo == "311" ~ "Capital y patrimonio",
# Compromisos no desembolsados a clientes: contingencia, no pasivo corriente.
        prefijo == "615" ~ "Líneas concedidas no utilizadas",
        TRUE ~ "Revisar"
      ),
# Descripción interpretativa; no reemplaza cálculos ni altera montos.
      tratamiento_general = dplyr::case_when(
# Tener caja al inicio no la convierte en una 'entrada' adicional del calendario.
        rubro_liquidez == "Caja y bóvedas" ~ "Disponible al corte; no genera cobro futuro",
# Un encaje contable no se trata automáticamente como caja de libre disposición.
        rubro_liquidez == "Encaje en el BCCR" ~ "Reserva sujeta a restricciones; no presumir uso libre",
# Se reconoce el cobro al vencimiento; liquidar antes exige otro módulo.
        rubro_liquidez == "Inversiones" ~ "Cobro contractual; venta anticipada se analiza aparte",
# La restricción se analiza junto con fecha_liberacion en preparar_flujos().
        rubro_liquidez == "Inversiones restringidas" ~ "Cobro sujeto a verificación de liberación",
# La mora y la tasa de cobro se aplican después, en el modelo de flujos.
        rubro_liquidez == "Cartera de crédito" ~ "Cobro contractual sujeto a mora y recuperación",
        rubro_liquidez == "Terrenos" ~ "Activo sin cobro contractual en este ejercicio",
# La diferencia contrato/comportamiento se modela posteriormente en 30 días.
        rubro_liquidez == "Depósitos a la vista" ~ "Salida exigible; ajuste histórico de retiros",
        rubro_liquidez == "Depósitos a plazo" ~ "Salida contractual; ajuste histórico de renovaciones",
        rubro_liquidez %in% c("Financiamiento recibido", "Otras cuentas por pagar") ~ "Salida contractual",
# El patrimonio se muestra en estructura, pero no se vuelve pago del calendario.
        rubro_liquidez == "Capital y patrimonio" ~ "No representa una salida contractual en el ejercicio",
        rubro_liquidez == "Líneas concedidas no utilizadas" ~ "Contingencia; posible desembolso estimado",
        TRUE ~ "Requiere criterio adicional"
      )
    ) |>
# Se descarta prefijo por ser auxiliar; la clasificación permanece disponible.
    dplyr::select(-prefijo)
}

# Función: preparar_saldos(datos_contables, catalogo_clasificado)
# Rol: construir posiciones de balance SIN duplicar saldos por su calendario de pagos.
# Entrada: datos_contables preparados por app.R y catálogo enriquecido.
# Salida: una fila por id_registro con su clasificación, días de vencimiento,
# banda_vencimiento y etiqueta cualitativa de disponibilidad.
# Importante: dato contable (stock) y pago contractual (flujo) son magnitudes distintas.
# Si una operación tiene varios pagos, un left_join podría repetir su saldo;
# por eso se verifica la consistencia antes de hacer distinct(id_registro).
preparar_saldos <- function(datos_contables, catalogo_clasificado) {
  # El app.R original ya unió pagos a los saldos. Se recupera una fila por ID
  # para no contar más de una vez el mismo activo o pasivo al sumar saldos.
# Control de integridad: una misma operación no debería tener cuentas/saldos contradictorios.
  inconsistentes <- datos_contables |>
# Los grupos se definen por identificador de posición, no por moneda ni rubro.
    dplyr::group_by(id_registro) |>
# n_distinct cuenta valores diferentes; se ignoran las repeticiones del mismo saldo.
    dplyr::summarise(n_saldos = dplyr::n_distinct(saldo_contable),
                     n_cuentas = dplyr::n_distinct(cuenta_contable), .groups = "drop") |>
# Nos quedamos exclusivamente con identificadores potencialmente inconsistentes.
    dplyr::filter(n_saldos > 1 | n_cuentas > 1)
# Si aparece una contradicción se detiene todo en lugar de agregar incorrectamente.
  if (nrow(inconsistentes) > 0) {
    stop("Hay ID_REGISTRO con saldos o cuentas diferentes; revisar antes de agregar.")
  }
# Una fila por operación para sumar posiciones de balance sin duplicarlas.
  saldos <- datos_contables |>
    dplyr::distinct(id_registro, .keep_all = TRUE) |>
# La cuenta se pasa a texto para empatar con el catálogo clasificado.
    dplyr::mutate(cuenta_contable = as.character(cuenta_contable),
                  alias = as.character(alias), moneda = as.character(moneda),
                  periodo = as.Date(periodo)) |>
# Evita columnas _x/_y si el modelo se ejecuta sobre datos ya enriquecidos.
    dplyr::select(-dplyr::any_of(c("clase_contable", "rubro_liquidez", "tratamiento_general"))) |>
# Relación muchos registros -> una cuenta: añade etiquetas sin alterar saldos.
    dplyr::left_join(
      catalogo_clasificado |>
        dplyr::select(cuenta_contable, denominacion_oficial,
                      clase_contable, rubro_liquidez, tratamiento_general),
      by = "cuenta_contable"
    )
# La clasificación es indispensable para decidir si un pago es entrada o salida.
  if (anyNA(saldos$clase_contable) || any(saldos$clase_contable == "Revisar")) {
    stop("Existen cuentas contables sin clasificación. Revisar catálogo.")
  }
# Verificación adicional de unicidad de la tabla de posiciones.
  if (anyDuplicated(saldos$id_registro)) stop("Los saldos tienen ID duplicados.")
  saldos |>
    dplyr::mutate(
# Resta de fechas: días que faltan hasta el vencimiento a partir del corte.
      dias_vencimiento = as.integer(as.Date(fecha_vencimiento) - periodo),
# Banda de la POSICIÓN CONTABLE, no banda de sus pagos: no sumarlas como flujos.
      banda_vencimiento = clasificar_banda(dias_vencimiento),
# Etiquetas cualitativas; el orden de case_when establece prioridad de criterios.
      disponibilidad = dplyr::case_when(
# La caja existe desde el corte: su uso corresponde a cobertura, no a entradas.
        rubro_liquidez == "Caja y bóvedas" ~ "Inmediata",
# El encaje se informa por separado del efectivo libre.
        rubro_liquidez == "Encaje en el BCCR" ~ "Encaje; no libre automáticamente",
# La cuenta misma se identifica como inversión restringida.
        rubro_liquidez == "Inversiones restringidas" ~ "Restringida",
# Restricción vigente si la liberación es posterior al cierre.
        !is.na(fecha_liberacion) & as.Date(fecha_liberacion) > periodo ~ "Restringida hasta liberación",
# Un compromiso positivo y no vencido puede limitar el uso del activo.
        !is.na(saldo_comprometido) & saldo_comprometido > 0 &
          (is.na(fecha_fin_compromiso) |
             as.Date(fecha_fin_compromiso) > periodo) ~ "Compromiso parcial o total",
# Negociable no equivale a vendido: todavía faltan precio, descuentos y costos.
        rubro_liquidez == "Inversiones" & as.character(negociacion_disponible) == "Sí" ~ "Venta potencial; no efectivo inmediato",
        rubro_liquidez == "Inversiones" ~ "Venta no acreditada",
# Cobrar créditos depende del calendario y del comportamiento del deudor.
        rubro_liquidez == "Cartera de crédito" ~ "Según cobros contractuales",
# Etiqueta residual, deliberadamente prudente para rubros sin regla específica.
        TRUE ~ "No es disponibilidad inmediata"
      )
    )
}

# Función: calcular_tasas(datos_observados)
# Rol: cuantificar comportamiento histórico, separado de los vencimientos contractuales.
# La hoja Observaciones contiene estadísticas de los 30 días PREVIOS a cada corte.
# Se producen tasas por (periodo, entidad, moneda); no se agregan monedas distintas.
# Fórmulas:
# retiro = retiros_vista_30d / saldo_vista_inicio_30d
# renovación = principal_renovado_30d / vencimientos_plazo_30d
# cobro = cobros_recibidos_30d / cobros_programados_30d
# uso de líneas = desembolsos_lineas_30d / limite_lineas_otorgadas_inicio_30d
# Se acotan a [0,1]. Si falta información, la tasa queda NA, no cero.
calcular_tasas <- function(datos_observados) {
  # Cada observación describe los 30 días anteriores al corte respectivo.
  # No se utilizan observaciones de meses posteriores al corte.
  datos_observados |>
# Se unifican tipos para que los joins por cierre, alias y moneda sean consistentes.
    dplyr::mutate(periodo = as.Date(periodo), alias = as.character(alias),
                  moneda = as.character(moneda),
# Mide el retiro observado respecto del saldo inicial a la vista.
                  tasa_retiro = tasa_acotada(retiros_vista_30d, saldo_vista_inicio_30d),
# Mide qué parte del principal que venció fue renovada por clientes.
                  tasa_renovacion = tasa_acotada(principal_renovado_30d, vencimientos_plazo_30d),
# Mide cuánto de los cobros previstos terminó ingresando efectivamente.
                  tasa_cobro = tasa_acotada(cobros_recibidos_30d, cobros_programados_30d),
# Mide desembolsos observados respecto del límite de líneas otorgadas.
                  tasa_uso_lineas = tasa_acotada(desembolsos_lineas_30d,
                                                limite_lineas_otorgadas_inicio_30d)) |>
# Se conservan únicamente variables necesarias para ajustar y auditar.
    dplyr::select(periodo, alias, moneda, tasa_retiro, tasa_renovacion,
                  tasa_cobro, tasa_uso_lineas,
# Líneas recibidas: información para un módulo posterior de COBERTURA; no se suman como entrada.
                  linea_recibida_no_dispuesta, fecha_disponible_linea_recibida,
                  fecha_fin_linea_recibida)
}

# Función: preparar_flujos(datos_contrato, saldos, tasas)
# Rol: convertir calendario contractual en entradas/salidas por operación y
# calcular una versión académica ajustada por comportamiento a 30 días.
# Entrada: pagos contractuales, saldos clasificados y tasas históricas por corte.
# Salida: registros detallados de pagos y desembolsos contingentes estimados.
# Separación esencial: monto_capital e interés SON flujos; saldo_contable NO se añade.
# En el escenario ajustado: se corrigen cobros por tasa de recuperación, depósitos
# a la vista por retiros y depósitos a plazo por renovación del principal; se trata
# de forma conservadora la mora y la restricción de inversiones. Las líneas
# CONCEDIDAS añaden una posible salida no contractual; las líneas RECIBIDAS no.
preparar_flujos <- function(datos_contrato, saldos, tasas) {
# Metadatos identifican quién recibe/paga y qué condiciones afectan cada flujo.
  metadatos <- saldos |>
    dplyr::select(id_registro, alias, moneda, cuenta_contable,
                  denominacion_oficial, clase_contable, rubro_liquidez,
                  fecha_liberacion, fecha_primer_pago_impago)

# Se parte del calendario de pagos, no de la tabla de saldos, para no duplicar principal.
  pagos <- datos_contrato |>
# Formatos compatibles para fecha de corte, fecha de pago e identificador.
    dplyr::mutate(id_registro = as.character(id_registro),
                  periodo = as.Date(periodo),
                  fecha_pago_contractual = as.Date(fecha_pago_contractual)) |>
# Agrega clasificación contable; un registro de pago debe tener su posición.
    dplyr::left_join(metadatos, by = "id_registro") |>
# Agrega tasas históricas del MISMO cierre y moneda, evitando fuga temporal.
    dplyr::left_join(tasas, by = c("periodo", "alias", "moneda"))
# Un pago sin clasificación impediría decidir si es entrada o salida.
  if (anyNA(pagos$clase_contable)) stop("Hay pagos sin registro contable vinculado.")
# Este calendario no está diseñado para pagos de patrimonio ni contingencias.
  if (any(pagos$clase_contable %in% c("Patrimonio", "Contingente"))) {
    stop("Se detectaron pagos contractuales sobre cuentas no previstas como flujos.")
  }

# Se calculan montos por operación. Esta tabla luego puede agregarse por banda.
  pagos <- pagos |>
    dplyr::mutate(
# Días residuales: 0 y negativos indican pagos exigibles/vencidos al cierre.
      dias = as.integer(fecha_pago_contractual - periodo),
# Activo contractual: entrada; pasivo contractual: salida, según el catálogo.
      sentido = dplyr::if_else(clase_contable == "Activo", "Entrada", "Salida"),
# Etiqueta adicional, independiente de la banda que finalmente recibe el flujo.
      estado_plazo = dplyr::case_when(
# Una entrada pasada NO implica que se haya cobrado; no se proyecta automáticamente.
        dias <= 0 & sentido == "Entrada" ~ "Cobro vencido/no confirmado",
# Una salida vencida puede ser exigida ahora; se mantiene como necesidad inmediata.
        dias <= 0 & sentido == "Salida" ~ "Salida exigible al corte",
        TRUE ~ "Futuro"
      ),
      # Por prudencia, las salidas exigibles se presentan en la primera banda.
# La salida ya exigible se coloca en 1-8; los cobros vencidos quedan fuera de seis bandas.
      banda = dplyr::if_else(dias <= 0 & sentido == "Salida",
                             "1-8", clasificar_banda(dias)),
# Flujo contractual positivo: capital más interés, en moneda ORIGINAL.
      efectivo_contractual = pmax(0, monto_capital) + pmax(0, monto_interes),
# El ajuste de tasas observadas se limita a fechas FUTURAS dentro de 30 días.
      horizonte_30d = dias <= 30 & dias > 0,
# Sin tasa de cobro se usa 100% contractual: es un respaldo académico explícito.
      ajuste_cobro = dplyr::if_else(!is.na(tasa_cobro), tasa_cobro, 1),
# Sin tasa de retiro se usa 100% contractual, no tasa cero.
      ajuste_retiro = dplyr::if_else(!is.na(tasa_retiro), tasa_retiro, 1),
# Sin tasa de renovación se usa 0%, suponiendo que no se renueva principal.
      ajuste_renovacion = dplyr::if_else(!is.na(tasa_renovacion), tasa_renovacion, 0),
# Se marca una inversión restringida cuya liberación llega DESPUÉS del pago.
      restriccion_cobro = rubro_liquidez == "Inversiones restringidas" &
        !is.na(fecha_liberacion) &
        as.Date(fecha_liberacion) > fecha_pago_contractual,
# Se considera 'en mora' si hay alguna fecha de primer impago no faltante.
      mora = rubro_liquidez == "Cartera de crédito" &
        !is.na(fecha_primer_pago_impago),
# case_when aplica la primera regla verdadera: el orden ES financiero, no decorativo.
      efectivo_ajustado = dplyr::case_when(
# La entrada ajustada restringida se lleva a cero, sin postergarla automáticamente.
        restriccion_cobro ~ 0,
# Por prudencia, toda operación marcada como morosa tiene cobro ajustado cero.
        mora ~ 0,
# Cobro futuro de cartera: monto contractual multiplicado por tasa histórica.
        horizonte_30d & rubro_liquidez == "Cartera de crédito" ~
          efectivo_contractual * ajuste_cobro,
# Depósitos vista: aproxima salida esperada en horizonte 30d con tasa retiro.
        horizonte_30d & rubro_liquidez == "Depósitos a la vista" ~
          efectivo_contractual * ajuste_retiro,
# Depósitos plazo: se paga principal NO renovado + interés completo.
        horizonte_30d & rubro_liquidez == "Depósitos a plazo" ~
          monto_capital * (1 - ajuste_renovacion) + monto_interes,
# Fuera de esas condiciones permanece el valor contractual sin ajuste.
        TRUE ~ efectivo_contractual
      ),
# Identifica el origen del renglón, necesario al mezclar pagos y líneas proyectadas.
      tipo_flujo = "Pago contractual",
# Motivo textual de cada decisión: la trazabilidad llega hasta la tabla de detalle.
      nota_ajuste = dplyr::case_when(
# La prioridad de mensajes no siempre es la misma que la exclusión temporal de bandas.
        mora ~ "Crédito con mora: entrada no reconocida en escenario ajustado",
        restriccion_cobro ~ "Inversión restringida al pago: entrada no utilizable",
        dias <= 0 & sentido == "Entrada" ~ "Cobro vencido: fuera de las seis bandas",
        horizonte_30d & rubro_liquidez == "Cartera de crédito" ~ "Recuperación histórica 30 días",
        horizonte_30d & rubro_liquidez == "Depósitos a la vista" ~ "Retiro histórico 30 días",
        horizonte_30d & rubro_liquidez == "Depósitos a plazo" ~ "Renovación histórica 30 días",
        TRUE ~ "Sin ajuste; contrato o plazo superior a 30 días"
      )
    )

# Orden y selección de columnas antes de anexar las contingencias.
  columnas <- c("id_registro", "periodo", "alias", "moneda", "cuenta_contable",
                "denominacion_oficial", "clase_contable", "rubro_liquidez",
                "fecha_pago_contractual", "dias", "banda", "estado_plazo",
                "tipo_flujo", "sentido", "monto_capital", "monto_interes",
                "efectivo_contractual", "efectivo_ajustado", "nota_ajuste")
# all_of exige que todas las columnas existan: evita errores silenciosos.
  pagos <- dplyr::select(pagos, dplyr::all_of(columnas))

  # Las líneas otorgadas son contingencias: no se confunden con el pasivo.
  # Solo se estima un posible uso durante los próximos 30 días.
# Construcción de flujos NO CONTRACTUALES a partir de líneas CONCEDIDAS.
  lineas <- saldos |>
# Solo líneas pendientes de disponer; no son pasivos exigibles ya desembolsados.
    dplyr::filter(clase_contable == "Contingente", saldo_no_utilizado > 0) |>
# Se utiliza la tasa de líneas de la misma entidad/corte/moneda.
    dplyr::left_join(tasas, by = c("periodo", "alias", "moneda")) |>
# transmute selecciona y crea columnas con la misma estructura que pagos.
    dplyr::transmute(
      id_registro, periodo, alias, moneda, cuenta_contable,
      denominacion_oficial, clase_contable, rubro_liquidez,
# Día 1 = hipótesis conservadora de oportunidad, NO fecha observada de desembolso.
      fecha_pago_contractual = periodo + 1,
# La proyección contingente se ubica en la primera banda (1-8).
      dias = 1L, banda = "1-8", estado_plazo = "Proyección no contractual",
      tipo_flujo = "Utilización estimada de línea concedida",
# Es una SALIDA potencial; monto_capital/interes se dejan en cero porque no es un pago contractual.
      sentido = "Salida", monto_capital = 0, monto_interes = 0,
# Flujo contractual igual a cero: la línea no está desembolsada por contrato.
      efectivo_contractual = 0,
# Proyección ajustada = saldo no usado × tasa histórica de utilización.
      efectivo_ajustado = dplyr::if_else(
        is.na(tasa_uso_lineas), 0, saldo_no_utilizado * tasa_uso_lineas),
      nota_ajuste = dplyr::if_else(
# Si la tasa es NA se utiliza cero solo como respaldo técnico; la nota advierte el faltante.
        is.na(tasa_uso_lineas),
        "Sin tasa observable: se reporta aparte y no se imputa flujo",
        "Supuesto: desembolso estimado el día 1 con tasa histórica 30 días")
    )
# Se apilan pagos y estimaciones con columnas compatibles; no se alteran saldos.
  dplyr::bind_rows(pagos, lineas) |>
# Banda como texto para agregación, comparación y exportación posterior.
    dplyr::mutate(banda = as.character(banda))
}

# Función: resumir_bandas(flujos)
# Rol: calcular brechas monetarias por plazo; núcleo cuantitativo del apartado 2.3.
# Definiciones por banda i: B_i = entradas_i - salidas_i.
# Acumulada hasta banda k = suma de B_1 a B_k, sin caja inicial ni coberturas.
# No se suman cierres ni monedas: cada serie pertenece a una entidad, moneda y corte.
# Entradas/salidas 'contrato' y 'ajustadas' se mantienen en columnas separadas.
# Una brecha negativa NO indica por sí sola insolvencia: faltan las medidas de cobertura.
resumir_bandas <- function(flujos) {
  # La brecha no utiliza saldos de caja ni líneas recibidas como entradas.
  flujos |>
# Solo las seis bandas FUTURAS; cobros vencidos/sin fecha se muestran por separado.
    dplyr::filter(banda %in% bandas_liquidez) |>
# Cada resultado tiene claves de periodo, entidad, moneda y banda temporal.
    dplyr::group_by(periodo, alias, moneda, banda) |>
# summarise calcula cuatro sumas monetarias independientes por banda.
    dplyr::summarise(
# Filtra conceptualmente entradas sin borrar salidas de la tabla: las sustituye por cero.
      entradas_contrato = sum(dplyr::if_else(sentido == "Entrada", efectivo_contractual, 0), na.rm = TRUE),
# Idéntica suma para obligaciones de salida contractuales.
      salidas_contrato = sum(dplyr::if_else(sentido == "Salida", efectivo_contractual, 0), na.rm = TRUE),
# Entradas después de aplicar las reglas académicas de ajuste.
      entradas_ajustadas = sum(dplyr::if_else(sentido == "Entrada", efectivo_ajustado, 0), na.rm = TRUE),
# Salidas incluyendo uso estimado de líneas y ajustes de depósitos.
      salidas_ajustadas = sum(dplyr::if_else(sentido == "Salida", efectivo_ajustado, 0), na.rm = TRUE),
      .groups = "drop"
    ) |>
# Genera bandas sin movimientos para preservar una serie completa de seis intervalos.
    tidyr::complete(tidyr::nesting(periodo, alias, moneda),
                    banda = bandas_liquidez,
# Una banda sin pagos tiene flujo cero; no significa que falten datos en la base.
                    fill = list(entradas_contrato = 0, salidas_contrato = 0,
                                entradas_ajustadas = 0, salidas_ajustadas = 0)) |>
# El factor ordenado garantiza el orden correcto antes de sumar acumulados.
    dplyr::mutate(banda = factor(banda, levels = bandas_liquidez, ordered = TRUE)) |>
# Orden crucial: cumsum debe recorrer 1-8, 9-15, ..., 181+.
    dplyr::arrange(periodo, alias, moneda, banda) |>
# Reinicia la acumulación para cada combinación entidad/fecha/moneda.
    dplyr::group_by(periodo, alias, moneda) |>
    dplyr::mutate(
# Brecha contractual por banda: E - S.
      brecha_contrato = entradas_contrato - salidas_contrato,
# Brecha de comportamiento por banda: E_ajustada - S_ajustada.
      brecha_ajustada = entradas_ajustadas - salidas_ajustadas,
# cumsum suma las brechas desde la primera banda hasta la banda actual.
      acumulada_contrato = cumsum(brecha_contrato),
# También se acumula la serie ajustada de forma independiente.
      acumulada_ajustada = cumsum(brecha_ajustada)
    ) |>
# Se quita el agrupamiento para no condicionar operaciones posteriores.
    dplyr::ungroup()
}

# Función: resumir_rubros(flujos)
# Rol: permitir rastrear qué CUENTAS originan la entrada/salida de cada banda.
# Agrupa pagos de una misma cuenta sin perder sentido (Entrada o Salida).
# El resultado alimenta el detalle de flujos y las columnas de la tabla maestra.
resumir_rubros <- function(flujos) {
  flujos |>
# Se excluyen registros vencidos y sin banda futura del resumen agregado.
    dplyr::filter(banda %in% bandas_liquidez) |>
# Se agrupa por clave financiera y cuenta, no por ID individual de pago.
    dplyr::group_by(periodo, alias, moneda, banda, cuenta_contable,
                    denominacion_oficial, clase_contable, rubro_liquidez, sentido) |>
# Suma contractual por rubro; la suma ajustada queda aparte.
    dplyr::summarise(contractual = sum(efectivo_contractual, na.rm = TRUE),
                     ajustado = sum(efectivo_ajustado, na.rm = TRUE), .groups = "drop")
}

# Función: resumir_estructura(saldos)
# Rol: composición de ACTIVOS/PASIVOS/PATRIMONIO/CONTINGENTES desde saldos al corte.
# No utiliza calendario de pagos ni tasas de comportamiento.
# Cada grupo permite conocer saldo registrado y número de operaciones.
resumir_estructura <- function(saldos) {
  saldos |>
# La disponibilidad se conserva como dimensión para explorar restricciones.
    dplyr::group_by(periodo, alias, moneda, clase_contable,
                    cuenta_contable, denominacion_oficial, rubro_liquidez,
                    disponibilidad) |>
# Stock contable a fecha de corte; sumarlo entre fechas sería incorrecto.
    dplyr::summarise(saldo = sum(saldo_contable, na.rm = TRUE),
# n cuenta cuántos registros contables integran el agregado.
                     operaciones = dplyr::n(), .groups = "drop")
}

# Función: resumir_vencimientos(saldos)
# Rol: distribución de POSICIONES según su fecha_vencimiento y disponibilidad.
# ATENCIÓN: el saldo completo ubicado en la banda de vencimiento NO es un flujo:
# los pagos efectivos se calculan con datos_contrato en preparar_flujos().
# Esta vista responde 'qué saldo vence', distinta de 'cuánto se cobrará ese día'.
resumir_vencimientos <- function(saldos) {
  saldos |>
# Separa entidad/corte/moneda/cuenta/banda y condición de disponibilidad.
    dplyr::group_by(periodo, alias, moneda, clase_contable, rubro_liquidez,
                    cuenta_contable, denominacion_oficial, banda_vencimiento,
                    disponibilidad) |>
# Suma valores de balance y cuenta las operaciones por categoría.
    dplyr::summarise(saldo = sum(saldo_contable, na.rm = TRUE),
                     operaciones = dplyr::n(), .groups = "drop")
}

# Función: crear_tabla_maestra(estructura, rubros)
# Rol: combinar, para consulta, UN saldo por cuenta/corte con sus seis bandas de flujos.
# Primero se resume estructura por cuenta; luego cada banda pasa a ser un grupo
# de columnas mediante pivot_wider. La unión se hace por claves de cada cuenta.
# Se evita repetir el saldo original una vez por cada banda temporal.
# Los NA de FLUJOS luego de la unión significan 'sin movimientos' y se vuelven cero;
# NO equivalen a imputar un saldo contable faltante.
crear_tabla_maestra <- function(estructura, rubros) {
  # Una fila por cuenta y corte: el saldo aparece una sola vez.
  # Las seis bandas se convierten en columnas, evitando duplicar saldos.
# Tabla base: una cuenta analítica por entidad, periodo y moneda.
  base <- estructura |>
# Se eliminan las divisiones por disponibilidad solo para consolidar el saldo.
    dplyr::group_by(periodo, alias, moneda, cuenta_contable,
                    denominacion_oficial, clase_contable, rubro_liquidez) |>
# Agrega saldo y número de operaciones; no incorpora efectivo estimado.
    dplyr::summarise(saldo = sum(saldo), operaciones = sum(operaciones),
# Concatena las etiquetas distintas para conservar información de restricciones.
      disponibilidad = paste(sort(unique(disponibilidad)), collapse = "; "),
      .groups = "drop")
# Tabla de flujos: se organiza por cuenta y por banda antes de pasar a formato ancho.
  matriz <- rubros |>
    dplyr::mutate(
# '1-8' pasa a 'd1_8' etc. Este sufijo distingue columnas temporales.
      banda_codigo = paste0("d", gsub("[+-]", "_", banda)),
# Se separan entradas y salidas para sumar cada signo de flujo por separado.
      entradas_contrato = dplyr::if_else(sentido == "Entrada", contractual, 0),
      salidas_contrato = dplyr::if_else(sentido == "Salida", contractual, 0),
# Misma separación, aplicada a la variante ajustada.
      entradas_ajustadas = dplyr::if_else(sentido == "Entrada", ajustado, 0),
      salidas_ajustadas = dplyr::if_else(sentido == "Salida", ajustado, 0)
    ) |>
# Identifica inequívocamente una cuenta en una banda/cierre/moneda.
    dplyr::group_by(periodo, alias, moneda, cuenta_contable, banda_codigo) |>
    dplyr::summarise(
# Suma movimientos que comparten cuenta y banda, contractuales y ajustados.
      entradas_contrato = sum(entradas_contrato),
      salidas_contrato = sum(salidas_contrato),
      entradas_ajustadas = sum(entradas_ajustadas),
      salidas_ajustadas = sum(salidas_ajustadas), .groups = "drop") |>
# Brecha por CUENTA y por banda; distinta de la acumulada por entidad.
    dplyr::mutate(
      brecha_contrato = entradas_contrato - salidas_contrato,
      brecha_ajustada = entradas_ajustadas - salidas_ajustadas) |>
# pivot_wider crea columnas, por ejemplo entradas_contrato_d1_8.
    tidyr::pivot_wider(names_from = banda_codigo,
       values_from = c(entradas_contrato, salidas_contrato, brecha_contrato,
                       entradas_ajustadas, salidas_ajustadas, brecha_ajustada),
# Si una celda de una banda no tenía flujos, se completa con cero.
       values_fill = 0)
# Junta tabla de posiciones con columnas de movimientos de las mismas cuentas.
  base |>
    dplyr::left_join(matriz, by = c("periodo", "alias", "moneda", "cuenta_contable")) |>
# Rellena exclusivamente NA en variables de flujos, no en saldos/etiquetas.
    dplyr::mutate(dplyr::across(
      dplyr::starts_with(c("entradas_", "salidas_", "brecha_")),
      ~ tidyr::replace_na(.x, 0)))
}

# Función: crear_modelo_liquidez(datos_contables, datos_contrato, ...)
# Rol: orquestador; ejecuta el cálculo completo en el orden correcto.
# No es una interfaz ni una función reactiva: devuelve una lista normal de tablas.
# app.R la ejecuta una vez al iniciar Shiny; después el servidor SOLO filtra resultados.
# Orden: clasificar -> saldos/tasas -> flujos -> bandas/rubros -> estructura -> tabla.
# Las verificaciones al final son CONTROLES de integridad, no indicadores financieros.
crear_modelo_liquidez <- function(datos_contables, datos_contrato,
                                  datos_observados, datos_catalogo) {
# 1. Clasificación normativa y analítica de las cuentas.
  catalogo <- clasificar_catalogo(datos_catalogo)
# 2. Posiciones únicas con etiquetas de plazo y disponibilidad.
  saldos <- preparar_saldos(datos_contables, catalogo)
# 3. Cuatro tasas históricas por fecha, entidad y moneda.
  tasas <- calcular_tasas(datos_observados)
# 4. Calendario de pagos más utilización estimada de líneas concedidas.
  flujos <- preparar_flujos(datos_contrato, saldos, tasas)
# 5. Flujos agregados y brechas por seis bandas.
  bandas <- resumir_bandas(flujos)
# 6. Desglose de flujos por cuenta contable y banda.
  rubros <- resumir_rubros(flujos)
# 7. Saldos de balance por clase, rubro y condición.
  estructura <- resumir_estructura(saldos)
# 8. Distribución de posiciones contables por vencimiento.
  vencimientos <- resumir_vencimientos(saldos)
# 9. Junta saldos y matriz de flujos en una sola consulta por cuenta.
  tabla_maestra <- crear_tabla_maestra(estructura, rubros)
# 10. Tablero mínimo de verificaciones: muestra cantidades, no afirma validez regulatoria.
  verificaciones <- tibble::tibble(
    prueba = c("Cuentas sin clase", "IDs contables duplicados tras preparar saldos",
               "Pagos sin vínculo", "Registros contables conservados",
               "Pagos contractuales conservados"),
# Primeras pruebas: clases sin definir, ID duplicados y pagos sin relación.
    resultado = c(sum(catalogo$clase_contable == "Revisar"),
                  sum(duplicated(saldos$id_registro)),
                  sum(!as.character(datos_contrato$id_registro) %in% saldos$id_registro),
# Últimas cifras: número de operaciones y de pagos conservados.
                  nrow(saldos),
                  sum(flujos$tipo_flujo == "Pago contractual"))
  )
# La lista tiene nombres estables; Shiny extrae cada resultado mediante modelo$nombre.
  list(catalogo = catalogo, saldos = saldos, tasas = tasas, flujos = flujos,
       bandas = bandas, rubros = rubros, estructura = estructura,
       vencimientos = vencimientos, tabla_maestra = tabla_maestra,
       verificaciones = verificaciones)
}
