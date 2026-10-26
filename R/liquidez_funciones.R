# Funciones del módulo 2.1–2.3. No modifican los objetos de origen.
# Cada fecha de corte y moneda se analiza por separado.
# Fuente para las clases: CONASSIF 6-18, art. 26:
# https://www.sugef.fi.cr/normativa/NormativaTransversal.aspx
# Las ponderaciones de comportamiento y los plazos son supuestos académicos.

bandas_liquidez <- c("1-8", "9-15", "16-30", "31-89", "90-180", "181+")

clasificar_banda <- function(dias) {
  dplyr::case_when(
    is.na(dias) ~ "Sin fecha",
    dias <= 0 ~ "Vencido/exigible",
    dias <= 8 ~ "1-8",
    dias <= 15 ~ "9-15",
    dias <= 30 ~ "16-30",
    dias <= 89 ~ "31-89",
    dias <= 180 ~ "90-180",
    TRUE ~ "181+"
  )
}

tasa_acotada <- function(numerador, denominador) {
  dplyr::if_else(!is.na(denominador) & denominador > 0,
                 pmin(1, pmax(0, numerador / denominador)), NA_real_)
}

clasificar_catalogo <- function(datos_catalogo) {
  datos_catalogo |>
    dplyr::mutate(
      cuenta_contable = as.character(cuenta_contable),
      prefijo = substr(cuenta_contable, 1, 3),
      clase_contable = dplyr::case_when(
        substr(cuenta_contable, 1, 1) == "1" ~ "Activo",
        substr(cuenta_contable, 1, 1) == "2" ~ "Pasivo",
        substr(cuenta_contable, 1, 1) == "3" ~ "Patrimonio",
        substr(cuenta_contable, 1, 1) == "6" ~ "Contingente",
        TRUE ~ "Revisar"
      ),
      rubro_liquidez = dplyr::case_when(
        prefijo == "111" ~ "Caja y bóvedas",
        prefijo == "112" ~ "Encaje en el BCCR",
        prefijo %in% c("121", "122", "123") ~ "Inversiones",
        prefijo == "125" ~ "Inversiones restringidas",
        prefijo %in% c("131", "132") ~ "Cartera de crédito",
        prefijo == "171" ~ "Terrenos",
        prefijo == "211" ~ "Depósitos a la vista",
        prefijo == "213" ~ "Depósitos a plazo",
        prefijo %in% c("222", "232") ~ "Financiamiento recibido",
        prefijo == "242" ~ "Otras cuentas por pagar",
        prefijo == "311" ~ "Capital y patrimonio",
        prefijo == "615" ~ "Líneas concedidas no utilizadas",
        TRUE ~ "Revisar"
      ),
      tratamiento_general = dplyr::case_when(
        rubro_liquidez == "Caja y bóvedas" ~ "Disponible al corte; no genera cobro futuro",
        rubro_liquidez == "Encaje en el BCCR" ~ "Reserva sujeta a restricciones; no presumir uso libre",
        rubro_liquidez == "Inversiones" ~ "Cobro contractual; venta anticipada se analiza aparte",
        rubro_liquidez == "Inversiones restringidas" ~ "Cobro sujeto a verificación de liberación",
        rubro_liquidez == "Cartera de crédito" ~ "Cobro contractual sujeto a mora y recuperación",
        rubro_liquidez == "Terrenos" ~ "Activo sin cobro contractual en este ejercicio",
        rubro_liquidez == "Depósitos a la vista" ~ "Salida exigible; ajuste histórico de retiros",
        rubro_liquidez == "Depósitos a plazo" ~ "Salida contractual; ajuste histórico de renovaciones",
        rubro_liquidez %in% c("Financiamiento recibido", "Otras cuentas por pagar") ~ "Salida contractual",
        rubro_liquidez == "Capital y patrimonio" ~ "No representa una salida contractual en el ejercicio",
        rubro_liquidez == "Líneas concedidas no utilizadas" ~ "Contingencia; posible desembolso estimado",
        TRUE ~ "Requiere criterio adicional"
      )
    ) |>
    dplyr::select(-prefijo)
}

preparar_saldos <- function(datos_contables, catalogo_clasificado) {
  # El app.R original ya unió pagos a los saldos. Se recupera una fila por ID
  # para no contar más de una vez el mismo activo o pasivo al sumar saldos.
  inconsistentes <- datos_contables |>
    dplyr::group_by(id_registro) |>
    dplyr::summarise(n_saldos = dplyr::n_distinct(saldo_contable),
                     n_cuentas = dplyr::n_distinct(cuenta_contable), .groups = "drop") |>
    dplyr::filter(n_saldos > 1 | n_cuentas > 1)
  if (nrow(inconsistentes) > 0) {
    stop("Hay ID_REGISTRO con saldos o cuentas diferentes; revisar antes de agregar.")
  }
  saldos <- datos_contables |>
    dplyr::distinct(id_registro, .keep_all = TRUE) |>
    dplyr::mutate(cuenta_contable = as.character(cuenta_contable),
                  alias = as.character(alias), moneda = as.character(moneda),
                  periodo = as.Date(periodo)) |>
    dplyr::select(-dplyr::any_of(c("clase_contable", "rubro_liquidez", "tratamiento_general"))) |>
    dplyr::left_join(
      catalogo_clasificado |>
        dplyr::select(cuenta_contable, denominacion_oficial,
                      clase_contable, rubro_liquidez, tratamiento_general),
      by = "cuenta_contable"
    )
  if (anyNA(saldos$clase_contable) || any(saldos$clase_contable == "Revisar")) {
    stop("Existen cuentas contables sin clasificación. Revisar catálogo.")
  }
  if (anyDuplicated(saldos$id_registro)) stop("Los saldos tienen ID duplicados.")
  saldos |>
    dplyr::mutate(
      dias_vencimiento = as.integer(as.Date(fecha_vencimiento) - periodo),
      banda_vencimiento = clasificar_banda(dias_vencimiento),
      disponibilidad = dplyr::case_when(
        rubro_liquidez == "Caja y bóvedas" ~ "Inmediata",
        rubro_liquidez == "Encaje en el BCCR" ~ "Encaje; no libre automáticamente",
        rubro_liquidez == "Inversiones restringidas" ~ "Restringida",
        !is.na(fecha_liberacion) & as.Date(fecha_liberacion) > periodo ~ "Restringida hasta liberación",
        !is.na(saldo_comprometido) & saldo_comprometido > 0 &
          (is.na(fecha_fin_compromiso) |
             as.Date(fecha_fin_compromiso) > periodo) ~ "Compromiso parcial o total",
        rubro_liquidez == "Inversiones" & as.character(negociacion_disponible) == "Sí" ~ "Venta potencial; no efectivo inmediato",
        rubro_liquidez == "Inversiones" ~ "Venta no acreditada",
        rubro_liquidez == "Cartera de crédito" ~ "Según cobros contractuales",
        TRUE ~ "No es disponibilidad inmediata"
      )
    )
}

calcular_tasas <- function(datos_observados) {
  # Cada observación describe los 30 días anteriores al corte respectivo.
  # No se utilizan observaciones de meses posteriores al corte.
  datos_observados |>
    dplyr::mutate(periodo = as.Date(periodo), alias = as.character(alias),
                  moneda = as.character(moneda),
                  tasa_retiro = tasa_acotada(retiros_vista_30d, saldo_vista_inicio_30d),
                  tasa_renovacion = tasa_acotada(principal_renovado_30d, vencimientos_plazo_30d),
                  tasa_cobro = tasa_acotada(cobros_recibidos_30d, cobros_programados_30d),
                  tasa_uso_lineas = tasa_acotada(desembolsos_lineas_30d,
                                                limite_lineas_otorgadas_inicio_30d)) |>
    dplyr::select(periodo, alias, moneda, tasa_retiro, tasa_renovacion,
                  tasa_cobro, tasa_uso_lineas,
                  linea_recibida_no_dispuesta, fecha_disponible_linea_recibida,
                  fecha_fin_linea_recibida)
}

preparar_flujos <- function(datos_contrato, saldos, tasas) {
  metadatos <- saldos |>
    dplyr::select(id_registro, alias, moneda, cuenta_contable,
                  denominacion_oficial, clase_contable, rubro_liquidez,
                  fecha_liberacion, fecha_primer_pago_impago)

  pagos <- datos_contrato |>
    dplyr::mutate(id_registro = as.character(id_registro),
                  periodo = as.Date(periodo),
                  fecha_pago_contractual = as.Date(fecha_pago_contractual)) |>
    dplyr::left_join(metadatos, by = "id_registro") |>
    dplyr::left_join(tasas, by = c("periodo", "alias", "moneda"))
  if (anyNA(pagos$clase_contable)) stop("Hay pagos sin registro contable vinculado.")
  if (any(pagos$clase_contable %in% c("Patrimonio", "Contingente"))) {
    stop("Se detectaron pagos contractuales sobre cuentas no previstas como flujos.")
  }

  pagos <- pagos |>
    dplyr::mutate(
      dias = as.integer(fecha_pago_contractual - periodo),
      sentido = dplyr::if_else(clase_contable == "Activo", "Entrada", "Salida"),
      estado_plazo = dplyr::case_when(
        dias <= 0 & sentido == "Entrada" ~ "Cobro vencido/no confirmado",
        dias <= 0 & sentido == "Salida" ~ "Salida exigible al corte",
        TRUE ~ "Futuro"
      ),
      # Por prudencia, las salidas exigibles se presentan en la primera banda.
      banda = dplyr::if_else(dias <= 0 & sentido == "Salida",
                             "1-8", clasificar_banda(dias)),
      efectivo_contractual = pmax(0, monto_capital) + pmax(0, monto_interes),
      horizonte_30d = dias <= 30 & dias > 0,
      ajuste_cobro = dplyr::if_else(!is.na(tasa_cobro), tasa_cobro, 1),
      ajuste_retiro = dplyr::if_else(!is.na(tasa_retiro), tasa_retiro, 1),
      ajuste_renovacion = dplyr::if_else(!is.na(tasa_renovacion), tasa_renovacion, 0),
      restriccion_cobro = rubro_liquidez == "Inversiones restringidas" &
        !is.na(fecha_liberacion) &
        as.Date(fecha_liberacion) > fecha_pago_contractual,
      mora = rubro_liquidez == "Cartera de crédito" &
        !is.na(fecha_primer_pago_impago),
      efectivo_ajustado = dplyr::case_when(
        restriccion_cobro ~ 0,
        mora ~ 0,
        horizonte_30d & rubro_liquidez == "Cartera de crédito" ~
          efectivo_contractual * ajuste_cobro,
        horizonte_30d & rubro_liquidez == "Depósitos a la vista" ~
          efectivo_contractual * ajuste_retiro,
        horizonte_30d & rubro_liquidez == "Depósitos a plazo" ~
          monto_capital * (1 - ajuste_renovacion) + monto_interes,
        TRUE ~ efectivo_contractual
      ),
      tipo_flujo = "Pago contractual",
      nota_ajuste = dplyr::case_when(
        mora ~ "Crédito con mora: entrada no reconocida en escenario ajustado",
        restriccion_cobro ~ "Inversión restringida al pago: entrada no utilizable",
        dias <= 0 & sentido == "Entrada" ~ "Cobro vencido: fuera de las seis bandas",
        horizonte_30d & rubro_liquidez == "Cartera de crédito" ~ "Recuperación histórica 30 días",
        horizonte_30d & rubro_liquidez == "Depósitos a la vista" ~ "Retiro histórico 30 días",
        horizonte_30d & rubro_liquidez == "Depósitos a plazo" ~ "Renovación histórica 30 días",
        TRUE ~ "Sin ajuste; contrato o plazo superior a 30 días"
      )
    )

  columnas <- c("id_registro", "periodo", "alias", "moneda", "cuenta_contable",
                "denominacion_oficial", "clase_contable", "rubro_liquidez",
                "fecha_pago_contractual", "dias", "banda", "estado_plazo",
                "tipo_flujo", "sentido", "monto_capital", "monto_interes",
                "efectivo_contractual", "efectivo_ajustado", "nota_ajuste")
  pagos <- dplyr::select(pagos, dplyr::all_of(columnas))

  # Las líneas otorgadas son contingencias: no se confunden con el pasivo.
  # Solo se estima un posible uso durante los próximos 30 días.
  lineas <- saldos |>
    dplyr::filter(clase_contable == "Contingente", saldo_no_utilizado > 0) |>
    dplyr::left_join(tasas, by = c("periodo", "alias", "moneda")) |>
    dplyr::transmute(
      id_registro, periodo, alias, moneda, cuenta_contable,
      denominacion_oficial, clase_contable, rubro_liquidez,
      fecha_pago_contractual = periodo + 1,
      dias = 1L, banda = "1-8", estado_plazo = "Proyección no contractual",
      tipo_flujo = "Utilización estimada de línea concedida",
      sentido = "Salida", monto_capital = 0, monto_interes = 0,
      efectivo_contractual = 0,
      efectivo_ajustado = dplyr::if_else(
        is.na(tasa_uso_lineas), 0, saldo_no_utilizado * tasa_uso_lineas),
      nota_ajuste = dplyr::if_else(
        is.na(tasa_uso_lineas),
        "Sin tasa observable: se reporta aparte y no se imputa flujo",
        "Supuesto: desembolso estimado el día 1 con tasa histórica 30 días")
    )
  dplyr::bind_rows(pagos, lineas) |>
    dplyr::mutate(banda = as.character(banda))
}

resumir_bandas <- function(flujos) {
  # La brecha no utiliza saldos de caja ni líneas recibidas como entradas.
  flujos |>
    dplyr::filter(banda %in% bandas_liquidez) |>
    dplyr::group_by(periodo, alias, moneda, banda) |>
    dplyr::summarise(
      entradas_contrato = sum(dplyr::if_else(sentido == "Entrada", efectivo_contractual, 0), na.rm = TRUE),
      salidas_contrato = sum(dplyr::if_else(sentido == "Salida", efectivo_contractual, 0), na.rm = TRUE),
      entradas_ajustadas = sum(dplyr::if_else(sentido == "Entrada", efectivo_ajustado, 0), na.rm = TRUE),
      salidas_ajustadas = sum(dplyr::if_else(sentido == "Salida", efectivo_ajustado, 0), na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(tidyr::nesting(periodo, alias, moneda),
                    banda = bandas_liquidez,
                    fill = list(entradas_contrato = 0, salidas_contrato = 0,
                                entradas_ajustadas = 0, salidas_ajustadas = 0)) |>
    dplyr::mutate(banda = factor(banda, levels = bandas_liquidez, ordered = TRUE)) |>
    dplyr::arrange(periodo, alias, moneda, banda) |>
    dplyr::group_by(periodo, alias, moneda) |>
    dplyr::mutate(
      brecha_contrato = entradas_contrato - salidas_contrato,
      brecha_ajustada = entradas_ajustadas - salidas_ajustadas,
      acumulada_contrato = cumsum(brecha_contrato),
      acumulada_ajustada = cumsum(brecha_ajustada)
    ) |>
    dplyr::ungroup()
}

resumir_rubros <- function(flujos) {
  flujos |>
    dplyr::filter(banda %in% bandas_liquidez) |>
    dplyr::group_by(periodo, alias, moneda, banda, cuenta_contable,
                    denominacion_oficial, clase_contable, rubro_liquidez, sentido) |>
    dplyr::summarise(contractual = sum(efectivo_contractual, na.rm = TRUE),
                     ajustado = sum(efectivo_ajustado, na.rm = TRUE), .groups = "drop")
}

resumir_estructura <- function(saldos) {
  saldos |>
    dplyr::group_by(periodo, alias, moneda, clase_contable,
                    cuenta_contable, denominacion_oficial, rubro_liquidez,
                    disponibilidad) |>
    dplyr::summarise(saldo = sum(saldo_contable, na.rm = TRUE),
                     operaciones = dplyr::n(), .groups = "drop")
}

resumir_vencimientos <- function(saldos) {
  saldos |>
    dplyr::group_by(periodo, alias, moneda, clase_contable, rubro_liquidez,
                    cuenta_contable, denominacion_oficial, banda_vencimiento,
                    disponibilidad) |>
    dplyr::summarise(saldo = sum(saldo_contable, na.rm = TRUE),
                     operaciones = dplyr::n(), .groups = "drop")
}

crear_tabla_maestra <- function(estructura, rubros) {
  # Una fila por cuenta y corte: el saldo aparece una sola vez.
  # Las seis bandas se convierten en columnas, evitando duplicar saldos.
  base <- estructura |>
    dplyr::group_by(periodo, alias, moneda, cuenta_contable,
                    denominacion_oficial, clase_contable, rubro_liquidez) |>
    dplyr::summarise(saldo = sum(saldo), operaciones = sum(operaciones),
      disponibilidad = paste(sort(unique(disponibilidad)), collapse = "; "),
      .groups = "drop")
  matriz <- rubros |>
    dplyr::mutate(
      banda_codigo = paste0("d", gsub("[+-]", "_", banda)),
      entradas_contrato = dplyr::if_else(sentido == "Entrada", contractual, 0),
      salidas_contrato = dplyr::if_else(sentido == "Salida", contractual, 0),
      entradas_ajustadas = dplyr::if_else(sentido == "Entrada", ajustado, 0),
      salidas_ajustadas = dplyr::if_else(sentido == "Salida", ajustado, 0)
    ) |>
    dplyr::group_by(periodo, alias, moneda, cuenta_contable, banda_codigo) |>
    dplyr::summarise(
      entradas_contrato = sum(entradas_contrato),
      salidas_contrato = sum(salidas_contrato),
      entradas_ajustadas = sum(entradas_ajustadas),
      salidas_ajustadas = sum(salidas_ajustadas), .groups = "drop") |>
    dplyr::mutate(
      brecha_contrato = entradas_contrato - salidas_contrato,
      brecha_ajustada = entradas_ajustadas - salidas_ajustadas) |>
    tidyr::pivot_wider(names_from = banda_codigo,
       values_from = c(entradas_contrato, salidas_contrato, brecha_contrato,
                       entradas_ajustadas, salidas_ajustadas, brecha_ajustada),
       values_fill = 0)
  base |>
    dplyr::left_join(matriz, by = c("periodo", "alias", "moneda", "cuenta_contable")) |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with(c("entradas_", "salidas_", "brecha_")),
      ~ tidyr::replace_na(.x, 0)))
}

crear_modelo_liquidez <- function(datos_contables, datos_contrato,
                                  datos_observados, datos_catalogo) {
  catalogo <- clasificar_catalogo(datos_catalogo)
  saldos <- preparar_saldos(datos_contables, catalogo)
  tasas <- calcular_tasas(datos_observados)
  flujos <- preparar_flujos(datos_contrato, saldos, tasas)
  bandas <- resumir_bandas(flujos)
  rubros <- resumir_rubros(flujos)
  estructura <- resumir_estructura(saldos)
  vencimientos <- resumir_vencimientos(saldos)
  tabla_maestra <- crear_tabla_maestra(estructura, rubros)
  verificaciones <- tibble::tibble(
    prueba = c("Cuentas sin clase", "IDs contables duplicados tras preparar saldos",
               "Pagos sin vínculo", "Registros contables conservados",
               "Pagos contractuales conservados"),
    resultado = c(sum(catalogo$clase_contable == "Revisar"),
                  sum(duplicated(saldos$id_registro)),
                  sum(!as.character(datos_contrato$id_registro) %in% saldos$id_registro),
                  nrow(saldos),
                  sum(flujos$tipo_flujo == "Pago contractual"))
  )
  list(catalogo = catalogo, saldos = saldos, tasas = tasas, flujos = flujos,
       bandas = bandas, rubros = rubros, estructura = estructura,
       vencimientos = vencimientos, tabla_maestra = tabla_maestra,
       verificaciones = verificaciones)
}
