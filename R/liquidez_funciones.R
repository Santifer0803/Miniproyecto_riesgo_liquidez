
# Funciones de los puntos 2, 3, 4 y 5. No modifican los objetos de origen.
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
  dplyr::if_else(!is.na(denominador) &
                   denominador > 0, pmin(1, pmax(0, numerador / denominador)), NA_real_)
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
        rubro_liquidez == "Caja y bóvedas" ~
          "Disponible al corte; no genera cobro futuro",
        rubro_liquidez == "Encaje en el BCCR" ~
          "Reserva sujeta a restricciones; no presumir uso libre",
        rubro_liquidez == "Inversiones" ~
          "Cobro contractual; venta anticipada se analiza aparte",
        rubro_liquidez == "Inversiones restringidas" ~
          "Cobro sujeto a verificación de liberación",
        rubro_liquidez == "Cartera de crédito" ~
          "Cobro contractual sujeto a mora y recuperación",
        rubro_liquidez == "Terrenos" ~
          "Activo sin cobro contractual en este ejercicio",
        rubro_liquidez == "Depósitos a la vista" ~
          "Salida exigible; ajuste histórico de retiros",
        rubro_liquidez == "Depósitos a plazo" ~
          "Salida contractual; ajuste histórico de renovaciones",
        rubro_liquidez %in% c("Financiamiento recibido", "Otras cuentas por pagar") ~ "Salida contractual",
        rubro_liquidez == "Capital y patrimonio" ~
          "No representa una salida contractual en el ejercicio",
        rubro_liquidez == "Líneas concedidas no utilizadas" ~
          "Contingencia registrada; no se incorpora a los flujos ni a las brechas",
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
    dplyr::summarise(
      n_saldos = dplyr::n_distinct(saldo_contable),
      n_cuentas = dplyr::n_distinct(cuenta_contable),
      .groups = "drop"
    ) |>
    dplyr::filter(n_saldos > 1 | n_cuentas > 1)
  
  if (nrow(inconsistentes) > 0) {
    stop("Hay ID_REGISTRO con saldos o cuentas diferentes; revisar antes de agregar.")
  }
  
  saldos <- datos_contables |>
    dplyr::distinct(id_registro, .keep_all = TRUE) |>
    dplyr::mutate(
      cuenta_contable = as.character(cuenta_contable),
      alias = as.character(alias),
      moneda = as.character(moneda),
      periodo = as.Date(periodo)
    ) |>
    dplyr::select(-dplyr::any_of(
      c("clase_contable", "rubro_liquidez", "tratamiento_general")
    )) |>
    dplyr::left_join(
      catalogo_clasificado |>
        dplyr::select(
          cuenta_contable,
          denominacion_oficial,
          clase_contable,
          rubro_liquidez,
          tratamiento_general
        ),
      by = "cuenta_contable"
    )
  
  if (anyNA(saldos$clase_contable) ||
      any(saldos$clase_contable == "Revisar")) {
    stop("Existen cuentas contables sin clasificación. Revisar catálogo.")
  }
  
  if (anyDuplicated(saldos$id_registro))
    stop("Los saldos tienen ID duplicados.")
  
  saldos |>
    dplyr::mutate(
      dias_vencimiento = as.integer(as.Date(fecha_vencimiento) - periodo),
      banda_vencimiento = clasificar_banda(dias_vencimiento),
      disponibilidad = dplyr::case_when(
        rubro_liquidez == "Caja y bóvedas" ~ "Inmediata",
        rubro_liquidez == "Encaje en el BCCR" ~
          "Encaje; no libre automáticamente",
        rubro_liquidez == "Inversiones restringidas" ~
          "Restringida",!is.na(fecha_liberacion) &
          as.Date(fecha_liberacion) > periodo ~
          "Restringida hasta liberación",!is.na(saldo_comprometido) &
          saldo_comprometido > 0 &
          (
            is.na(fecha_fin_compromiso) |
              as.Date(fecha_fin_compromiso) > periodo
          ) ~ "Compromiso parcial o total",
        rubro_liquidez == "Inversiones" &
          as.character(negociacion_disponible) == "Sí" ~
          "Venta potencial; no efectivo inmediato",
        rubro_liquidez == "Inversiones" ~ "Venta no acreditada",
        rubro_liquidez == "Cartera de crédito" ~
          "Según cobros contractuales",
        TRUE ~ "No es disponibilidad inmediata"
      )
    )
}

calcular_tasas <- function(datos_observados) {
  # Cada observación describe los 30 días anteriores al corte respectivo.
  # No se utilizan observaciones de meses posteriores al corte.
  datos_observados |>
    dplyr::mutate(
      periodo = as.Date(periodo),
      alias = as.character(alias),
      moneda = as.character(moneda),
      tasa_retiro = tasa_acotada(retiros_vista_30d, saldo_vista_inicio_30d),
      tasa_renovacion = tasa_acotada(principal_renovado_30d, vencimientos_plazo_30d),
      tasa_cobro = tasa_acotada(cobros_recibidos_30d, cobros_programados_30d),
      tasa_uso_lineas = tasa_acotada(
        desembolsos_lineas_30d,
        limite_lineas_otorgadas_inicio_30d
      )
    ) |>
    dplyr::select(
      periodo,
      alias,
      moneda,
      tasa_retiro,
      tasa_renovacion,
      tasa_cobro,
      tasa_uso_lineas,
      linea_recibida_no_dispuesta,
      fecha_disponible_linea_recibida,
      fecha_fin_linea_recibida
    )
}

preparar_flujos <- function(datos_contrato, saldos, tasas) {
  metadatos <- saldos |>
    dplyr::select(
      id_registro,
      alias,
      moneda,
      cuenta_contable,
      denominacion_oficial,
      clase_contable,
      rubro_liquidez,
      fecha_liberacion,
      fecha_primer_pago_impago
    )
  
  pagos <- datos_contrato |>
    dplyr::mutate(
      id_registro = as.character(id_registro),
      periodo = as.Date(periodo),
      fecha_pago_contractual = as.Date(fecha_pago_contractual)
    ) |>
    dplyr::left_join(metadatos, by = "id_registro") |>
    dplyr::left_join(tasas, by = c("periodo", "alias", "moneda"))
  
  if (anyNA(pagos$clase_contable))
    stop("Hay pagos sin registro contable vinculado.")
  
  if (any(pagos$clase_contable %in% c("Patrimonio", "Contingente"))) {
    stop("Se detectaron pagos contractuales sobre cuentas no previstas como flujos.")
  }
  
  pagos <- pagos |>
    dplyr::mutate(
      dias = as.integer(fecha_pago_contractual - periodo),
      sentido = dplyr::if_else(clase_contable == "Activo", "Entrada", "Salida"),
      estado_plazo = dplyr::case_when(
        dias <= 0 & sentido == "Entrada" ~
          "Cobro vencido/no confirmado",
        dias <= 0 & sentido == "Salida" ~
          "Salida exigible al corte",
        TRUE ~ "Futuro"
      ),
      # Por prudencia, las salidas exigibles se presentan en la primera banda.
      banda = dplyr::if_else(dias <= 0 &
                               sentido == "Salida", "1-8", clasificar_banda(dias)),
      efectivo_contractual = pmax(0, monto_capital) +
        pmax(0, monto_interes),
      horizonte_30d = dias <= 30 & dias > 0,
      ajuste_cobro = dplyr::if_else(!is.na(tasa_cobro), tasa_cobro, 1),
      ajuste_retiro = dplyr::if_else(!is.na(tasa_retiro), tasa_retiro, 1),
      ajuste_renovacion = dplyr::if_else(!is.na(tasa_renovacion), tasa_renovacion, 0),
      restriccion_cobro =
        rubro_liquidez == "Inversiones restringidas" &
        !is.na(fecha_liberacion) &
        as.Date(fecha_liberacion) > fecha_pago_contractual,
      mora = rubro_liquidez == "Cartera de crédito" &
        !is.na(fecha_primer_pago_impago),
      efectivo_ajustado = dplyr::case_when(
        restriccion_cobro ~ 0,
        mora ~ 0,
        horizonte_30d &
          rubro_liquidez == "Cartera de crédito" ~
          efectivo_contractual * ajuste_cobro,
        horizonte_30d &
          rubro_liquidez == "Depósitos a la vista" ~
          efectivo_contractual * ajuste_retiro,
        horizonte_30d &
          rubro_liquidez == "Depósitos a plazo" ~
          monto_capital * (1 - ajuste_renovacion) + monto_interes,
        TRUE ~ efectivo_contractual
      ),
      tipo_flujo = "Pago contractual",
      nota_ajuste = dplyr::case_when(
        mora ~
          "Crédito con mora: entrada no reconocida en escenario ajustado",
        restriccion_cobro ~
          "Inversión restringida al pago: entrada no utilizable",
        dias <= 0 & sentido == "Entrada" ~
          "Cobro vencido: fuera de las seis bandas",
        horizonte_30d &
          rubro_liquidez == "Cartera de crédito" ~
          "Recuperación histórica 30 días",
        horizonte_30d &
          rubro_liquidez == "Depósitos a la vista" ~
          "Retiro histórico 30 días",
        horizonte_30d &
          rubro_liquidez == "Depósitos a plazo" ~
          "Renovación histórica 30 días",
        TRUE ~ "Sin ajuste; contrato o plazo superior a 30 días"
      )
    )
  
  columnas <- c(
    "id_registro",
    "periodo",
    "alias",
    "moneda",
    "cuenta_contable",
    "denominacion_oficial",
    "clase_contable",
    "rubro_liquidez",
    "fecha_pago_contractual",
    "dias",
    "banda",
    "estado_plazo",
    "tipo_flujo",
    "sentido",
    "monto_capital",
    "monto_interes",
    "efectivo_contractual",
    "efectivo_ajustado",
    "nota_ajuste"
  )
  
  pagos <- dplyr::select(pagos, dplyr::all_of(columnas))
  
  # Las líneas de crédito concedidas no utilizadas permanecen en saldos,
  # estructura y vencimientos como posiciones contingentes. No se les asigna
  # un desembolso estimado ni se incluyen en las entradas, salidas o brechas.
  # Aquí solo se devuelven los pagos contractuales de activos y pasivos.
  pagos |>
    dplyr::mutate(banda = as.character(banda))
}

resumir_bandas <- function(flujos) {
  # Las brechas solo recogen pagos contractuales y sus ajustes de comportamiento.
  # Las líneas contingentes no generan flujos ni salidas en este modelo.
  flujos |>
    dplyr::filter(banda %in% bandas_liquidez) |>
    dplyr::group_by(periodo, alias, moneda, banda) |>
    dplyr::summarise(
      entradas_contrato = sum(
        dplyr::if_else(sentido == "Entrada", efectivo_contractual, 0),
        na.rm = TRUE
      ),
      salidas_contrato = sum(
        dplyr::if_else(sentido == "Salida", efectivo_contractual, 0),
        na.rm = TRUE
      ),
      entradas_ajustadas = sum(
        dplyr::if_else(sentido == "Entrada", efectivo_ajustado, 0),
        na.rm = TRUE
      ),
      salidas_ajustadas = sum(
        dplyr::if_else(sentido == "Salida", efectivo_ajustado, 0),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    tidyr::complete(
      tidyr::nesting(periodo, alias, moneda),
      banda = bandas_liquidez,
      fill = list(
        entradas_contrato = 0,
        salidas_contrato = 0,
        entradas_ajustadas = 0,
        salidas_ajustadas = 0
      )
    ) |>
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
    dplyr::group_by(
      periodo,
      alias,
      moneda,
      banda,
      cuenta_contable,
      denominacion_oficial,
      clase_contable,
      rubro_liquidez,
      sentido
    ) |>
    dplyr::summarise(
      contractual = sum(efectivo_contractual, na.rm = TRUE),
      ajustado = sum(efectivo_ajustado, na.rm = TRUE),
      .groups = "drop"
    )
}

resumir_estructura <- function(saldos) {
  saldos |>
    dplyr::group_by(
      periodo,
      alias,
      moneda,
      clase_contable,
      cuenta_contable,
      denominacion_oficial,
      rubro_liquidez,
      disponibilidad
    ) |>
    dplyr::summarise(
      saldo = sum(saldo_contable, na.rm = TRUE),
      operaciones = dplyr::n(),
      .groups = "drop"
    )
}

resumir_vencimientos <- function(saldos) {
  saldos |>
    dplyr::group_by(
      periodo,
      alias,
      moneda,
      clase_contable,
      rubro_liquidez,
      cuenta_contable,
      denominacion_oficial,
      banda_vencimiento,
      disponibilidad
    ) |>
    dplyr::summarise(
      saldo = sum(saldo_contable, na.rm = TRUE),
      operaciones = dplyr::n(),
      .groups = "drop"
    )
}

crear_tabla_maestra <- function(estructura, rubros) {
  # Una fila por cuenta y corte: el saldo aparece una sola vez.
  # Las seis bandas se convierten en columnas, evitando duplicar saldos.
  base <- estructura |>
    dplyr::group_by(
      periodo,
      alias,
      moneda,
      cuenta_contable,
      denominacion_oficial,
      clase_contable,
      rubro_liquidez
    ) |>
    dplyr::summarise(
      saldo = sum(saldo),
      operaciones = sum(operaciones),
      disponibilidad = paste(sort(unique(disponibilidad)), collapse = "; "),
      .groups = "drop"
    )
  
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
      salidas_ajustadas = sum(salidas_ajustadas),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      brecha_contrato = entradas_contrato - salidas_contrato,
      brecha_ajustada = entradas_ajustadas - salidas_ajustadas
    ) |>
    tidyr::pivot_wider(
      names_from = banda_codigo,
      values_from = c(
        entradas_contrato,
        salidas_contrato,
        brecha_contrato,
        entradas_ajustadas,
        salidas_ajustadas,
        brecha_ajustada
      ),
      values_fill = 0
    )
  
  base |>
    dplyr::left_join(matriz, by = c("periodo", "alias", "moneda", "cuenta_contable")) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with(c(
      "entradas_", "salidas_", "brecha_"
    )), ~ tidyr::replace_na(.x, 0)))
}

crear_modelo_liquidez <- function(datos_contables,
                                  datos_contrato,
                                  datos_observados,
                                  datos_catalogo) {
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
    prueba = c(
      "Cuentas sin clase",
      "IDs contables duplicados tras preparar saldos",
      "Pagos sin vínculo",
      "Registros contables conservados",
      "Pagos contractuales conservados"
    ),
    resultado = c(
      sum(catalogo$clase_contable == "Revisar"),
      sum(duplicated(saldos$id_registro)),
      sum(
        !as.character(datos_contrato$id_registro) %in%
          saldos$id_registro
      ),
      nrow(saldos),
      sum(flujos$tipo_flujo == "Pago contractual")
    )
  )
  
  list(
    catalogo = catalogo,
    saldos = saldos,
    tasas = tasas,
    flujos = flujos,
    bandas = bandas,
    rubros = rubros,
    estructura = estructura,
    vencimientos = vencimientos,
    tabla_maestra = tabla_maestra,
    verificaciones = verificaciones
  )
}

################## Punto 4 ##################

# Función auxiliar que vende las inversiones de una entidad por periodo, moneda
# y bandas, de menor a mayor costo, para cubrir el faltante de liquidez
venta_inv <- function(temporal,
                      bandas,
                      saldos,
                      act_liquidados,
                      periodo,
                      entidad,
                      moneda,
                      banda,
                      j) {
  inversiones <- saldos %>%
    filter(
      .data$rubro_liquidez %in% c("Inversiones", "Inversiones restringidas"),
      .data$periodo == .env$periodo,
      .data$alias == .env$entidad,
      .data$moneda == .env$moneda,
      .data$banda_num >= .env$j
    ) %>%
    arrange(.data$valor_cotizado_venta_urgente)
  
  remanente_inv <- 0
  
  for (i in 1:nrow(inversiones)) {
    temporal <- temporal + inversiones$valor_cotizado_venta_urgente[i]
    
    if (temporal >= 0) {
      if (i != nrow(inversiones)) {
        remanente_inv <- sum(inversiones$valor_cotizado_venta_urgente[(i + 1):nrow(inversiones)])
        
      }
      
      break
      
    }
    
  }
  
  inversiones_vend <- inversiones[1:i, ]
  
  inv_resumen <- inversiones_vend %>%
    group_by(periodo, alias, moneda, banda) %>%
    summarise(
      monto_capital = sum(monto_capital, na.rm = TRUE),
      monto_interes = sum(monto_interes, na.rm = TRUE),
      .groups = "drop"
    )
  
  bandas <- bandas %>%
    left_join(inv_resumen, by = c("periodo", "alias", "moneda", "banda")) %>%
    mutate(
      brecha_contrato = if_else(
        .data$banda == .env$banda,
        brecha_contrato,
        brecha_contrato -
          coalesce(monto_capital, 0) -
          coalesce(monto_interes, 0)
      )
    ) %>%
    select(-monto_capital, -monto_interes)
  
  act_liquidados <- rbind(act_liquidados, inversiones_vend)
  
  return(
    list(
      temporal = temporal,
      remanente_inv = remanente_inv,
      bandas = bandas,
      act_liquidados = act_liquidados
    )
  )
}

# Supuesto inicial de encaje
tasa_encaje <- 0.15

# Función que añade las columnas de recursos utilizados para cubrir faltantes de
# liquidez y de recursos remanentes al dataframe de bandas
cobertura_faltantes <- function(modelo) {
  # Se selecciona el dataframe a modificar
  bandas <- modelo$bandas
  
  # Dataframe de saldos
  saldos <- modelo$saldos
  
  # Se le agrega la banda respectiva a cada fila
  saldos <- saldos %>%
    left_join(modelo$flujos %>% select(id_registro, banda), by = "id_registro")
  
  # Se le agrega el número de la banda respectiva
  saldos$banda_num <- case_when(
    saldos$banda == "1-8" ~ 1,
    saldos$banda == "9-15" ~ 2,
    saldos$banda == "16-30" ~ 3,
    saldos$banda == "31-89" ~ 4,
    saldos$banda == "90-180" ~ 5,
    saldos$banda == "181+" ~ 6
  )
  
  # Dataframe con las inversiones liquidadas por bandas, después se actualiza
  act_liquidados <- saldos[1, ]
  
  # Se suman los valores de las disponibilidades utilizables por periodo, entidad
  # y moneda
  saldos_caja <- saldos %>%
    filter(rubro_liquidez == "Caja y bóvedas") %>%
    group_by(periodo, alias, moneda) %>%
    summarise(saldo_caja = sum(saldo_contable, na.rm = TRUE),
              .groups = "drop")
  
  # Se asigna el saldo de la caja solo a las bandas iniciales de 1 a 8 días
  bandas <- bandas %>%
    left_join(saldos_caja, by = c("periodo", "alias", "moneda")) %>%
    mutate(saldo_caja = if_else(banda == "1-8", saldo_caja, 0))
  
  # Disponibilidades utilizables remanentes
  bandas$saldo_caja_rem <- numeric(length = nrow(bandas))
  
  # Reservas mantenidas por periodo, entidad y moneda
  reserv_man <- saldos %>%
    filter(rubro_liquidez == "Encaje en el BCCR") %>%
    group_by(periodo, alias, moneda) %>%
    summarise(reserv_man = sum(saldo_contable, na.rm = TRUE),
              .groups = "drop")
  
  # Base sujeta a encaje por periodo, entidad y moneda
  base_encaje <- saldos %>%
    filter(rubro_liquidez %in% c("Depósitos a la vista", "Depósitos a plazo")) %>%
    group_by(periodo, alias, moneda) %>%
    summarise(base_encaje = sum(saldo_contable, na.rm = TRUE),
              .groups = "drop")
  
  # Reservas excedentes por periodo, entidad y moneda
  reserv_exc <- reserv_man %>%
    left_join(base_encaje, by = c("periodo", "alias", "moneda")) %>%
    mutate(reserv_exc = pmax(0, reserv_man - tasa_encaje * base_encaje)) %>%
    select(-reserv_man, -base_encaje)
  
  # Se asignan las reservas excedentes solo a las bandas iniciales de 1 a 8 días
  bandas <- bandas %>%
    left_join(reserv_exc, by = c("periodo", "alias", "moneda")) %>%
    mutate(reserv_exc = if_else(banda == "1-8", reserv_exc, 0))
  
  # Reservas excedentes remanentes
  bandas$reserv_exc_rem <- numeric(length = nrow(bandas))
  
  # Valor de venta urgente de todas las inversiones e inversiones restringidas
  # por periodo, entidad, moneda y banda
  venta_instr <- saldos %>%
    filter(rubro_liquidez %in% c("Inversiones", "Inversiones restringidas")) %>%
    group_by(periodo, alias, moneda, banda) %>%
    summarise(
      venta_instr = sum(valor_cotizado_venta_urgente, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Se asigna el valor de venta urgente de las inversiones a la banda respectiva
  bandas <- bandas %>%
    left_join(venta_instr, by = c("periodo", "alias", "moneda", "banda"))
  
  # Valor de la venta de instrumentos remanente
  bandas$venta_instr_rem <- numeric(length = nrow(bandas))
  
  # Líneas de crédito utilizables por periodo, entidad y moneda
  lineas_cred <- saldos %>%
    filter(rubro_liquidez == "Líneas concedidas no utilizadas") %>%
    group_by(periodo, alias, moneda) %>%
    summarise(lineas_cred = sum(saldo_contable, na.rm = TRUE),
              .groups = "drop")
  
  # Se asignan las líneas de crédito solo a las bandas iniciales de 1 a 8 días
  bandas <- bandas %>%
    left_join(lineas_cred, by = c("periodo", "alias", "moneda")) %>%
    mutate(lineas_cred = if_else(banda == "1-8", lineas_cred, 0))
  
  # Valor de las líneas de crédito remanente
  bandas$lineas_cred_rem <- numeric(length = nrow(bandas))
  
  # Columna de faltante residual
  bandas$falt_resid <- numeric(length = nrow(bandas))
  
  # Se recorre las bandas para cubrir los faltantes de liquidez con los recursos disponibles
  for (i in seq(1, nrow(bandas), by = 6)) {
    # Filas del bloque, es decir, las bandas
    filas <- i:min(i + 5, nrow(bandas))
    
    # Se itera sobre las bandas de cada bloque
    for (j in 1:6) {
      # Si es la última banda, la línea de crédito vence y ya no puede utilizarse
      if (j == 6) {
        bandas$lineas_cred[filas[j]] <- 0
        
      }
      
      # Si la brecha es negativa se usan las disponibilidades utilizables, sino
      # se actualizan los recursos remanentes
      if (bandas$brecha_contrato[filas[j]] < 0) {
        temporal <- bandas$brecha_contrato[filas[j]] + bandas$saldo_caja[filas[j]]
        
      } else{
        if (j != 6) {
          bandas$saldo_caja[filas[j + 1]] <- bandas$saldo_caja[filas[j]]
          
          bandas$reserv_exc[filas[j + 1]] <- bandas$reserv_exc[filas[j]]
          
          bandas$lineas_cred[filas[j + 1]] <- bandas$lineas_cred[filas[j]]
          
        } else{
          bandas$lineas_cred[filas[j]] <- 0
          
        }
        
        bandas$saldo_caja_rem[filas[j]] <- bandas$saldo_caja[filas[j]]
        
        bandas$reserv_exc_rem[filas[j]] <- bandas$reserv_exc[filas[j]]
        
        bandas$venta_instr_rem[filas[j]] <- bandas$venta_instr[filas[j]]
        
        bandas$lineas_cred_rem[filas[j]] <- bandas$lineas_cred[filas[j]]
        
        next
        
      } # Fin if-else
      
      # Si el primer recurso no fue suficiente se utilizan las reservas excedentes,
      # sino se actualizan los recursos remanentes
      if (temporal < 0) {
        temporal <- temporal + bandas$reserv_exc[filas[j]]
        
      } else{
        if (j != 6) {
          bandas$saldo_caja[filas[j + 1]] <- temporal
          
          bandas$reserv_exc[filas[j + 1]] <- bandas$reserv_exc[filas[j]]
          
          bandas$lineas_cred[filas[j + 1]] <- bandas$lineas_cred[filas[j]]
          
        } else{
          bandas$lineas_cred[filas[j]] <- 0
          
        }
        
        bandas$saldo_caja_rem[filas[j]] <- temporal
        
        bandas$reserv_exc_rem[filas[j]] <- bandas$reserv_exc[filas[j]]
        
        bandas$venta_instr_rem[filas[j]] <- bandas$venta_instr[filas[j]]
        
        bandas$lineas_cred_rem[filas[j]] <- bandas$lineas_cred[filas[j]]
        
        next
        
      } # Fin if-else
      
      # Si los primeros 2 recursos no fueron suficientes se venden inversiones,
      # sino se actualizan los recursos remanentes
      if (temporal < 0) {
        temporal_remanente <- venta_inv(
          temporal,
          bandas,
          saldos,
          act_liquidados,
          bandas$periodo[filas[j]],
          bandas$alias[filas[j]],
          bandas$moneda[filas[j]],
          bandas$banda[filas[j]],
          j
        )
        
        temporal <- temporal_remanente$temporal
        
        bandas <- temporal_remanente$bandas
        
        act_liquidados <- temporal_remanente$act_liquidados
        
      } else{
        if (j != 6) {
          bandas$reserv_exc[filas[j + 1]] <- temporal
          
          bandas$lineas_cred[filas[j + 1]] <- bandas$lineas_cred[filas[j]]
          
        } else{
          bandas$lineas_cred[filas[j]] <- 0
          
        }
        
        bandas$reserv_exc_rem[filas[j]] <- temporal
        
        bandas$venta_instr_rem[filas[j]] <- bandas$venta_instr[filas[j]]
        
        bandas$lineas_cred_rem[filas[j]] <- bandas$lineas_cred[filas[j]]
        
        next
        
      } # Fin if-else
      
      # Si todavía no se cubre el faltante se usa el financiamiento disponible,
      # sino se actualizan los recursos remanentes
      if (temporal < 0) {
        temporal <- temporal + bandas$lineas_cred[filas[j]]
        
      } else{
        if (j != 6) {
          bandas$saldo_caja[filas[j + 1]] <- temporal
          
          bandas$lineas_cred[filas[j + 1]] <- bandas$lineas_cred[filas[j]]
          
        } else{
          bandas$falt_resid <- temporal
          
          bandas$lineas_cred[filas[j]] <- 0
          
        }
        
        bandas$venta_instr_rem[filas[j]] <- temporal_remanente$remanente_inv
        
        bandas$lineas_cred_rem[filas[j]] <- bandas$lineas_cred[filas[j]]
        
        next
        
      } # Fin if-else
      
      # En caso de no cubrirse el faltante, se registra como un faltante residual
      # positivo, sino entonces se actualiza el valor remanente de las líneas de crédito
      if (temporal < 0) {
        bandas$falt_resid[filas[j]] <- temporal
        
      } else{
        if (j != 6) {
          bandas$lineas_cred[filas[j + 1]] <- temporal
          
          bandas$lineas_cred_rem[filas[j]] <- temporal
          
        }
        
      } # Fin if-else
      
    } # Fin for j
    
  } # Fin for i
  
  act_liquidados <- act_liquidados[-1, ]
  
  return(list(bandas = bandas, act_liquidados = act_liquidados))
  
}

################## Punto 5 ##################






