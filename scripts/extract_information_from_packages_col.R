library(stringr)


df_steam <- df_clean

parse_steam_packages_final <- function(pkg_str) {
  
  # Estructura de salida
  result <- list(
    count_versions = 0,
    min_version_price = NA_real_,
    max_version_price = NA_real_,
    count_subscriptions = 0,
    min_monthly_sub_price = NA_real_,
    max_monthly_sub_price = NA_real_,
    count_micropayments = 0,
    min_micropayment_price = NA_real_,
    max_micropayment_price = NA_real_
  )
  
  if (is.na(pkg_str) || pkg_str == "" || pkg_str == "[]") return(result)
  
  pkg_lower <- str_to_lower(pkg_str)
  
  # Separamos bloques por título
  chunks <- str_split(pkg_lower, "\\{'title':")[[1]]
  chunks <- chunks[chunks != ""] 
  
  prices_versions <- c()
  prices_subs_monthly <- c()
  prices_micro <- c()
  
  micro_keywords <- c("coin", "credit", "point", "token", "currency", 
                      "gold", "gem", "diamond", "cash", "money", 
                      "ticket", "game time", "cartel coins", "platinum",
                      "atom", "season pass", "battle pass", "crowns", "bfc")
  micro_regex <- paste(micro_keywords, collapse = "|")
  
  # Frase "Mágica" para detectar micropagos/DLCs vs Versiones limpias
  magic_phrase_micro <- "click here for more information"
  
  for (chunk in chunks) {
    
    # 1. Extracción segura del título (para keywords)
    block_title <- str_extract(chunk, "^\\s*'([^']+)'")
    if (is.na(block_title)) block_title <- "" else block_title <- str_remove_all(block_title, "'")
    
    # 2. Extracción de precios
    prices_matches <- str_match_all(chunk, "'price':\\s*([0-9\\.]+)")[[1]]
    
    if (nrow(prices_matches) > 0) {
      raw_prices <- as.numeric(prices_matches[, 2])
      raw_prices <- raw_prices[!is.na(raw_prices)] # Limpieza básica
      
      if (length(raw_prices) > 0) {
        
        # --- LÓGICA DE DECISIÓN JERÁRQUICA ---
        
        # A. ¿Es Suscripción? (PRIORIDAD ALTA)
        # Buscamos patrones de recurrencia temporal explícita
        is_sub_block <- str_detect(chunk, "subscription|recurring|auto-renewed|/ (day|month|year)")
        if (is.na(is_sub_block)) is_sub_block <- FALSE
        
        if (is_sub_block) {
          # >>> PROCESAR COMO SUSCRIPCIÓN <<<
          
          # Normalización temporal
          sub_regex <- "([0-9\\.]+)\\s*/\\s*([0-9]+)?\\s*(day|month|year)s?"
          matches <- str_match_all(chunk, sub_regex)[[1]]
          
          if (nrow(matches) > 0) {
            for(i in 1:nrow(matches)) {
              p <- as.numeric(matches[i, 2])
              dur_num <- as.numeric(matches[i, 3]); if (is.na(dur_num)) dur_num <- 1
              dur_unit <- matches[i, 4]
              
              divisor <- 1
              if (!is.na(dur_unit)) {
                if (str_starts(dur_unit, "year")) divisor <- dur_num * 12
                else if (str_starts(dur_unit, "day")) divisor <- dur_num / 30.44
                else divisor <- dur_num 
              }
              if (!is.na(p) && divisor > 0) prices_subs_monthly <- c(prices_subs_monthly, p / divisor)
            }
          }
          
        } else {
          # NO ES SUSCRIPCIÓN. AHORA DISCRIMINAMOS ENTRE VERSIÓN Y MICROPAGO
          
          # B. ¿Es Micropago?
          # Criterio 1: El título tiene palabras clave (atom, coin, pass...)
          has_micro_keyword <- str_detect(block_title, micro_regex)
          if (is.na(has_micro_keyword)) has_micro_keyword <- FALSE
          
          # Criterio 2 (TU APORTE): La descripción contiene "Click HERE..."
          # Como ya descartamos suscripciones, si aparece esto, es casi seguro un DLC o Micropago
          has_magic_phrase <- str_detect(chunk, magic_phrase_micro)
          if (is.na(has_magic_phrase)) has_magic_phrase <- FALSE
          
          if (has_micro_keyword || has_magic_phrase) {
            # >>> PROCESAR COMO MICROPAGO <<<
            prices_micro <- c(prices_micro, raw_prices)
          } else {
            # >>> PROCESAR COMO VERSIÓN (Juego Base/Deluxe) <<<
            # Por descarte: No es sub, no tiene keywords de moneda, no tiene el texto genérico de DLC
            prices_versions <- c(prices_versions, raw_prices)
          }
        }
      }
    }
  }
  
  # -- CÁLCULO FINAL DE METADATOS --
  
  # 1. Versiones
  result$count_versions <- length(prices_versions)
  paid_versions <- prices_versions[prices_versions > 0]
  if (length(paid_versions) > 0) {
    result$min_version_price <- min(paid_versions)
    result$max_version_price <- max(paid_versions)
  } else if (length(prices_versions) > 0) { # F2P puro
    result$min_version_price <- 0; result$max_version_price <- 0
  }
  
  # 2. Suscripciones
  result$count_subscriptions <- length(prices_subs_monthly)
  if (length(prices_subs_monthly) > 0) {
    result$min_monthly_sub_price <- min(prices_subs_monthly)
    result$max_monthly_sub_price <- max(prices_subs_monthly)
  }
  
  # 3. Micropagos
  result$count_micropayments <- length(prices_micro)
  paid_micro <- prices_micro[prices_micro > 0]
  if (length(paid_micro) > 0) {
    result$min_micropayment_price <- min(paid_micro)
    result$max_micropayment_price <- max(paid_micro)
  }
  
  return(result)
}

features_economics <- map_dfr(df_steam[["packages"]], parse_steam_packages_final)

df_final <- bind_cols(df_steam, features_economics)
df_final$packages <- NULL

# La variable 'price' del dataset original puede llegar a ser engañosa, ya que hay
# mucho mercado más allá de la compra de los juegos. Dentro de ellos puede haber
# suscripciones o microtransacciones para comprar puntos. Por este motivo hemos
# seleccionado la columna packages para extraer este tipo de información que
# que venía en un formato semiestructurado.
# Mediante patrones que hemos observado y expresiones regulares hemos conseguido
# extraer varias columnas de información muy útiles:
#   - Número de versiones del juego: hay muchos juegos que contienen varias
#     versiones de entrada para el juego, como pueden ser el juego normal y una
#     versión 'ultimate' por ejemplo. Para obtener más información hemos obtenido 
#     otros dos valores:
#       - Precio mínimo de las versiones: Nos sirve para saber cuál es el mínimo
#         que te has de gastar para poder jugar ese juego
#       - Precio máximo de las versiones: Sirve para saber cuanto nos podemos
#         llegar a gastar de primeras
#   - Número de suscripciones posibles: Nos indica la cantidad de suscripciones
#     que están disponibles. Acompañada de esta variable hemos generado dos
#     variables más que están normalizadas por precio por mes
#     (Ejemplo: $30 / 3 meses -> 10$ / mes). Estas variables son:
#       - Precio mínimo por mes: La barrera de entrada de las suscripciones,
#         Normalmente cuando tienen varios modelos de suscripciones el modelo que
#         sale más económico mensualmente es el que incluye más meses en la
#         suscripción.
#       - Precio máximo por mes : Lo máximo que llegarías a pagar con un plan en
#         un mes. Normalmente, estos valores provienen de la suscripción más corta
#   - Número de micropagos disponibles: Cantidad de pagos relacionados con compras
#     de monedas para el juego, normalmente no son pagos que se requieran para
#     poder jugar. A esta variable, como a las demás les acompañan dos variables:
#       - Precio del micropago más barato
#       - Precio del micropago más caro.
#
# De esta manera hemos conseguido extraer información muy importante de cada uno
# de los juegos, exactamente hemos extraído 9 columnas nuevas que nos darán
# información tanto de la complejidad de la oferta, como de los rangos de precios.
#
# ¡¡ IMPORTANTE !! --> las columnas de min y max price de número de suscripciones
# y de número de micropagos hay que tener en cuenta que tiene muchos NA's. No 
# se deben usar para realizar los clusters.