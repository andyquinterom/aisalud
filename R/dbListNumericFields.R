#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#

#' @title Columnas númericas en PostgreSQL
#' @description Funcion para encontrar las columnas numericas dentro de una 
#' tabla en PostgreSQL.
#' @param conn Conexión a base de datos
#' @param table_name Nombre de la tabla en base de datos
#' @return Vector con nombres de columnas de tipo numerico


dbListNumericFields <- function(conn, table_name) {
  # El query en SQL que se utiliza para obtener los datos
  sql <- "select
       col.column_name
from information_schema.columns col
join information_schema.tables tab on tab.table_schema = col.table_schema
                                   and tab.table_name = col.table_name
                                   and tab.table_type = 'BASE TABLE'
where col.data_type in ('smallint', 'integer', 'bigint',
                        'decimal', 'numeric', 'real', 'double precision',
                        'smallserial', 'serial', 'bigserial', 'money')
      and col.table_schema not in ('information_schema', 'pg_catalog')
      and col.table_name in (?id)
order by col.table_schema,
         col.table_name,
         col.ordinal_position"
  # Se utiliza sqlInterpolate para evitar SQL injection attacks
  query <- sqlInterpolate(conn, sql, id = table_name)
  # Se envia el query
  dbGetQuery(conn, query) %>%
    unlist() %>%
    unname()
}
