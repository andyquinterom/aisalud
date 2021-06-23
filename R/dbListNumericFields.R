# Funcion para encontrar las columnas numericas dentro de una tabla
# en PostgreSQL.
# conn es la conexion a la base de datos y table_name es el nombre de la tabla
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
