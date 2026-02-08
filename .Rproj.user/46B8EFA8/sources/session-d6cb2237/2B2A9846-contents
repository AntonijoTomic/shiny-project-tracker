library(DBI)

load_tasks <- function() {
  con <- open_con()
  on.exit({ if (DBI::dbIsValid(con)) dbDisconnect(con) }, add = TRUE)
  
  df <- dbGetQuery(con, "
    SELECT
      id,
      project,
      task,
      status,
      CONVERT(varchar(10), start_date, 23) AS start_date,
      CONVERT(varchar(10), end_date, 23)   AS end_date,
      a, m, b,
      ROUND( (a + 4*m + b) / 6.0, 2 ) AS te,
      ROUND( POWER((b - a) / 6.0, 2), 4 ) AS var,
      predecessor_id
    FROM dbo.tasks
  ")
  
  df$predecessor_id <- as.integer(df$predecessor_id)
  df
}

insert_task <- function(project, task, status, start_date, end_date, a, m, b, pred_id = NA) {
  con <- open_con()
  on.exit({ if (DBI::dbIsValid(con)) dbDisconnect(con) }, add = TRUE)
  
  dbExecute(
    con,
    "INSERT INTO dbo.tasks (project, task, status, start_date, end_date, a, m, b, predecessor_id)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      trimws(project),
      trimws(task),
      status,
      as.character(start_date),
      as.character(end_date),
      as.numeric(a),
      as.numeric(m),
      as.numeric(b),
      if (is.na(pred_id) || pred_id == "") NA else as.integer(pred_id)
    )
  )
}

