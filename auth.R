# Authentication records are stored in Neon; only password hashes are persisted.

register_user <- function(p, username, password) {
  username <- trimws(username)
  if (length(username) != 1L || is.na(username) ||
      !grepl("^[A-Za-z0-9_]{3,30}$", username)) {
    stop("Username must contain 3 to 30 letters, digits, or underscores.")
  }
  if (length(password) != 1L || is.na(password) ||
      nchar(password, type = "chars") < 12L ||
      nchar(password, type = "bytes") > 1024L) {
    stop("Password must contain 12 or more characters (maximum 1024 bytes).")
  }

  hash <- sodium::password_store(password)
  db_query(p,
    paste0("INSERT INTO app_users (username, password_hash, role, status) ",
           "VALUES ($1, $2, 'reviewer', 'pending') RETURNING user_id"),
    list(username, hash))
  invisible(TRUE)
}

authenticate_user <- function(p, username, password) {
  if (length(username) != 1L || length(password) != 1L ||
      is.na(username) || is.na(password)) return(NULL)

  user <- db_query(p,
    "SELECT user_id, username, password_hash, role, status FROM app_users WHERE username = $1",
    list(trimws(username)))

  if (nrow(user) != 1L) return(NULL)

  valid <- tryCatch(
    sodium::password_verify(user$password_hash[1], password),
    error = function(e) FALSE)
  if (!isTRUE(valid) || !identical(user$status[1], 'active')) return(NULL)

  exec(p, "UPDATE app_users SET last_login_at = NOW() WHERE user_id = $1",
       list(as.character(user$user_id[1])))
  user[, c('user_id', 'username', 'role'), drop = FALSE]
}
