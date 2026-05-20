# ── MCP Progress Notifications (spec 2025-06-18) ────────────────
#
# Clients may send a _meta.progressToken in tools/call requests.
# Servers can emit progress notifications to report long-running work.
#
# Tool functions should call emit_progress() during execution:
#   emit_progress(current = 50, total = 100)
#
# The helper is a no-op when no progress token is active.

#' @title Emit a progress notification for a running tool
#'
#' @description
#' Sends an MCP `notifications/progress` message to the connected client.
#' Only has an effect when the client supplied a `_meta.progressToken` in
#' the current `tools/call` request. Otherwise it's a silent no-op.
#'
#' Tool functions should call this at meaningful milestones to give
#' clients visibility into long-running operations.
#'
#' @param current The current progress value (non-negative number).
#' @param total The total progress value (optional). When provided,
#'   clients can display a percentage.
#'
#' @examples
#' # Inside a tool function:
#' for (i in seq_len(100)) {
#'   # ... do work ...
#'   if (i %% 10 == 0) emit_progress(current = i, total = 100)
#' }
#'
#' @export
emit_progress <- function(current, total = NULL) {
  token <- the$current_progress_token
  if (is.null(token)) return(invisible(NULL))

  notification <- drop_nulls(list(
    jsonrpc = "2.0",
    method = "notifications/progress",
    params = drop_nulls(list(
      progressToken = token,
      progress = current,
      total = total
    ))
  ))

  cat_json(notification)
  invisible(NULL)
}

# Internal: extract and store progress token from incoming request
store_progress_token <- function(data) {
  the$current_progress_token <- NULL
  if (!is.null(data$params$`_meta`)) {
    meta <- data$params$`_meta`
    if (!is.null(meta$progressToken)) {
      the$current_progress_token <- meta$progressToken
    }
  }
}

# Internal: clear progress token after tool execution
clear_progress_token <- function() {
  the$current_progress_token <- NULL
}

# ── MCP Logging (spec 2025-06-18) ───────────────────────────────
#
# Servers can emit structured log messages to clients via
# notifications/message. Clients control verbosity with logging/setLevel.
#
# Tool functions should call emit_log():
#   emit_log("info", "Downloading occurrences... 1200 records")
#   emit_log("warning", "Some records lack coordinates")

#' The log levels defined by MCP (RFC 5424 syslog severity).
#' @noRd
MCP_LOG_LEVELS <- c("debug", "info", "notice", "warning", "error", "critical", "alert", "emergency")

# Internal: numeric rank for level comparison (higher = more severe)
log_level_rank <- function(level) {
  match(match.arg(level, MCP_LOG_LEVELS), MCP_LOG_LEVELS)
}

#' @title Emit a structured log message to the MCP client
#'
#' @description
#' Sends an MCP `notifications/message` notification to the client.
#' Respects the minimum log level set by the client via `logging/setLevel`.
#' Messages below the minimum level are silently dropped.
#'
#' @param level Log level: one of "debug", "info", "notice", "warning",
#'   "error", "critical", "alert", "emergency".
#' @param message Human-readable log message string.
#' @param logger Optional logger name (e.g., "occurrences", "models").
#' @param data Optional structured data (named list, JSON-serialisable).
#'
#' @examples
#' emit_log("info", "Downloading occurrences from ALA...")
#' emit_log("warning", "Some records lack coordinates", logger = "occurrences")
#' emit_log("error", "Model fitting failed", data = list(species = "Koala"))
#'
#' @export
emit_log <- function(level = "info", message, logger = NULL, data = NULL) {
  min_level <- the$min_log_level %||% "info"
  if (log_level_rank(level) < log_level_rank(min_level)) {
    return(invisible(NULL))
  }

  notification <- drop_nulls(list(
    jsonrpc = "2.0",
    method = "notifications/message",
    params = drop_nulls(list(
      level = level,
      logger = logger,
      data = if (!is.null(data)) data else message
    ))
  ))

  cat_json(notification)
  invisible(NULL)
}

# ── MCP Cancellation (spec 2025-06-18) ──────────────────────────
#
# Clients can send notifications/cancelled to abort a running tool.
# Tool functions should check cancelled() periodically:
#   if (cancelled()) stop("Operation cancelled by user")

#' @title Check if the current tool call has been cancelled
#'
#' @description
#' Returns `TRUE` if the client has sent a `notifications/cancelled`
#' for the current tool call. Tool functions should check this
#' periodically in long-running loops and exit cleanly if cancelled.
#'
#' @return Logical scalar.
#'
#' @examples
#' for (i in seq_len(nrow(data))) {
#'   if (cancelled()) stop("Operation cancelled by user")
#'   # ... process row ...
#' }
#'
#' @export
cancelled <- function() {
  isTRUE(the$cancelled_requests[[as.character(the$current_request_id)]])
}

# Internal: mark a request as cancelled
mark_cancelled <- function(request_id) {
  if (is.null(the$cancelled_requests)) the$cancelled_requests <- list()
  the$cancelled_requests[[as.character(request_id)]] <- TRUE
}

# Internal: clear cancellation flag when request completes
clear_cancelled <- function(request_id) {
  if (!is.null(the$cancelled_requests)) {
    the$cancelled_requests[[as.character(request_id)]] <- NULL
  }
}
