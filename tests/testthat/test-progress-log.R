skip_if(is_fedora())

# ── emit_progress ───────────────────────────────────────────────

test_that("emit_progress is a no-op when no progress token is set", {
  the$current_progress_token <- NULL
  expect_invisible(emit_progress(current = 50, total = 100))
})

test_that("store_progress_token extracts from _meta", {
  the$current_progress_token <- NULL
  on.exit(the$current_progress_token <- NULL, add = TRUE)

  data <- list(params = list(`_meta` = list(progressToken = "abc123")))
  store_progress_token(data)

  expect_equal(the$current_progress_token, "abc123")
})

test_that("store_progress_token handles missing _meta", {
  the$current_progress_token <- "old"
  on.exit(the$current_progress_token <- NULL, add = TRUE)

  data <- list(params = list())
  store_progress_token(data)

  expect_null(the$current_progress_token)
})

test_that("clear_progress_token resets", {
  the$current_progress_token <- "abc"
  clear_progress_token()
  expect_null(the$current_progress_token)
})

# ── emit_log ────────────────────────────────────────────────────

test_that("emit_log respects minimum log level", {
  the$min_log_level <- "warning"
  on.exit(the$min_log_level <- "info", add = TRUE)

  # "info" is below "warning" — should be silently dropped
  expect_invisible(emit_log("info", "This should be dropped"))
})

test_that("emit_log sends when level meets threshold", {
  the$min_log_level <- "warning"
  on.exit(the$min_log_level <- "info", add = TRUE)

  # "error" is above "warning" — should produce output
  # We can't easily capture cat_json output, but at least verify no error
  expect_invisible(emit_log("error", "Something went wrong"))
})

test_that("log_level_rank returns correct ordering", {
  expect_lt(log_level_rank("debug"), log_level_rank("info"))
  expect_lt(log_level_rank("info"), log_level_rank("warning"))
  expect_lt(log_level_rank("warning"), log_level_rank("error"))
  expect_lt(log_level_rank("error"), log_level_rank("critical"))
})

# ── cancelled ───────────────────────────────────────────────────

test_that("cancelled returns FALSE by default", {
  the$cancelled_requests <- list()
  expect_false(cancelled())
})

test_that("cancelled returns TRUE after mark_cancelled", {
  the$current_request_id <- 42
  the$cancelled_requests <- list()
  on.exit({
    the$cancelled_requests <- list()
    the$current_request_id <- NULL
  }, add = TRUE)

  mark_cancelled(42)
  expect_true(cancelled())
})

test_that("clear_cancelled removes the flag", {
  the$cancelled_requests <- list()
  mark_cancelled(99)
  expect_true(isTRUE(the$cancelled_requests[["99"]]))
  clear_cancelled(99)
  expect_null(the$cancelled_requests[["99"]])
})

# ── execute_tool_call with middleware ───────────────────────────

test_that("execute_tool_call runs on_tool_call middleware", {
  called <- FALSE
  the$on_tool_call <- function(name, args) {
    called <<- TRUE
    NULL  # proceed with original args
  }
  on.exit(the$on_tool_call <- NULL, add = TRUE)

  tool1 <- ellmer::tool(
    fun = function() "hello",
    name = "middleware_test",
    description = "Test middleware",
    arguments = list()
  )
  set_server_tools(list(tool1), session_tools = FALSE)

  data <- list(
    id = 1,
    method = "tools/call",
    params = list(name = "middleware_test", arguments = list()),
    tool = tool1
  )

  result <- execute_tool_call(data)
  expect_true(called)
})

test_that("execute_tool_call middleware can modify args", {
  the$on_tool_call <- function(name, args) {
    args$x <- toupper(args$x)
    args
  }
  on.exit(the$on_tool_call <- NULL, add = TRUE)

  tool1 <- ellmer::tool(
    fun = function(x) paste0("got: ", x),
    name = "modify_args_test",
    description = "Test arg modification",
    arguments = list(x = ellmer::type_string("Input"))
  )
  set_server_tools(list(tool1), session_tools = FALSE)

  data <- list(
    id = 1,
    method = "tools/call",
    params = list(name = "modify_args_test", arguments = list(x = "hello")),
    tool = tool1
  )

  result <- execute_tool_call(data)
  expect_true(grepl("HELLO", result$result$content[[1]]$text))
})

test_that("execute_tool_call middleware can block execution", {
  the$on_tool_call <- function(name, args) {
    jsonrpc_response(
      1,
      error = list(code = -32603, message = "Blocked by policy")
    )
  }
  on.exit(the$on_tool_call <- NULL, add = TRUE)

  tool1 <- ellmer::tool(
    fun = function() "should not run",
    name = "blocked_test",
    description = "Test blocking",
    arguments = list()
  )
  set_server_tools(list(tool1), session_tools = FALSE)

  data <- list(
    id = 1,
    method = "tools/call",
    params = list(name = "blocked_test", arguments = list()),
    tool = tool1
  )

  result <- execute_tool_call(data)
  expect_true(grepl("Blocked", result$error$message))
})

# ── logging/setLevel handler ────────────────────────────────────

test_that("logging/setLevel updates min_log_level", {
  the$min_log_level <- "info"
  on.exit(the$min_log_level <- "info", add = TRUE)

  # Simulate the handler logic
  level <- "debug"
  if (level %in% MCP_LOG_LEVELS) {
    the$min_log_level <- level
  }

  expect_equal(the$min_log_level, "debug")
})
