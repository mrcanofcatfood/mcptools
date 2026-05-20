skip_if(is_fedora())

# ── set_tool_output_schema / get_tool_output_schema ─────────────

test_that("set_tool_output_schema attaches schema to a tool", {
  tool1 <- ellmer::tool(
    fun = function() list(auc = 0.9),
    name = "test_output_schema",
    description = "Test tool",
    arguments = list()
  )

  schema <- list(
    type = "object",
    properties = list(
      auc = list(type = "number", description = "AUC value")
    ),
    required = "auc"
  )

  tool2 <- set_tool_output_schema(tool1, schema)

  # Should return the tool (possibly modified)
  expect_s3_class(tool2, "ellmer::ToolDef")

  # get_tool_output_schema should retrieve it
  retrieved <- get_tool_output_schema(tool2)
  expect_type(retrieved, "list")
  expect_equal(retrieved$type, "object")
  expect_equal(retrieved$required, "auc")
})

test_that("set_tool_output_schema errors on non-ToolDef input", {
  expect_error(
    set_tool_output_schema("not a tool", list()),
    class = "cli_abort"
  )
})

test_that("set_tool_output_schema errors on non-list schema", {
  tool1 <- ellmer::tool(
    fun = function() list(),
    name = "test_bad_schema",
    description = "Test",
    arguments = list()
  )
  expect_error(
    set_tool_output_schema(tool1, "not a schema"),
    class = "cli_abort"
  )
})

test_that("get_tool_output_schema returns NULL for tool without schema", {
  tool1 <- ellmer::tool(
    fun = function() list(),
    name = "test_no_schema",
    description = "Test",
    arguments = list()
  )
  expect_null(get_tool_output_schema(tool1))
})

# ── tool_as_json with outputSchema ──────────────────────────────

test_that("tool_as_json includes outputSchema when set", {
  tool1 <- ellmer::tool(
    fun = function() list(auc = 0.92, tss = 0.81),
    name = "eval_model",
    description = "Evaluate",
    arguments = list()
  )

  schema <- list(
    type = "object",
    properties = list(
      auc = list(type = "number"),
      tss = list(type = "number")
    ),
    required = c("auc", "tss")
  )

  tool2 <- set_tool_output_schema(tool1, schema)
  json <- tool_as_json(tool2)

  expect_equal(json$name, "eval_model")
  expect_true("outputSchema" %in% names(json))
  expect_equal(json$outputSchema$type, "object")
  expect_equal(json$outputSchema$required, c("auc", "tss"))
})

test_that("tool_as_json omits outputSchema when not set", {
  tool1 <- ellmer::tool(
    fun = function() "hello",
    name = "no_schema",
    description = "No schema",
    arguments = list()
  )

  json <- tool_as_json(tool1)

  expect_equal(json$name, "no_schema")
  expect_false("outputSchema" %in% names(json))
})

# ── tool_as_json with title ─────────────────────────────────────

test_that("tool_as_json extracts title from annotations", {
  tool1 <- ellmer::tool(
    fun = function() "hello",
    name = "with_title",
    description = "Has title",
    arguments = list(),
    annotations = ellmer::tool_annotations(title = "My Tool Title")
  )

  json <- tool_as_json(tool1)

  expect_equal(json$title, "My Tool Title")
  # title should not be duplicated inside annotations
  if (!is.null(json$annotations)) {
    expect_null(json$annotations$title)
  }
})

test_that("tool_as_json works without title", {
  tool1 <- ellmer::tool(
    fun = function() "hello",
    name = "no_title",
    description = "No title",
    arguments = list()
  )

  json <- tool_as_json(tool1)

  expect_false("title" %in% names(json))
})

# ── as_tool_call_result with structuredContent ──────────────────

test_that("as_tool_call_result includes structuredContent for list results", {
  data <- list(id = 1)
  result <- list(species = "Koala", auc = 0.89, tss = 0.74)

  output <- as_tool_call_result(data, result)

  expect_true("structuredContent" %in% names(output$result))
  expect_equal(output$result$structuredContent$species, "Koala")
  expect_equal(output$result$structuredContent$auc, 0.89)
  # text content still present for backwards compat
  expect_equal(output$result$content[[1]]$type, "text")
})

test_that("as_tool_call_result includes row-oriented structuredContent for data.frames", {
  data <- list(id = 1)
  result <- data.frame(
    species = c("Koala", "Wombat"),
    auc = c(0.89, 0.72),
    stringsAsFactors = FALSE
  )

  output <- as_tool_call_result(data, result)

  expect_true("structuredContent" %in% names(output$result))
  # Should be row-oriented: list of records, not column-oriented
  expect_length(output$result$structuredContent, 2)
  expect_equal(output$result$structuredContent[[1]]$species, "Koala")
  expect_equal(output$result$structuredContent[[1]]$auc, 0.89)
  expect_equal(output$result$structuredContent[[2]]$species, "Wombat")
  expect_equal(output$result$structuredContent[[2]]$auc, 0.72)
})

test_that("as_tool_call_result omits structuredContent for string results", {
  data <- list(id = 1)
  result <- "plain text result"

  output <- as_tool_call_result(data, result)

  expect_false("structuredContent" %in% names(output$result))
  expect_equal(output$result$content[[1]]$text, "plain text result")
})
