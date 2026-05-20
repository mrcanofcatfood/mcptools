#' @title Set the output schema on a tool
#'
#' @description
#' Attaches an `outputSchema` (MCP spec 2025-06-18) to a tool created with
#' [ellmer::tool()]. The output schema is a JSON Schema object that describes
#' the expected structure of the tool's return value. Clients that support
#' structured content can use this to validate tool responses automatically.
#'
#' This is a bridge helper: once ellmer adds native `outputSchema` support to
#' `tool()` and `ToolDef`, this function will still work but become unnecessary.
#'
#' @param tool An [ellmer::tool()] object (ToolDef).
#' @param schema A named list representing a JSON Schema object. For example:
#'   `list(type = "object", properties = list(auc = list(type = "number")),
#'   required = "auc")`.
#'
#' @returns The modified tool object with `outputSchema` attached.
#'
#' @examples
#' library(ellmer)
#'
#' my_tool <- tool(
#'   fun = function() list(auc = 0.92, tss = 0.81),
#'   name = "evaluate_model",
#'   description = "Evaluate model performance",
#'   arguments = list()
#' )
#'
#' my_tool <- set_tool_output_schema(my_tool, list(
#'   type = "object",
#'   properties = list(
#'     auc = list(type = "number", description = "Area Under the ROC Curve"),
#'     tss = list(type = "number", description = "True Skill Statistic")
#'   ),
#'   required = c("auc", "tss")
#' ))
#'
#' # Now tools/list will include outputSchema in the response
#' # and tools/call will include structuredContent alongside text content
#'
#' @export
set_tool_output_schema <- function(tool, schema) {
  if (!inherits(tool, "ellmer::ToolDef")) {
    cli::cli_abort("{.arg tool} must be a {.cls ellmer::ToolDef} object.")
  }

  if (!is.list(schema)) {
    cli::cli_abort("{.arg schema} must be a named list (JSON Schema object).")
  }

  # Try setting the S7 property first (works when ellmer adds outputSchema to ToolDef)
  # Fall back to attribute (works with current ellmer)
  tryCatch(
    {
      tool@outputSchema <- schema
      # Clear any stale attribute to avoid divergence
      attr(tool, "outputSchema") <- NULL
    },
    error = function(e) {
      attr(tool, "outputSchema") <<- schema
    }
  )

  tool
}

#' @title Get the output schema from a tool
#'
#' @description
#' Retrieves the `outputSchema` attached to a tool, if any.
#'
#' @param tool An [ellmer::tool()] object (ToolDef).
#'
#' @returns A named list (the JSON Schema), or `NULL` if none is set.
#' @export
get_tool_output_schema <- function(tool) {
  if (!inherits(tool, "ellmer::ToolDef")) {
    cli::cli_abort("{.arg tool} must be a {.cls ellmer::ToolDef} object.")
  }

  schema <- NULL
  tryCatch(
    schema <- tool@outputSchema,
    error = function(e) {
      schema <<- attr(tool, "outputSchema")
    }
  )

  schema
}
