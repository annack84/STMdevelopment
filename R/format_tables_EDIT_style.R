#' Format tables in RMarkdown to match EDIT website style
#'
#' @param data Data frame to create table from
#' @param caption Character. Caption for table.
#' @param row.names Logical. Include row names in output table?
#' @param col.names Optional character vector of column names to be used in the table. Defaults to the column names of the data frame.
#'
#' @return Table formatted to match EDIT website
#' @export
#'

format_tables_EDIT_style <- function(data,
                                     caption,
                                     row.names = F,
                                     col.names = NA){
  kableExtra::kbl(x = data, format = "html", row.names = row.names,
                  col.names = col.names, align = "c",
      caption = caption) %>%
    kable_styling(bootstrap_options = "bordered") %>%
    row_spec(kable_input=., row = 0:nrow(data), background = "lightsteelblue")
}
