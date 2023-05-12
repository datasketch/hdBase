

#' @export
hdBase_write <- function(hdb, path = ""){
  if(!is_hdBase(hdb))
    stop("x is not an hdBase")
  hdb$write(path)
}


#' @export
hdBase_read <- function(path, slug = NULL){

  find_base <- list.files(path, pattern = "\\.base\\.json")
  slug <- slug %||% basename(path)
  base_file <- paste0(slug, ".base.json")
  if(! base_file  == find_base)
    stop("base path must be the same as base.json")

  meta_json <- jsonlite::read_json(file.path(path, base_file), simplifyVector = TRUE)

  standard_fields <- c("name", "description", "slug", "formats",
                       "hdBaseType", "hdBaseTypeGroup",
                       "hdTables_slugs",
                       "credits")
  additional_meta <- meta_json[!names(meta_json) %in% standard_fields]

  hdTables_slugs <- meta_json$hdTables_slugs

  hdTables <- purrr::map(hdTables_slugs, ~ hdTable_read(path, slug = .)) |>
    purrr::set_names(hdTables_slugs)

  hdBase(hdTables,
          name = meta_json$name, description = meta_json$description,
          slug = meta_json$slug, format = meta_json$formats,
          meta = additional_meta)

}





