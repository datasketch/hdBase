

#' @export
hdbase_write <- function(hdb, path = ""){
  if(!is_hdbase(hdb))
    stop("x is not an hdbase")
  hdb$write(path)
}


#' @export
hdbase_read <- function(path, slug = NULL){

  find_base <- list.files(path, pattern = "\\.base\\.json")
  slug <- slug %||% basename(path)
  base_file <- paste0(slug, ".base.json")
  if(! base_file  == find_base)
    stop("base path must be the same as base.json")

  meta_json <- jsonlite::read_json(file.path(path, base_file), simplifyVector = TRUE)

  standard_fields <- c("name", "description", "slug", "formats",
                       "hdbaseType", "hdbaseTypeGroup",
                       "hdtables_slugs",
                       "credits")
  additional_meta <- meta_json[!names(meta_json) %in% standard_fields]

  hdtables_slugs <- meta_json$hdtables_slugs

  hdtables <- purrr::map(hdtables_slugs, ~ hdtable_read(path, slug = .)) |>
    purrr::set_names(hdtables_slugs)

  hdbase(hdtables,
          name = meta_json$name, description = meta_json$description,
          slug = meta_json$slug, format = meta_json$formats,
          meta = additional_meta)

}





