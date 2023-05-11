
#' @title Create a hdTable data base
#' @description Create a hdBase object from a data frame or a list of data frames.
#' The main value of a hdBase is the metadata associated to a data base.
#'
#' - name: Name for the hdTable data frame, setted on _name_ argument
#' - description: Description for the hdTable data frame, setted on _description_ argument
#' - hdTables: A list of hdTables
#' - slug: a custom slug can be added to the hdBase
#' @param x A data frame
#' @param hdTableType The type of hdTable to create
#' @param dic a custom variable dictionary can be added. [create_dic()] can help you with that.
#' @param name a custom name can be added
#' @param nam a custom description can be added
#' @param slug a custom slug can be added. If not, hdTable will try creating one.
#' @param meta Custom Metadata can be added
#'
#' @examples
#' hdTable(mtcars, hdTableType = "Num", name = "MTCars")
#'
#' @return A hdTable object

#' @export
hdBase <- function(ts,
                   name = NULL,
                   description = NULL,
                   slug = NULL,
                   meta = NULL,
                   dic = NULL,
                   ...){

  if(is.data.frame(ts)){
    hdt <- hdTable(ts, dic = dic,
                   name = name, description = description,
                   slug = slug)
    hdts <- list(hdt)
  } else if(is_hdTable(ts)){
    hdts <- list(ts)
  } else{
    hdts <- ts
  }
  if(!are_hdTables(hdts)){
    stop("Must be a list of hdTables")
  }

  name <- name %||% deparse(substitute(ts))
  meta <- c(meta, list(...))
  if(dstools::is.empty(meta)) meta <- NULL

  if(dstools::is.empty(meta)) meta <- NULL

  hdBaseClass$new(hdts,
                  name = name, description = description,
                  slug = slug,
                  meta = meta)


}
