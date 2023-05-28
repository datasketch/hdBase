

hdbase_hdtables_meta <- function(hb){

  if(!is_hdbase(hb))
    stop("Not an hdbase")
  hb$hdtables_meta()
}

