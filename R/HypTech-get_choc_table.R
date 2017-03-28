#----------------------------------------------------------------------------------------------------------------------------------------------------
# Methode pour l'application des chocs de mortalite et de longevite
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Applique les chocs de mortalite et de longevite de la formule standard.
##'
##' \code{get_choc_table} est une methode permettant d'appliquer a l'ensemble des table de mortalite
##' d'un objet \code{\link{HypTech}} les chocs de mortalite ou de longevite de la formule standard.
##' @name get_choc_table
##' @docType methods
##' @param x un objet de la classe \code{\link{HypTech}} contenant differentes tables de
##' mortalite.
##' @param choc une valeur \code{numeric} indiquant le taux de choc.
##' @return L'objet \code{x} apres choc.
##' @author Prim'Act
##' @export
##' @include HypTech-class.R
setGeneric("get_choc_table", function(x, choc){standardGeneric("get_choc_table")})
setMethod(
  f = "get_choc_table",
  signature = c(x = "HypTech",  choc = "numeric"),
  def = function(x, choc){

    tables <- x@tables_mort
    nom_tables <- names(tables)
    nb_tables <- length(tables)

    for(i in 1: nb_tables){

      param_mort <- tables[[nom_tables[i]]]
      df_mort <- param_mort@table
      genmin <- param_mort@gen_min
      genmax <- param_mort@gen_max
      agemin <- param_mort@age_min
      agemax <- param_mort@age_max

      for (gen in genmin : genmax)
      {
        for(age in (agemin + 1) : agemax){

          lx_anc <- df_mort[df_mort$gen == gen & df_mort$age == age - 1, "lx"]
          lx_anc_central <- param_mort@table[df_mort$gen == gen & df_mort$age == age - 1, "lx"]
          if (lx_anc == 0 || lx_anc_central == 0) {
            df_mort[df_mort$gen == gen & df_mort$age == age, "lx"] <- 0
          }else{
            qx_choc <- min((calc_qx(param_mort, age - 1, gen) * (1 + choc)), 1)
            df_mort[df_mort$gen == gen & df_mort$age == age, "lx"] <- lx_anc * (1 - qx_choc)
          }
        }
      }

      x@tables_mort[[nom_tables[i]]]@table <-  df_mort
    }

    return(x)}
)
