#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Getteur et Setteur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Getteur
setMethod(
  f = "[",
  signature = "AlmEngine",
  definition = function(x,i){
    switch(EXPR = i,
           # Data frame Financier
           "journal_achat_vente" = {return(x@journal_achat_vente)},
           stop("Cet attribut n'existe pas!")
    )
  }
)
# Setteur
setReplaceMethod(
  f = "[",
  signature = "AlmEngine",
  definition = function(x,i,value){
    switch(EXPR = i,
           "journal_achat_vente" = {x@journal_achat_vente <- value},
           stop("Cet attribut n'existe pas!")
    )
    validObject(x)
    return(x)
  }
)

# Constructeur generique
journal <- function(annee = integer(),
                    operation  = "",
                    type_actif = "",
                    num_mp     = integer(),
                    num_index  = integer(),
                    montant    = double(),
                    nb_unit    = double(),
                    pmvr       = double()){
  x <- new("AlmEngine")
  x["journal_achat_vente"] <- data.frame(annee = annee, operation = operation, type_actif = type_actif,
                                         num_mp = num_mp, num_index = num_index, montant = montant , nb_unit = nb_unit, pmvr = pmvr)
  return(x)
}


#
# x <- update_journal(x,journal_achat_vente)
# update(x,journal_achat_vente)
# journal_achat_vente = data.frame(annee = integer(1),
#                                  operation = as.character("achat"),
#                                  type_actif = as.character("action"),
#                                  num_mp = as.integer(4),
#                                  num_index = as.integer(1),
#                                  montant = 50,
#                                  nb_unit = 3.2)
# annee = integer(1),
# operation = as.character("achat"),
# type_actif = as.character("action"),
# num_mp = as.integer(4),
# num_index = as.integer(1),
# montant = 50,
# nb_unit = 3.2
