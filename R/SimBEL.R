#' SimBEL: Un package de calcul du best estimate epargne sous Solvabilite 2.
#'
#' SimBEL fourni un ensemble de fonctionnalites pour permettre l'evaluation d'un best
#' estimate epargne sous Solvabilite 2. L'utilisation de ce package necessite au prealable
#' de disposer de donnees stockees dans un repertoire dont le format est predetermine par
#' la societe Prim'Act. Ce package est developpe a partir d'objet de type S4.
#'
#' Ce package comprends :
#' \itemize{
#' \item une modelisation d'un canton auquel est relie un portefeuille d'actifs et un portefeuille de passif.
#' SimBEL gere les interactions entre ces deux objets.
#' \item une modelisation du best estimate pour des produits d'epargne en euros.
#' \item d'appliquer les principaux chocs de la formule standard.
#' }
#'
#' @docType package
#' @name SimBEL
#' @import rootSolve
NULL
