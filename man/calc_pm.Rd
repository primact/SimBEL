% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PassifBase-calc_pm.R
\docType{methods}
\name{calc_pm}
\alias{calc_pm}
\title{Calcul les PM pour des contrats epargne en euros et les contrats de retraite euro en phases de restitution.}
\usage{
calc_pm(x, method, an, tx_cible, y)
}
\arguments{
\item{x}{un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros ou
retraite euro en phases de restitution.}

\item{method}{un \code{character} prenant pour valeur \code{normal} pour le calcul des flux avec application de la revalorisation au titre de
la participation aux benefices, et la valeur \code{gar} pour le calcul avec uniquement les flux garanti (calcul de la FDB).}

\item{an}{une valeur \code{integer} correspondant a l'annee du calcul des prestations.}

\item{tx_cible}{une liste conteant les taux cibles annuel et semestriel par model points.
Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_cible}}}

\item{y}{une liste a remplir uniquement si \code{x} est de type \code{\link{EpEuroInd}} contenant les parametres :
\describe{
\item{\code{tab_prime} : }{une liste contenant les flux de primes pour chaque ligne de model points.
Le format de cette liste correspond a la sortie \code{flux} de la methode \code{\link{calc_primes}}.
A remplir uniquement si l'objet \code{x} est de type \code{\link{EpEuroInd}}.}
\item{\code{tab_prest} : }{une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
Le format de cette liste correspond a la sortie \code{flux} de la methode \code{\link{calc_prest}}.
A remplir uniquement si \code{x} est de type \code{\link{EpEuroInd}}.}
\item{\code{tx_min} : }{une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
 Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_min}}.
 A remplir uniquement si \code{x} est de type \code{\link{EpEuroInd}}.}
\item{\code{tx_soc} : }{est une valeur \code{numeric} correspondant au taux de prelevements sociaux \code{\link{EpEuroInd}}.}
}}
}
\value{
Une liste contenant :
\describe{
\item{\code{method} : }{la valeur de l'argument \code{method}.}
\item{\code{flux} : }{une liste comprenant les flux de l'annee.}
\item{\code{stock} : }{une liste comprenant les nombres de sorties.}
}

Le format de la liste \code{flux} est :
\describe{
\item{\code{rev_stock_brut} : }{un vecteur contenant la revalorisation minimale
brute de l'annee appliquee au PM (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{rev_stock_nette} : }{un vecteur contenant la revalorisation minimale
nette de l'annee appliquee au PM (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{enc_charg_stock} : }{un vecteur contenant les chargement sur encours de l'annee,
calcules en prenant en compte la revalorisation minimale (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{enc_charg_base_th} : }{un vecteur contenant les chargements sur encours theoriques
de l'annee, evalues sur la base de la PM non revalorisees (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{enc_charg_rmin_th} : }{un vecteur contenant les chargements sur encours theoriques
 de l'annee, evalues sur la seule base de la revalorisation minimale des PM (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{base_enc_th} : }{un vecteur contenant l'assiette de calcul des chargements
 sur encours de l'annee (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{soc_stock} : }{un vecteur contenant le prelevements sociaux de l'annee
 (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{it_tech_stock} : }{un vecteur contenant les interets
techniques sur stock de l'annee (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{it_tech} : }{un vecteur contenant les interets techniques sur stock et
sur prestations de l'annee (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
\item{\code{bes_tx_cible} : }{un vecteur contenant le besoin de financement de l'annee pour
atteindre le taux cible de chaque assure.}
}

Le format de la liste \code{stock} est :
\describe{
\item{\code{pm_deb : }}{un vecteur contenant le montant de PM en debut d'annee}
\item{\code{pm_fin : }}{un vecteur contenant le montant de PM en fin d'annee, avec
revalorisation au taux minimum}
\item{\code{pm_moy : }}{un vecteur contenant le montant de PM moyenne sur l'annee.}
}
}
\description{
\code{calc_pm} est une methode permettant de calculer les provisions mathematiques (PM)
 de fin de periode avant application de la revalorisation au titre de la participation aux benefices.
}
\details{
En epargne, cette methode permet de calculer les montants de PM de fin d'annee, avec une revalorisation
minimale pour les inputs. Les chargements sur encours sont egalement preleves. Cette methode permet de gerer les contrats a taux de
revalorisation net negatif. Cette methode permet egalement de calculer le besoin de financement necessaire
pour atteindre les exigences de revalorisation des assures.
Pour la  retraite, cette methode renvoie les elements de PM ainsi que le besoin de financement afferent.
}
\seealso{
\code{\link{calc_primes}}, \code{\link{calc_prest}}, \code{\link{calc_tx_cible}},
\code{\link{calc_tx_min}}.
}
\author{
Prim'Act
}
