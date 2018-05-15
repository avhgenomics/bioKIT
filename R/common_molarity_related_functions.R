
#' Calc grams for Molarity
#'
#' This function, part of a set of sub functions at the moment, calculates the grams required in a given volume to achieve a certain molarity.
#'
#' @param M_target The molarity, given as M. Dbl.
#' @param vol_target The volume, given in liters. Dbl.
#' @param mw The molecular weight of the compound in question. Does not account for hydrated compounds. Dbl.
#' @param sentence Return the calculation as a description? Logical
#' @param compoundID If sentence == T, provide a label or the compound. String.
#' @export
#' @return Returns a string.
#' @examples
#' Mcalc_grams(M_target = 1,vol_target = 0.005,mw = 84.01,sentence = T,compoundID = "NaHCO~3~")
Mcalc_grams <- function(M_target,vol_target,mw,sentence = F,compoundID = NULL){
  grams <- paste(M_target*vol_target*mw,"grams")

  if(sentence){
    sentence.out <- paste("For",M_target,"M",compoundID,"in",vol_target,"liters, add",grams)
    return(sentence.out)
  } else {return(grams)}

}


