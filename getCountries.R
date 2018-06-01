library(countrycode)


#' Title
#'
#' @param x Character vector with text
#'
#' @return For each element in x, the country code of the first country mentioned in the text
#' @export
#'
#' @examples
getCountries <- function(x){
  # look for countries in the text
  # Only the first country found in the text will be returned
  retCntry <- character(length(x))
  hitpos <- rep(Inf,length(x))
  # for each country
  for(i in 1:nrow(codelist)){
    cntry_regexpr <- regexpr(codelist$country.name.en.regex[i], 
                             x, ignore.case = T,perl=T)
    # which lines of texts has a hit
    idxHits <- na.omit(which(cntry_regexpr != -1))
    # Only keep hits if that are before a prior hit
    idxHits <- idxHits[cntry_regexpr[idxHits] < hitpos[idxHits]]
    # Store the country and position of the hits
    retCntry[idxHits] <- rep(codelist$iso3c[i],length(idxHits))
    hitpos[idxHits] <- cntry_regexpr[idxHits]
  }
  return(retCntry)
}