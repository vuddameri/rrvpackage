#' computes reliability, risk, resilience, vulnerability from drought index dataset
#' 
#' @author Venki Uddameri, \email{venki.uddameri@@ttu.edu}
#' @references Hashimoto, T., Stedinger, J.R. and Loucks, D.P., 1982. Reliability, resiliency, and vulnerability criteria for water resource system performance evaluation. Water resources research, 18(1), pp.14-20. 
#' \url{http://pure.iiasa.ac.at/id/eprint/14077/1/wrcr3066.pdf}\url{http://pure.iiasa.ac.at/id/eprint/14077/1/wrcr3066.pdf}
#' @param X a vector of sequential drought index values (numeric)
#' @param cutoff numeric value indicating the drought cutoff for the index
#'
#' @return rrvx reliability, risk, resilience, vulnerability
#'
#' @examples
#' rrv(rnorm(1000),cutoff=-0.7)
#' @importFrom stats na.omit
#' @export
#' 
# Function rrv to compute risk, resilience
# vulnerability measures with drought indicators
# Venki Uddameri, 02/20/2021
rrv <- function(X,cutoff)
{
  # Remove any NA values
  # remove 1st two months as they are NAs
  X <- na.omit(X) 
  # create a binary SPI time-series
  # Drought = 0; No-Drought = 1
  spi3.bin <- ifelse(X < cutoff,0,1)
  
  # Compute reliability and risk
  rel.lbb <- sum(spi3.bin)/length(spi3.bin)
  risk.lbb <- sum(1-spi3.bin)/length(spi3.bin)
  
  # Compute Resilience
  Xt1 <- c(spi3.bin[2:length(spi3.bin)],NA)
  zz <- cbind(spi3.bin,Xt1)
  zz <- na.omit(zz)
  Psum <- ifelse(zz[,1]==0 & zz[,2]==1,1,0)
  Psumx <- sum(Psum)/(nrow(zz))
  res <- Psumx/risk.lbb
  
  # Compute Vulnerability
  nbin <- 1 - spi3.bin
  vul <- sum(nbin * abs(X))/(sum(nbin))
  rrvx <- c(rel.lbb,risk.lbb,res,vul)
  rrvx <- round(rrvx,4)
  names(rrvx) <- c('Reliability','Risk','Resilience','Vulnerabiliy')
  return(rrvx)
  }