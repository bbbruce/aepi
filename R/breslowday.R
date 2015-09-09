#' Breslow-Day Test
#'
#' @param x a 2 by 2 by K strata array
#' @param OR if specified used instead of the Mantel-Haenszel
#' @param exact if TRUE use the hypergeometric distribution directly
#' @source Breslow NE, Day NE. Statistical methods in cancer research. Volume I - The analysis of case-control studies. IARC Sci Publ. 1980;(32):5-338.
#' @export
breslow_day <- function(x, OR = NULL, exact = FALSE) {
  # If OR not specified use MH
  if(is.null(OR)) OR <- mantelhaen.test(x)$estimate

  K <- dim(x)[3] # Number of strata
  X2 <- 0        # chi square

  for (k in 1:K) {
    # calculate margins
    n <- apply(x[, , k], 1, sum)
    m <- apply(x[, , k], 2, sum)

    if(exact) {
      # use the hypergeometric distribution directly
      X2 <- X2 + (x[1, 1, k] -
                    BiasedUrn::meanFNCHypergeo(m[1], m[2], n[1], OR))^2 /
        BiasedUrn::varFNCHypergeo(m[1], m[2], n[1], OR)
    } else {
      # use the normal approximation, a quadratic equation
      # Breslow & Day (1980) pg. 130
      a <- OR - 1
      b <- -(m[1] + n[1]) * OR - (n[2] - m[1])
      c <- n[1] * m[1] * OR
      roots <- Re(polyroot(c(c, b, a)))
      Ea <- roots[max(0, m[1] - n[2]) <= roots & roots <= min(m[1],  n[1])]

      # calculate the rest of the cells for variance calc
      Eb <- m[1] - Ea
      Ec <- n[1] - Ea
      Ed <- m[2] - Ec
      Va <- 1 / sum(1 / c(Ea, Eb, Ec, Ed))

      X2 <- X2 + (x[1, 1, k] - Ea)^2 / Va
    }
  }

  list(chisq = X2, df = K - 1, p = 1 - pchisq(X2, K - 1))
}
