#' Calculate OR
#' @export
OR <- function(x, ...) UseMethod("OR")

#' @rdname OR
#' @method OR table
#' @param table a 2 x 2 table
#' @export
OR.table <- function(table) {
  list(table = table, OR = crossmul(table))
}

#' @rdname OR
#' @method OR formula
#' @param formula a formula, see details
#' @param data data.frame containing variables formula refers to
#' @details
#' Formula should be specified as outcome ~ exposure | strata1 + strata2...
#' @examples
#' OR(prevhosp ~ methicse | agecat, nilton)
#' OR(prevhosp ~ methicse | agecat + preantbu, nilton)
#' @export
OR.formula <- function(formula, data) {
  checksum <- function(call, type) {
    # helper function to check that a call is a simple sum
    # returns the names of any terms in sum if valid
    n <- all.names(call)
    l <- length(n)
    if(l == 1) return(n)
    if(sum(n == "+") != l %/% 2) stop(paste(type, "terms must be a simple sum of variable names."))
    else tail(n, l - (l %/% 2))
  }
  exposure <- NULL # this will hold the exposure var
  strata   <- NULL # this will hold the control vars
  # parse formula at first level
  lhs <- all.names(formula[[2]])
  rhs <- formula[[3]]
  if(length(lhs) != 1) stop("Exactly one outcome variable must be specified.")
  if(length(rhs) > 1) {
    # are there strata?
    if(rhs[[1]] == '|') {
      exposure <- rhs[[2]]
      strata   <- checksum(rhs[[3]], "Control variable")
    } else stop("Formula misspecified. Should be of the form: out ~ exp | control1 + control2")
  } else {
    exposure <- rhs
  }
  if(length(exposure) > 1) stop("Exactly one exposure variable must be specifed.")
  exposure <- all.names(exposure)
  tab <- table(data[c(lhs, exposure)])
  out <- list(crude = OR(tab))

  if(!is.null(strata)) {
    s.df       <- data[c(lhs, exposure, strata)]
    s.df       <- lapply(s.df, as.factor)
    s.tab      <- table(s.df)
    s.levels   <- expand.grid(tail(dimnames(s.tab), -2))  # remove the first two axes
    strata.out <- list(strata = list())
    for(i in 1:NROW(s.levels)) {
      row <- s.levels[i, , drop = FALSE]
      strata.out$strata[[strata.name(row)]] <- OR(as.table(strata.extract(row, s.tab)))
    }
    out <- c(out, strata.out)

    # do the Mantel-Haenszel
    mh.arr <- array(NA, dim = c(2, 2, NROW(s.levels)))
    for(i in 1:NROW(s.levels)) {
      mh.arr[1:2, 1:2, i] <- strata.out$strata[[i]]$table
    }
    out <- c(out, list(adj = list(OR = unname(mantelhaen.test(mh.arr)$estimate))))

    out <- c(out, list(bd = breslow_day(mh.arr, out$adj$OR)))
  }

  class(out) <- c("aepi.or", "list")
  out
}

#' @export
print.aepi.or <- function(x, ...) {
  cat(sprintf("Crude OR: %.2f\n", x$crude$OR))
  if(!is.null(x$strata)) {
    cat("Stratum specific ORs:\n")
    n <- names(x$strata)
    cat(sapply(seq_along(n), function(i) sprintf("\t%s: %.2f\n", n[i], x$strata[[i]]$OR)))
    cat(sprintf("Adjusted OR: %.2f\n", x$adj$OR))
    cat(sprintf("Breslow-Day p: %.3f\n", x$bd$p))
  }
}
