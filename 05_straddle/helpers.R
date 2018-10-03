library(pipeR)
library(lattice)
library(devtools)
library(quantmod)
library(xts)
library(yaml)
library(data.table)
# load_all("~/Documents/pz")

UNIVERSE <- yaml.load_file("universe.yml")
default_portfolio <- "
AAPL: 1
AMZN: 2
"

load_symbol <- function(symbol, discount_rate=0, ...) {
  s <- getSymbols(symbol, ...)
  inflation_adj <- cumprod(rep_len(1+discount_rate/252, length.out=nrow(s))) %>>% rev
  idx <- Cl(adjustOHLC(s, use.Adjusted = TRUE)) * inflation_adj
  ret <- dailyReturn(idx, type="log")

  return(ret)
}

get_returns <- function(symbols, weights, discount_rate=0, ...) {
  pfo <- Reduce(merge, lapply(symbols, load_symbol, auto.assign=FALSE, discount_rate=discount_rate))

  if (ncol(pfo) > 1) {
    pfo <- reclass(as.matrix(coalesce(pfo, 0)) %*% matrix(weights, ncol=1), pfo)
  }
  
  return(pfo)
}

recognize <- function(x1, x2) {
  if (x1 >= 0) return(x2) # recognize gain (loss if hedge) 
  return(x1 + x2) # otherwise carry forward
}

# substantial_overlap <- function(pfo, hedge, .step = sum(unlist(hedge))*.1, return_table = FALSE) {
#   if (sum(unlist(hedge), na.rm=TRUE) <= 0) return(FALSE)
# 
#   dt.pfo <- data.table(ticker=names(pfo), wt = unlist(pfo)[names(pfo)] %>>% (. / sum(.)), key='ticker')
#   dt.hedge <- data.table(ticker=names(hedge), wt = unlist(hedge)[names(hedge)] %>>% (. / sum(.)), key='ticker')
# 
#   dt <- merge(dt.pfo, dt.hedge, all=TRUE, suffixes = c("_pfo", "_hedge"))
#   dt[, subportfolio := pmin(wt_pfo, wt_hedge) %>>% coalesce(0)]
#   if (return_table) return(dt)
# 
#   overlap <- dt[, sum(subportfolio) / sum(wt_hedge, na.rm=TRUE)]
# 
#   if (overlap >= .7) { return(TRUE) }
#   else { Recall(pfo, Map(function(x) pmax(0, x * (1 - .step / sum(unlist(hedge)))), hedge), .step=.step) } # reduce size of position
# }

substantial_overlap <- function(pfo, pfo_size, hedge, hedge_size) {
  sapply(lapply(10:1, function(x) Map(function(x) x*.1, hedge))
    , function(pfo, hedge) {
      if (sum(unlist(hedge), na.rm=TRUE) <= 0) return(FALSE)
      
      dt.pfo <- data.table(ticker=names(pfo), wt = unlist(pfo)[names(pfo)] %>>% (. / sum(.) * pfo_size), key='ticker')
      dt.hedge <- data.table(ticker=names(hedge), wt = unlist(hedge)[names(hedge)] %>>% (. / sum(.) * hedge_size), key='ticker')
      
      dt <- merge(dt.pfo, dt.hedge, all=TRUE, suffixes = c("_pfo", "_hedge"))
      dt[, subportfolio := pmin(wt_pfo, wt_hedge) %>>% coalesce(0)]
      overlap <- dt[, sum(subportfolio) / sum(wt_hedge, na.rm=TRUE)]
      
      return(overlap)
      # if (overlap >= .7) { return(TRUE) }
      # else { Recall(pfo, Map(function(x) pmax(0, x * (1 - .step / sum(unlist(hedge)))), hedge), .step=.step) } # reduce size of position
  }, pfo=pfo)
}

#' @title coalesce function
#' @description coalesce vectors element at a time. Exactly the same as the
#' COALESCE function available in mysql
#'
#' @param ... vectors
#'
#' @examples
#' vec.A <- c(1, 2, NA, 3, 4, NA, 6)
#' vec.B <- c(2, NA, NA, NA, 7, 8, 9)
#' coalesce(vec.B, vec.A) # c(2, 2, NA, 3, 7, 8, 9)
#' @return singular vector
coalesce <- function(...) {
  lst <- list(...)
  if(length(lst) == 0) return(NULL)
  if(length(lst[[1]]) == 0) return(lst[[1]])
  Reduce(function(x,y) ifelse(is.na(x), y, x), list(...))
}


#' @title Conveniently plot time series data
#' @name plotReturn
#' @description Takes in a data.table of time unit (month, trade days, etc) as the first column and returns per time unit in subsequent columns, one column per stream, and plots out returns in either lattice or ggplot
#' This function is used extensively throughout my code for quickly producing decent looking timeseries plots
#'
#'
#' @param returns data.table with first column being the time index and each subsequent columns being the individual returns per unit time for a stream OR xts time series object
#' @param time_col integer or numeric representing which column is taken as the time column or a character string taken as the name of the column
#' @param facet_col similar to time_col, can be either integer, numeric or character indicating the column in which to facet plot. Defaults to NULL and only applies to lattice plot
#' @param fn.color_scheme function defining color scheme. See gfDevices::heat.colors for more information
#' @param .xlab character string denoting name of x axis (time axis)
#' @param .ylab character string denoting name of y axis
#' @param .combine whether returns should be compounded (default) or summed
#' @param .package character string denoting package to use for plotting, either lattice or ggplot2
#' @param ... lattice::xyplot parameters
#' @import data.table pipeR
#' @importFrom zoo index coredata
#'
#' @details
#' ggplot support is not fully implemented.
#'
#' @return lattice or ggplot
#' @export
plotReturn <- function(returns, ...) {
  UseMethod("plotReturn")
}

#' @rdname plotReturn
#' @export
plotReturn.xts <- function(returns, ...) {
  data.table(date = zoo::index(returns), zoo::coredata(returns)) %>>%
  plotReturn.data.table(...)
}

#' @rdname plotReturn
#' @export
plotReturn.data.table <- function(returns, time_col = 1
  , facet_col = NULL, .combine = c('compound', 'sum', 'identity')
  , fn.color_scheme = pzColors, fn.format_y = formatSi('$', 3, bln.parens=TRUE)
  , .xlab = NULL, .ylab = 'returns', .package = c('lattice', 'ggplot2'), ...) {

  if (class(time_col) %in% c('integer', 'numeric'))
    chr.time_col <- colnames(returns)[time_col]
  else if (class(time_col) %in% c('character'))
    chr.time_col <- time_col[1]
  else
    stop("time_col must either be integer, numeric or character")

  if (!is.null(facet_col)) {
    if (class(facet_col) %in% c('integer', 'numeric'))
      chr.facet_col <- colnames(returns)[facet_col]
    else if (class(facet_col) %in% c('character'))
      chr.facet_col <- facet_col[1]
    else
      stop("facet_col must either be integer, numeric or character")
  } else
    chr.facet_col <- NULL

  color_scheme <- fn.color_scheme(length(colnames(returns)) - 1)

  .dtplot <- melt.data.table(returns, id.vars = c(chr.facet_col, chr.time_col), variable.name = 'stream', value.name = 'ret') %>>%
    setkeyv(c(chr.facet_col, chr.time_col))
  .dtplot[, ret_cum := cumReturn(ret, .combine[1]), by=c(chr.facet_col, 'stream')]

  if (.package[1] == 'lattice') {
    chr.formula <- ifelse(!is.null(chr.facet_col)
      , sprintf('ret_cum ~ as.Date(%s) | %s', chr.time_col, chr.facet_col)
      , sprintf('ret_cum ~ as.Date(%s)', chr.time_col))
    defaultArgs <- list(x = as.formula(chr.formula), data = .dtplot, group = as.name('stream'), type = 'l', as.table = TRUE,
                        xlab = .xlab, ylab = .ylab,
                        lwd = 2,
                        panel = function(...) {
                          panel.grid(h=-1, v=0)
                          panel.xyplot(...)
                        },
                        scales = list(y=list(tck=0), x=list(tck=c(1,0))),
                        # add dollar formatting by default
                        yscale.components = function(...) {
                          ans <- yscale.components.default(...)
                          ans$left$labels$labels <- fn.format_y(ans$left$labels$at)
                          ans
                        },
                        par.settings = list(superpose.line = list(col = color_scheme)),
                        auto.key = if (length(unique(.dtplot$stream)) == 1) FALSE else list(lines = TRUE, points = FALSE)
                  )
    args <- modifyList(defaultArgs, list(...))
    return(do.call(lattice::xyplot, args))
  }

  if (.package[1] == 'ggplot2')
    return(
      ggplot2::ggplot(aes_string(chr.time_col, ret_cum, colour = stream)) +
      ggplot2::geom_line()
    )

  NULL
}

#' @title Format numbers for easier viewing
#' @description Return a function telling lattice how to display axes labels
#' @param chr.prefix prefix to output numbers
#' @param int.signif number of significant figures to display in the output
#' @param bln.parens TRUE: negative numbers will be wrapped in parentheses. FALSE: will use "-"
#' @param chr.suffix character vector corresponding to suffixes used for 1, 1e3, 1e6, 1e9 and 1e12, etc.
#' @return function saving the prefix and number of sigfigs and can be applied
#'
#' @examples
#' formatSi(chr.prefix="$", int.signif=3L)(sample(1e6, 100))
#' @return function
#' @export
formatSi <- function(chr.prefix ='', int.signif = 3L, bln.parens = FALSE
    , chr.suffix = c('', 'k', 'M', 'B', 'T')) {
    function(x) {

        if (max(x, na.rm = TRUE) < 1e3 & !all(x == floor(x), na.rm = TRUE)) {
            return(paste0(chr.prefix, format(x, nsmall = 2L, trim = TRUE, big.mark = ',', scientific = FALSE, digits = 2L)))
        } else {
            digits <- ifelse(x != 0, log10(abs(x)), 0) %/% 3
            x_simple <- signif(abs(x) / 10^(3*digits), int.signif)
            suffix <- chr.suffix[digits + 1]
            if (bln.parens) {
                paste0(ifelse(x < 0, "(", ''), chr.prefix, x_simple, suffix, ifelse(x < 0, ")", ''))
            } else {
                paste0(ifelse(x < 0, '-', ''), chr.prefix, x_simple, suffix)
            }
        }
    }
}

#' @title Hand-picked color palette
#' @description 5 hand-picked colors and remainder randomly selected from colors(distinct = TRUE)
#' @param n integer or numeric specifying the number of colors to be selected
#'
#' @import grDevices
#' @export
pzColors <- function(n) {
  default <- c('firebrick3', 'steelblue', 'forestgreen', 'tan1', 'slategrey')
  if (n <= 5) return(default)

  c(default, sample(setdiff(colors(distinct = TRUE), default), n - 5, rep = FALSE))
}

#' Basic function to calculate cumulative returns
#'
#'
#' @param vec.ret numeric vector of individual returns per unit time
#' @param .combine character string indicates whether individual returns should be compound or sum
#'
#' @return numeric vector
#'
#'
#' @examples
#' vec.month_returns <- rnorm(24, mean = .1, sd = .3)
#' cumReturn(vec.month_returns, .combine = 'compound')
#'
#' @export

cumReturn <- function(vec.ret, .combine = c('compound', 'sum', 'identity')) {
  # replace NA with 0
  vec.ret <- coalesce(vec.ret, 0)
  switch(.combine
    , 'compound' = cumprod(1 + vec.ret)
    , 'sum' = cumsum(vec.ret)
    , 'identity' = vec.ret
    )
}
