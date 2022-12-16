# Global settings ---------------------------------------------------------
## Add post-mortem debugging facilities
# https://wiki.archlinux.org/index.php/R#Profile
error <- quote(dump.frames("${R_HOME_USER}/testdump", TRUE))

## Set CRAN mirror:
# https://wiki.archlinux.org/index.php/R#Set_CRAN_mirror_across_R_sessions
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://mirror.las.iastate.edu/CRAN/" # "https://cloud.r-project.org/"
  options(repos = r)
})

## Disable graphical menu
options(menu.graphics = FALSE)

# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/traceback
# Print the stack trace at the time of the error
options(error = function() recover())

## Add timestamp to console
#https://stackoverflow.com/a/4225446/2860744
invisible(
  addTaskCallback(
    function(expr, value, ok, visible) {
      options("prompt" = format(Sys.time(), "%H:%M:%S > "));
      return(TRUE)
    },
    name = "simpleHandler"
  )
)

# Shortcuts --------------------------------------------------------------------
grepv  <- function(...) {
    grep(..., value = TRUE)
}

pasten <- function(...) {
    paste(..., sep = "\n")
}

ulength <- function(x) {
    length(unique(x))
}

unrow   <- function(x) {
    nrow(unique(x))
}

RMSE   <- function(x, y) {
  sqrt(mean((x - y)^2))
}

histfd <- function(x, ...) {
  hist(x, breaks = "FD", ...)
}

# Useful interactive-mode functions ---------------------------------------
posize  <- function(x = NULL) {
  if (!is.null(x))
    return(print(object.size(x), units = "auto"))

  osizes <- sapply(ls(.GlobalEnv), function(li) { object.size(get(li)) })

  round(osizes[rev(order(unlist(osizes)))] / 1000000, 1)
}

qView  <- function(x) {
  f <- tempfile(fileext = ".csv")
  data.table::fwrite(x, file = f)
  system(sprintf("xdg-open %s", f))
}

plapply <- function(X, FUN, ..., cl = "allBut1") {
  cl0 <- cl
  if (cl == "allBut1")
    cl0 <- parallel::detectCores() - 1

  pbapply::pblapply(X, FUN, ..., cl = cl0)
}

# https://stackoverflow.com/a/45344288/2860744
pdf_all <- function(plotLs, ...) {
  pdf(...); plotLs; dev.off()
}

dbg    <- function(x = .Last.value) { str(x); data.table::address(x)}

gimme_a_grid <- function(x, y, z, resolution = 30) {
  f     <- function(u) { seq(min(u), max(u), len = resolution) }

  fit   <- loess(z ~ x + y)
  grdDF <- expand.grid(x = f(x), y = f(y))
  grdDF$prd <- predict(fit, data.frame(grdDF))

  return(grdDF)
}

dataframe2roxygen <- function(DT, source = NULL) {
  title <- "Title"
  desc  <- "Description"

  summarize_vector <- function(x) {
    if (typeof(x) %in% c("character")) {
      u <- sort(unique(x))
      if (length(u) < 11)
        return(paste(u, collapse = ", "))

      return(paste(c(u[1:5], "and more"), collapse = ", "))
    }

    if (typeof(x) %in% c("numeric")) {
      return(range(x))
    }
  }

  p_ <- Vectorize(function(item, type, summary) {
    sprintf("\\t\\item\\{%s\\}\\{%s\\t%s\\}", item, type, summary)
  })

  items <- names(DF)
  type  <- apply(DF, 2, typeof)
  sumry <- apply(DF, 2, summarize_vector)
  form0 <- sprintf("@format A data.frame iwth %d rows and %d columns\\n",
                   nrow(DF), ncol(DF))
  form1 <- sprintf("\\describe\\{\n\\%s\n\\}",
                  paste(p_(items, type, sumry), collapse = "\n"))
  src   <- sprintf("@source <%s>", source)
  return(form)
}

mem_used <- function(print = TRUE) {
  # https://github.com/cran/lobstr/blob/3d7749df14d430f8a1dcab618d196013cf9c1cd9/R/mem.R#L21
  # nodeSize  <- switch(
  #   as.character((8L * .Machine$sizeof.pointer)),
  #   "32" = 28L, "64" = 56L, stop("Unknown architecture"))
  # usedBytes <- sum(gc()[, 1] * c(nodeSize, 8))
  # print(structure(usedBytes, class = "object_size"), units = "auto")
  x <- sum(gc()[, 2])
  if (print) cat(sprintf("%g Mb", x))
  invisible(x)
}

install_packages_I_use_often <- function() {
  reqs <- c("akima", "coda", "data.table", "devtools", "digest", "docopt",
            "doParallel", "emmeans", "fda", "FNN", "foreach", "GGally", "ggh4x",
            "ggplot2", "ggpubr", "hetGP", "jsonlite", "lme4", "logging",
            "matrixcalc", "memoise", "mlegp", "mvtnorm", "pbapply", "pracma",
            "rmarkdown", "xtable")
  pkgs <- installed.packages()[, 1]

  for (req in reqs)
    if (!(req %in% pkgs))
      install.packages(req)
}

# General purpose functions that should go into a package one day --------------
#' Create a two-way table with a summary statistics
#'
#' @param x an object
#' @param by a list of grouping elements, each as long as `x`
#' @param FUN a function to compute the summary statistics which can be applied
#' to all data subsets.
#'
#' @return A matrix with named dimensions
#' @export
#'
#' @examples
#' aggtable(mtcars$mpg, mtcars[, c("carb", "gear")], mean)
aggtable <- function(x, by, FUN) {
  addmargins(
    xtabs(x ~ ., aggregate(x, by, FUN), subset = seq(x),
          drop.unused.levels = TRUE),
    FUN = FUN, quiet = TRUE)
}

#' Apply a function to all combinations of variables
#'
#' @param f function taking as many arguments as passed in `...`
#' @param ...vectors, factors or a list containing these
#'
#' @return A list with the result returned by `f` applied to each combination
#' of all the vectors supplied in the ... arguments
#' @export
#' @examples
#' x = month.name
#' y = 2022:2025
#' z = 1:5
#' myfun <- function(x1, x2, x3) { paste(x1, x2, x3) }
#' xpply(myfun, x, y, z)
xpply <- function(f, ...) {
  # Use expand.grid to create a list with all combinations of ... args
  xList <- as.list(do.call(expand.grid, list(stringsAsFactors = FALSE, ...)))

  # Use Map to apply f over all combinations
  do.call(Map, c(list(f = f), unname(xList)))
}

#' Cut a vector into quantile intervals and produce a box plot
#'
#' @param x a numeric vector which is to be converted to a factor by cutting
#' @param nBins number of intervals
#' @param ... arguments passed to boxplot
#' @return see ?boxplot
#' @export
#' @examples cutplot(mtcars$mpg, 4)
#' @examples cutplot(with(data.frame(x = 1:100), rnorm(100, x, x)))
#' @examples cutplot(rexp(100, .3))
cutplot <- function(x, nBins = 10, ...) {
  g <- cut(x, breaks = unique(quantile(x, probs = seq(0, 1, 1 / nBins))))
  boxplot(x ~ g, at = tapply(x, g, mean), las = 3, ...)
}
