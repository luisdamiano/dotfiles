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

# Useful interactive-mode functions ---------------------------------------
ulength <- function(x) {
    length(unique(x))
}

unrow   <- function(x) {
    nrow(unique(x))
}

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

RMSE   <- function(x, y) {
  sqrt(mean((x - y)^2))
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

histfd <- function(x, ...) { hist(x, breaks = "FD", ...) }

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
