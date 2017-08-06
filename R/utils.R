#' @keywords internal
read.archive <- function(repo) {
  tryCatch({
    con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds",
                             repo), "rb"))
    on.exit(close(con))
    readRDS(con)
  }, warning = function(e) {
    list()
  }, error = function(e) {
    list()
  })
}


#' @keywords internal
pkg.info <- function (package, repos) {
  archive.info <- function(repo) {
    archive <- read.archive(repo)
    info <- archive[[package]]
    if (!is.null(info)) {
      info$repo <- repo
      info$path <- rownames(info)
      info
    }
  }
  res <- do.call(rbind.data.frame, c(lapply(repos, archive.info),
                                     list(make.row.names = FALSE)))
  if (NROW(res) == 0) {
    NULL
  } else {
    res[order(res$path, res$mtime), ]
  }
}
