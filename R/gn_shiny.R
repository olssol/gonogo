#' Run Web-Based application
#'
#' Call Shiny to run \code{statidea} as a web-based application.
#'
#' @details
#'
#' A web browser will be brought up for users to access the GUI
#'
#'
#' @export
#'
gn_shiny <- function(appname = "shiny", pkgname = "gonogo") {

    req_pkgs        <- c("shiny", "shinythemes", "DT",
                         "knitr", "rmarkdown", "pander");
    chk_uninstalled <- sapply(req_pkgs,
                              function(x) {
        !requireNamespace(x, quietly = TRUE)
    })

    chk_inx <- which(chk_uninstalled)
    if (0 < length(chk_inx)) {
        msg <- paste("For the GUI to work, please install ",
                     ifelse(1 < length(chk_inx),
                            "packages ",
                            "package "),

                     paste(req_pkgs[chk_inx], collapse = ", "),
                     " by \n install.packages(",
                     paste(paste("'",
                                 req_pkgs[chk_inx],
                                 "'",
                                 sep = ""), collapse = ", "),
                     ") \n  ",
                     sep = "")
        stop(msg, call. = FALSE)
    }

    app_dir <- system.file(appname,
                          package = pkgname)

    if (app_dir == "") {
        stop(paste("Could not find Shiny directory. Try re-installing `",
                   pkgname,
                   "`."),
             call. = FALSE)
    }

    shiny::runApp(app_dir, display.mode = "normal")
}
