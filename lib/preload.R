library(dplyr)
library(lmtest)
library(sandwich)
library(texreg) # https://github.com/mrpg/texreg_fork @ 6268c7f

tt <- function (models, no_resize = F, placement = "", siunitx = T, ...) {
    out <- gsub("\\begin{table}", paste0("\\begin{table}", placement), fixed = T,
                gsub("table-text-alignment=right", "table-text-alignment=center", fixed = T,
                     gsub(" (\\d+) ", "{\\1}",
                          texreg(
                                 models,
                                 label = deparse(substitute(models)),
                                 booktabs = T,
                                 threeparttable = T,
                                 siunitx = siunitx,
                                 digits = 3,
                                 use.packages = F,
                                 ...
                          )
                     )
                )
    )

    if (!no_resize) {
        gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{%\n\\begin{tabular}", fixed = T,
             gsub("end{tabular}", "end{tabular}}", fixed = T,
                  out
             )
        )
    }
    else {
        out
    }
}

save_to <- function(extension, ...) {
    args <- list(...)
    arg_names <- sapply(substitute(list(...))[-1], deparse)

    for (i in seq_along(args)) {
        file_name <- paste0("output/", arg_names[i], ".", extension)

        writeLines(args[[i]], file_name)
    }
}

robustly <- function (models, column) {
    out <- list()

    for (i in 1:length(models)) {
        model <- models[[i]]

        out[[i]] <- coeftest(model, vcovHC(model, type = "HC3"))[, column]
    }

    out
}

standard <- function (models, column) {
    out <- list()

    for (i in 1:length(models)) {
        model <- models[[i]]

        out[[i]] <- coef(summary(model))[, column]
    }

    out
}

clustered <- function (models, column, ...) {
    out <- list()

    for (i in 1:length(models)) {
        model <- models[[i]]

        out[[i]] <- coeftest(model, vcovCL(model, ..., type = "HC3"))[, column]
    }

    out
}

ci <- function (b, ix = ix) {
    prop.test(sum(b), length(b))$conf.int[ix]
}
