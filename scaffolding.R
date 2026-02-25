library(devtools)
library(jsonlite)
library(pkgdown)
install_cellar <- function(path = ".") {
    cellar <- renv:::renv_paths_cellar()
    if (!dir.exists(cellar)) {
        dir.create(cellar)
    }
    pkgbuild::build(path, dest_path = cellar)
}