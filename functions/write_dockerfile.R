#'  Convert the output from `sessionInfo()` into the contents of a Dockerfile
#'
#' @param write_file Defaults to TRUE, which will write a dockerfile. Else,
#' output will be written to console (or whatever connection is open).
#' console (or whatever connection you have open). The dockerfile's name
#' will combine the dir and filename parameters: by default, './Dockerfile'.
#' Writing a file will overwrite any Dockerfile in the same location.
#' 'TRUE' is probably appropriate for exporting to a tool like mybinder.org;
#' 'FALSE ' is appropriate if you already have a Dockerfile and you prefer
#' to paste sections from this function into it.
#' @param org Defaults to Rocker (https://www.rocker-project.org/); another
#' option is Docker's official R images (https://hub.docker.com/_/r-base).
#' For more on Rocker, see https://arxiv.org/abs/1710.03675.
#' @param image Defaults to automatically detect R version and use matching rocker/r-base image.
#' Available tags for r-base are listed here: https://hub.docker.com/r/rocker/r-base/tags.
#' Other Rocker images can be browsed here: https://hub.docker.com/u/rocker.
#' @param apt_packages should standard needed apt packages be installed?
#' Defaults to true and installs libcurl4-openssl-dev, libssl-dev, and
#' libxml2-dev.
#' @param dir Defaults to current directory, './'.
#' @param filename Defaults to 'Dockerfile', making the composite file
#' './Dockerfile'.
#' @param all_packages If TRUE, includes all installed packages. If FALSE (default),
#' only includes currently loaded packages.
#'
#'@importFrom sessioninfo package_info
#'
#' @examples write_dockerfile()
#' @examples write_dockerfile(write_file = FALSE, image = 'r-base:4.4.1')
#' @examples write_dockerfile(all_packages = TRUE) # Include all installed packages
write_dockerfile <- function(write_file = TRUE,
                             org = "rocker",
                             image = NULL,
                             apt_packages = TRUE,
                             dir = './',
                             filename = 'Dockerfile',
                             all_packages = FALSE){

  ## is sessioninfo available?
  if(!"sessioninfo" %in% rownames(installed.packages())){
    stop("please install the library `sessioninfo` before proceeding")
  }

  ## Auto-detect R version and set appropriate image if not specified
  if(is.null(image)){
    r_version <- paste(R.version$major, R.version$minor, sep = ".")
    image <- paste0("r-base:", r_version)
    message("Using detected R version: ", r_version)
  }

  ## Write the Dockerfile as a new file, or save to current connection?
  if(isTRUE(write_file)){
    Dockerfile <- paste0(dir, filename)
    file.create(Dockerfile)
    sink(file = Dockerfile, append = TRUE)
  }

  ## Add header comment with generation info
  cat(paste0("# Dockerfile generated on ", Sys.Date(), "\n"))
  cat(paste0("# R version: ", R.version.string, "\n"))
  cat(paste0("# Platform: ", R.version$platform, "\n\n"))

  ## Top of Dockerfile: add base image
  cat(paste0("FROM ", org, "/", image, "\n\n"))

  ## Add system dependencies
  if(isTRUE(apt_packages)){
    cat("# Install system dependencies\n")
    cat(paste0("RUN apt-get update \\", "\n", "  && apt-get install -y \\", "\n",
               "    libcurl4-openssl-dev \\", "\n",
               "    libssl-dev \\", "\n",
               "    libxml2-dev \\", "\n",
               "    libfontconfig1-dev \\", "\n",  # For some plotting packages
               "    libharfbuzz-dev \\", "\n",     # For text rendering
               "    libfribidi-dev \\", "\n",      # For text shaping
               "  && rm -rf /var/lib/apt/lists/*", "\n\n"))
  }

  ## Get packages - either all installed or just loaded
  if(all_packages){
    pkgs <- sort(rownames(installed.packages()))
    cat("# Installing all packages from current environment\n")
  } else {
    pkgs <- sort(sessioninfo::package_info(pkgs = .packages(),
                                          dependencies = FALSE)$package)
    cat("# Installing currently loaded packages\n")
  }

  if(length(pkgs) != 0){
    cat(paste0("RUN Rscript -e 'if(!require(\"remotes\")) install.packages(\"remotes\")'", "\n\n"))
  }

  ## Check for Bioconductor packages
  if(any(grep('Bioconductor', sessioninfo::package_info()$source))){
    cat(paste0("RUN Rscript -e 'options(warn = 2); if(!require(\"BiocManager\")) install.packages(\"BiocManager\")'", "\n\n"))
  }

  ## Separate packages into CRAN, Bioconductor, and GitHub packages
  cran_packs <- c()
  bioc_packs <- c()
  github_packs <- c()

  for(pkg in pkgs){
    pkg_info <- sessioninfo::package_info(pkg, dependencies = FALSE)
    if(nrow(pkg_info) == 0) next  # Skip if package info not available

    source_info <- pkg_info$source

    if(any(grep("CRAN", source_info))){
      cran_packs <- c(cran_packs, pkg)
    } else if(any(grep("Bioconductor", source_info))){
      bioc_packs <- c(bioc_packs, pkg)
    } else if(any(grep("Github", source_info))){
      github_packs <- c(github_packs, pkg)
    }
  }

  ## Install CRAN packages
  if(length(cran_packs) > 0){
    cat("# Install CRAN packages\n")
    for(pkg in cran_packs){
      tryCatch({
        version <- as.character(getNamespaceVersion(pkg))
        cat(paste0("RUN Rscript -e 'remotes::install_version(package = \"", pkg,
                   "\", version = \"", version, "\")'", "\n"))
      }, error = function(e){
        # If version lookup fails, just install latest
        cat(paste0("RUN Rscript -e 'install.packages(\"", pkg, "\")'", "\n"))
      })
    }
    cat("\n")
  }

  ## Install Bioconductor packages
  if(length(bioc_packs) > 0){
    cat("# Install Bioconductor packages\n")
    cat(paste0("RUN Rscript -e 'options(warn = 2); BiocManager::install(c(",
               paste0('"', bioc_packs, '"', collapse = ", "),
               "))'", "\n\n"))
  }

  ## Install GitHub packages
  if(length(github_packs) > 0){
    cat("# Install GitHub packages\n")
    for(pkg in github_packs){
      pkg_info <- sessioninfo::package_info(pkg, dependencies = FALSE)
      source <- pkg_info$source
      repo <- gsub(pattern = ".*\\((.*)\\).*", replacement = "\\1", source)
      cat(paste0("RUN Rscript -e 'remotes::install_github(\"", repo, "\")'", "\n"))
    }
    cat("\n")
  }

  ## Add working directory and copy instructions as comments
  cat("# Set working directory and copy project files\n")
  cat("# WORKDIR /project\n")
  cat("# COPY . /project\n\n")

  cat("# Default command\n")
  cat("# CMD [\"R\"]\n")

  if(isTRUE(write_file)){
    sink()
    message("Dockerfile written to: ", Dockerfile)
  }

  invisible(NULL)
}
