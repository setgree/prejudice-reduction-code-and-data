#'  Convert the output from `sessionInfo()` into the contents of a Dockerfile
#'
#' @param write_file Defaults to TRUE, which will write a dockerfile. Else,
#' output will be written to console (or whatever connection is open).
#' console (or whatever connection you have open). The dockerfile's name
#' will combine the dir and filename parameterss: by default, './Dockerfile'.
#' Writing a file will overwrite any Dockerfile in the same location.
#' 'TRUE' is probably appropriate for exporting to a tool like mybinder.org;
#' 'FALSE ' is appropriate if you already have a Dockerfile and you prefer
#' to paste sections from this function into it.
#' @param org Defaults to Rocker (https://www.rocker-project.org/); another
#' option is Docker's official R images (https://hub.docker.com/_/r-base).
#' For more on Rocker, see https://arxiv.org/abs/1710.03675.
#' @param image Defaults to r-base:latest, whose source code is on Rocker's
#' GitHub:
#' https://github.com/rocker-org/rocker/blob/master/r-base/latest/Dockerfile.
#' available tags for r-base are listed here:
#' https://hub.docker.com/r/rocker/r-base/tags.
#' @param apt_packages should standard needed apt packages be installed? 
#' Defaults to true and installs libcurl4-openssl-dev, libssl-dev, and 
#' libxml2-dev.
#' Other Rocker images can be browsed here: https://hub.docker.com/u/rocker.
#' @param dir Defaults to current directory, './'.
#' @param filename Defaults to 'Dockerfile', making the composite file
#' './Dockerfile'.
#'
#'@importFrom sessioninfo package_info
#'
#' @examples write_dockerfile()
#' @examples write_dockerfile(write_file = FALSE, image = 'geospatial')
write_dockerfile <- function(write_file = TRUE,
                             org = "rocker",
                             image = "r-base:latest",
                             apt_packages = TRUE,
                             dir = './',
                             filename = 'Dockerfile'){
  
  ## is sessioninfo available? 
  if("sessioninfo" %in% rownames(installed.packages()) == FALSE){
    stop("please install the library `sessioninfo` before proceeding")
  }
  
  ## Write the Dockerfile as a new file, or save to current connection?
  if(isTRUE(write_file)){
    Dockerfile = paste0(dir, filename)
    file.create(Dockerfile)
    sink(file = Dockerfile, append = T)
  }
  
  
  ## Top of Dockerfile: add base image; 
  ## if apt_packages=T add standard apt packages
  ## if len(pkgs) !=0, add  and 'remotes' 
  cat(paste0("FROM ", org, "/", image, "\n", "\n"))
  
  if(isTRUE(apt_packages)){
    cat(paste0("RUN apt-get update \\", "\n", "  && apt-get install -y \\", "\n",
               "    \"libcurl4-openssl-dev\" \\", "\n",
               "    \"libssl-dev\" \\", "\n",
               "    \"libxml2-dev\" \\", "\n",
               "  && rm -rf /var/lib/apt/lists/*", "\n", "\n"))
  }
  
  pkgs <- sort(sessioninfo::package_info((pkgs =.packages()), 
                                         dependencies = F)$package)
  if(length(pkgs) != 0){
    cat(paste0("RUN Rscript -e 'if(!require(\"remotes\")) install.packages(\"remotes\")'", "\n"))
  }
  if(any(grep('Bioconductor', sessioninfo::package_info()$source))){
    cat(paste0("RUN Rscript -e 'options(warn = 2); if(!require(\"BiocManager\")) install.packages(\"BiocManager\")'", "\n"))
  }
  
  ## Separate packages into CRAN, Bioconductor, and GitHub packages
  cran_packs <- list()
  bioc_packs <- list()
  github_packs <- list()
  
  for(pkg in pkgs){
    if(any(grep("CRAN",
                sessioninfo::package_info(pkg, dependencies = FALSE)$source))){
      cran_packs <- c(cran_packs, pkg)}
    else if(any(grep("Bioconductor",
                     sessioninfo::package_info(pkg, dependencies = FALSE)$source))){
      bioc_packs <- c(bioc_packs, pkg)
    }
    else if(any(grep("Github",
                     sessioninfo::package_info(pkg, dependencies = FALSE)$source))){
      github_packs <- c(github_packs, pkg)
    }
  }
  
  ### add one space after initial installation lines
  cat(paste0("\n"))
  
  ### install CRAN packages
  for(pkg in cran_packs){
    cat(paste0(noquote("RUN Rscript -e '"),
               noquote("remotes::install_version(package = "), '"', pkg, '"',
               ",", " version = ", '"', getNamespaceVersion(pkg), '"', ")", "'", "\n"))
  }
  if(length(cran_packs) != 0) {cat(paste0("\n"))}
  
  ### install Bioconductor packages, iff there are bioconductor packages
  if(length(bioc_packs) != 0){
    cat(paste0(noquote("RUN Rscript -e '"), noquote("options(warn = 2); "),
               noquote("BiocManager::install(c(" ), " ", "\\", "\n", "  ",  '"'))
    cat(paste(bioc_packs, collapse='", \\ \n  "'))
    cat(paste0('"', " \\", "\n", '  )', ')', "'", "\n"))
  }
  if(length(bioc_packs) != 0){cat(paste0("\n"))}
  
  ### install GitHub packages iff there are GitHub packages
  for(pkg in github_packs){
    source <- sessioninfo::package_info(pkg, dependencies = F)$source
    cat(paste0(noquote("RUN Rscript -e '"),
               noquote("remotes::install_github(repo = "), '"',
               gsub(pattern = ".*\\((.*)\\).*", replacement =  "\\1", source),
               '"', ")", "'", "\n"))
  }
  
  if(isTRUE(write_file)){
    sink()
  }
}
