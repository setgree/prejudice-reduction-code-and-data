FROM rocker/r-base:latest

RUN apt-get update \
  && apt-get install -y \
    "libcurl4-openssl-dev" \
    "libssl-dev" \
    "libxml2-dev" \
  && rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'if(!require("remotes")) install.packages("remotes")'

RUN Rscript -e 'remotes::install_version(package = "broom", version = "1.0.10")'
RUN Rscript -e 'remotes::install_version(package = "dplyr", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version(package = "forcats", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version(package = "ggplot2", version = "4.0.0")'
RUN Rscript -e 'remotes::install_version(package = "ggpubr", version = "0.6.2")'
RUN Rscript -e 'remotes::install_version(package = "ggrepel", version = "0.9.6")'
RUN Rscript -e 'remotes::install_version(package = "ggthemes", version = "5.1.0")'
RUN Rscript -e 'remotes::install_version(package = "glue", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version(package = "haven", version = "2.5.5")'
RUN Rscript -e 'remotes::install_version(package = "janitor", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version(package = "kableExtra", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version(package = "knitr", version = "1.50")'
RUN Rscript -e 'remotes::install_version(package = "lubridate", version = "1.9.4")'
RUN Rscript -e 'remotes::install_version(package = "Matrix", version = "1.7-4")'
RUN Rscript -e 'remotes::install_version(package = "metadat", version = "1.4-0")'
RUN Rscript -e 'remotes::install_version(package = "metafor", version = "4.8-0")'
RUN Rscript -e 'remotes::install_version(package = "numDeriv", version = "2016.8-1.1")'
RUN Rscript -e 'remotes::install_version(package = "openxlsx", version = "4.2.8")'
RUN Rscript -e 'remotes::install_version(package = "purrr", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version(package = "readr", version = "2.1.5")'
RUN Rscript -e 'remotes::install_version(package = "readxl", version = "1.4.5")'
RUN Rscript -e 'remotes::install_version(package = "rmarkdown", version = "2.30")'
RUN Rscript -e 'remotes::install_version(package = "rprojroot", version = "2.1.1")'
RUN Rscript -e 'remotes::install_version(package = "scales", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version(package = "sessioninfo", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version(package = "stringr", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version(package = "textclean", version = "0.9.3")'
RUN Rscript -e 'remotes::install_version(package = "tidyr", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version(package = "vtable", version = "1.4.8")'

