# Dockerfile generated on 2025-11-10
# R version: R version 4.4.1 (2024-06-14)
# Platform: aarch64-apple-darwin20

FROM rocker/r-base:4.4.1

# Install system dependencies
RUN apt-get update \
  && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
  && rm -rf /var/lib/apt/lists/*

# Installing currently loaded packages
RUN Rscript -e 'if(!require("remotes")) install.packages("remotes")'

# Install CRAN packages
RUN Rscript -e 'remotes::install_version(package = "dplyr", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version(package = "ggplot2", version = "4.0.0")'
RUN Rscript -e 'remotes::install_version(package = "knitr", version = "1.50")'
RUN Rscript -e 'remotes::install_version(package = "Matrix", version = "1.7-4")'
RUN Rscript -e 'remotes::install_version(package = "metadat", version = "1.4-0")'
RUN Rscript -e 'remotes::install_version(package = "metafor", version = "4.8-0")'
RUN Rscript -e 'remotes::install_version(package = "numDeriv", version = "2016.8-1.1")'
RUN Rscript -e 'remotes::install_version(package = "purrr", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version(package = "readr", version = "2.1.5")'
RUN Rscript -e 'remotes::install_version(package = "rmarkdown", version = "2.30")'
RUN Rscript -e 'remotes::install_version(package = "tidyr", version = "1.3.1")'

# Set working directory and copy project files
# WORKDIR /project
# COPY . /project

# Default command
# CMD ["R"]
