# Dockerfile for Arrowtooth flounder project includes csasdown, ADMB, iSCAM, and INLA
FROM cgrandin/csasdown-admb-iscam:latest
LABEL maintainer = "chris.grandin@gmail.com"

# Install Linux packages needed for various R packages to run including INLA
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxt6 \
    tk8.6-dev \
    libgeos-dev \
    libudunits2-dev \
    libgdal-dev \
    libv8-dev \
    cargo

COPY .Renviron .Renviron
COPY install_packages.R install_packages.R
RUN Rscript install_packages.R

ENV INST=/usr/bin
ENV ADMB_HOME=$INST/admb/build/admb
ENV ADMB_AD2CSV=$INST/admb/contrib/ad2csv
ENV ISCAM_BASE=$INST/gfiscam
ENV ISCAM_HOME=$ISCAM_BASE/build/dist/bin
ENV PATH=$ISCAM_BASE:$ISCAM_HOME:$ADMB_AD2CSV:$ADMB_HOME/bin:$HOME:$PATH
