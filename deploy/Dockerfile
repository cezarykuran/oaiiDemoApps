ARG BASE_IMG=r-base
FROM $BASE_IMG

COPY deploy/install_packages.R /tmp/install_packages.R
RUN Rscript /tmp/install_packages.R && rm -rf /tmp/install_packages
