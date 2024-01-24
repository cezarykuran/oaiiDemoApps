ARG BASE_IMG=r-base
FROM $BASE_IMG

# OS
RUN apt update && apt install --no-install-recommends -y libssl-dev libcurl4-openssl-dev

# R
COPY deploy/install_packages.R /tmp/install_packages.R
RUN Rscript /tmp/install_packages.R && rm -rf /tmp/install_packages
