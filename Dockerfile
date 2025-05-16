FROM rocker/verse:latest

WORKDIR /code

RUN apt-get update && apt-get install -y \
        default-jdk \
        r-cran-rjava \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('clock', 'PatientProfiles', 'CDMConnector', 'DatabaseConnector'))"

COPY drivers/ drivers/
COPY scripts/ .
COPY R .

ENTRYPOINT ["/bin/bash", "-c", "Rscript main.r"]