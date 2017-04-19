FROM r-base:latest

MAINTAINER Apparao Kalimireddy "apparaokalimireddy@gmail.com"

# Install dependencies and Download and install shiny server
RUN apt-get update && apt-get install -y -t unstable && apt-get install libssl-dev/unstable -y && apt-get install libxml2 libxml2-dev -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev && \
    wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown', 'rvest', 'ggplot2', 'scales', 'chron', 'timeDate'), repos='https://cran.rstudio.com/')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    mkdir /srv/shiny-server/movie-revenue-predictor && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod 777 /usr/bin/shiny-server.sh
CMD ["/usr/bin/shiny-server.sh"]
