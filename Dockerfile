# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev


# install renv & restore packages
RUN Rscript -e 'install.packages("plotly")'
RUN Rscript -e 'install.packages("lubridate")'
RUN Rscript -e 'install.packages(c("Rcpp","tidyverse"))'

# expose port
EXPOSE 3838

COPY /app ./app
WORKDIR "/app"

# run app on container start
CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 3838)"]