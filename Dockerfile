### Dockerfile

FROM rocker/shiny-verse:3.6.3

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev 

RUN install2.r --error \
    ggplot2 \
    shinydashboard \
    here \
    lubridate \
    gghighlight \
    leaflet \
    leaflet.extras \
    plotly \
    rgdal \
    rgeos \
    sf 

#copy the current folder into the path of the app
COPY . /usr/local/src/app
#set working directory to the app
WORKDIR /usr/local/src/app

#set the unix commands to run the app
CMD ["Rscript","app_run.R"]