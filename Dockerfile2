FROM jinlongru/deepsmirud_web

LABEL author="Jinlong Ru"

# copy the app to the image
COPY app/* /srv/shiny-server/

WORKDIR /srv/shiny-server/

# select port
EXPOSE 3838

# run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3838)"]
