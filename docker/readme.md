#Make sure to copy movie-revenue-predictor folder to /srv/shinyapps folder on the server
docker run -d -p 80:3838 \
    -v /srv/shinyapps/:/srv/shiny-server/ \
    -v /srv/shinylog/:/var/log/shiny-server/ \
    shiny-server
