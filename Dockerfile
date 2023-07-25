FROM ghcr.io/luomus/base-r-image@sha256:047e13660472b4e82a2de18b3aca9900edd0ac67ddcf729e71a6c53a29c6c09b

COPY renv.lock /home/user/renv.lock
COPY app.R /home/user/app.R
COPY update.R /home/user/update.R
COPY taxa.rds /home/user/taxa.rds
COPY translation.json /home/user/translation.json
COPY R/ /home/user/R/
COPY www/ /home/user/www
COPY DESCRIPTION /home/user/DESCRIPTION

RUN R -e "renv::restore()" \
 && permissions.sh

CMD ["R", "-e", "shiny::runApp(port = 3838, host = '0.0.0.0')"]
