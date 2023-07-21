FROM ghcr.io/luomus/base-r-image@sha256:b61f78d380e35c41b4161a55b56b4ba2c6ba9baeb5837df9504d141e1a8cdce7

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
