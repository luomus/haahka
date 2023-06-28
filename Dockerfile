FROM ghcr.io/luomus/base-r-image@sha256:1284c451bd7c894bc77aa728087648562a9c10a203e688cf81a317aaa6f93de5

COPY renv.lock /home/user/renv.lock
COPY app.R /home/user/app.R
COPY update.R /home/user/update.R
COPY taxa.rds /home/user/taxa.rds
COPY translation.json /home/user/translation.json
COPY R/ /home/user/R/
COPY www/ /home/user/www
COPY DESCRIPTION /home/user/DESCRIPTION

RUN R -e "renv::restore()" \
 && mkdir -p /home/user/data \
 && chgrp -R 0 /home/user \
 && chmod -R g=u /home/user /etc/passwd

CMD ["R", "-e", "shiny::runApp(port = 3838, host = '0.0.0.0')"]
