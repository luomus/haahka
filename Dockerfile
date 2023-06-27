FROM ghcr.io/luomus/base-r-image@sha256:7b02c5e1679ea46fa44e1d8ad8a56551fff2f90779e509676a378670e8e85517

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
