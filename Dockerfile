FROM rocker/r-ver:4.2.1@sha256:84dbe29c3218221af453eca9bf95249d605920d9aa03598fcc96767242b7ea5e

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      curl \
      libcurl4-openssl-dev \
      libmagick++-dev \
      libsodium-dev \
      libssl-dev \
      libxml2-dev \
      libz-dev \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /var/lib/apt/lists/*

ENV RENV_PATHS_LIBRARY renv/library

COPY renv.lock renv.lock

RUN R -e "install.packages('renv')" \
 && R -e "renv::restore()"

HEALTHCHECK --interval=1m --timeout=10s \
  CMD curl -sfI -o /dev/null 0.0.0.0:3838 || exit 1

ENV HOME /home/user
ENV  OPENBLAS_NUM_THREADS 1

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
COPY app.R /home/user/app.R
COPY update.R /home/user/update.R
COPY taxa.rds /home/user/taxa.rds
COPY translation.json /home/user/translation.json
COPY google-analytics.js /home/user/google-analytics.js
COPY R/ /home/user/R/
COPY www/ /home/user/www
COPY DESCRIPTION /home/user/DESCRIPTION

RUN  mkdir -p /home/user/data \
  && chgrp -R 0 /home/user \
  && chmod -R g=u /home/user /etc/passwd

WORKDIR /home/user

USER 1000

EXPOSE 3838

ENTRYPOINT ["entrypoint.sh"]

CMD ["R", "-e", "shiny::runApp(port = 3838, host = '0.0.0.0')"]
