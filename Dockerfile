FROM ghcr.io/luomus/base-r-image@sha256:db3ad26dc6edfdaed2611f87b948cb3ee9fd41cf9fab8dc701508f5c70906020

COPY renv.lock /home/user/renv.lock
COPY app.R /home/user/app.R
COPY api.R /home/user/api.R
COPY update.R /home/user/update.R
COPY taxa.rds /home/user/taxa.rds
COPY translation.json /home/user/translation.json
COPY R/ /home/user/R/
COPY download_photos.R /home/user/download_photos.R
COPY resize_photos.R /home/user/resize_photos.R
COPY download_descriptions.R /home/user/download_descriptions.R
COPY download_data.R /home/user/download_data.R
COPY www/ /home/user/www
COPY DESCRIPTION /home/user/DESCRIPTION
COPY NAMESPACE /home/user/NAMESPACE

RUN R -e "renv::restore()" \
 && R -e 'remotes::install_local(dependencies = FALSE, upgrade = FALSE)' \
 && permissions.sh
