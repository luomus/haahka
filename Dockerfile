# docker manifest inspect ghcr.io/luomus/base-r-image:main -v | jq '.Descriptor.digest'
FROM ghcr.io/luomus/base-r-image@sha256:a3f407c546d9213507e451e679675154e563ebf259aa1af9e6ea5e3ca149bc65

COPY renv.lock /home/user/renv.lock

RUN R -s -e "renv::restore()"

COPY app.R /home/user/app.R
COPY api.R /home/user/api.R
COPY update.R /home/user/update.R
COPY translation.json /home/user/translation.json
COPY R/ /home/user/R/
COPY man /home/user/man
COPY download_photos.R /home/user/download_photos.R
COPY resize_photos.R /home/user/resize_photos.R
COPY download_descriptions.R /home/user/download_descriptions.R
COPY download_data.R /home/user/download_data.R
COPY www/ /home/user/www
COPY DESCRIPTION /home/user/DESCRIPTION
COPY NAMESPACE /home/user/NAMESPACE
COPY .Rbuildignore /home/user/.Rbuildignore

RUN R CMD INSTALL .
RUN permissions.sh
