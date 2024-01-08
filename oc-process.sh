#!/bin/bash

i="all"

while getopts ":f:e:i::" flag; do
  case $flag in
    f) f=${OPTARG} ;;
    e) e=${OPTARG} ;;
    i) i=${OPTARG} ;;
  esac
done

set -a

source ./$e

set +a

BRANCH=$(git symbolic-ref --short -q HEAD)

if [ "$BRANCH" != "main" ]; then

  HOST=$DEV_HOST
  DB_USER_PASSWORD=$DEV_DB_USER_PASSWORD
  DB_PRIMARY_PASSWORD=$DEV_DB_PRIMARY_PASSWORD
  DB_SUPER_PASSWORD=$DEV_DB_SUPER_PASSWORD

fi

if [ $i = "volume-db" ]; then

  ITEM=".items[0]"

elif [ $i = "volume-api" ]; then

  ITEM=".items[1]"

elif [ $i = "image" ]; then

  ITEM=".items[2]"

elif [ $i = "build" ]; then

  ITEM=".items[3]"

elif [ $i = "deploy-app" ]; then

  ITEM=".items[4]"

elif [ $i = "deploy-api" ]; then

  ITEM=".items[5]"

elif [ $i = "deploy-db" ]; then

  ITEM=".items[6]"

elif [ $i = "service-app" ]; then

  ITEM=".items[7]"

elif [ $i = "service-api" ]; then

  ITEM=".items[8]"

elif [ $i = "service-db" ]; then

  ITEM=".items[9]"

elif [ $i = "route-app" ]; then

  ITEM=".items[10]"

elif [ $i = "route-api" ]; then

  ITEM=".items[11]"

elif [ $i = "job" ]; then

  ITEM=".items[12]"

else

  ITEM=""

fi

oc process -f $f \
  -p BRANCH=$BRANCH \
  -p HOST=$HOST \
  -p FINBIF_ACCESS_TOKEN=$FINBIF_ACCESS_TOKEN \
  -p FINBIF_API_URL=$FINBIF_API_URL \
  -p FINBIF_WAREHOUSE=$FINBIF_WAREHOUSE \
  -p FINBIF_EMAIL=$FINBIF_EMAIL \
  -p DB_USER_PASSWORD=$DB_USER_PASSWORD \
  -p DB_PRIMARY_PASSWORD=$DB_PRIMARY_PASSWORD \
  -p DB_SUPER_PASSWORD=$DB_SUPER_PASSWORD \
  | jq $ITEM
