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

  FINBIF_ACCESS_TOKEN=$FINBIF_DEV_ACCESS_TOKEN
  FINBIF_API_URL=$FINBIF_DEV_API_URL

fi

if [ $i = "build" ]; then

  ITEM=".items[0]"

elif [ $i = "image" ]; then

  ITEM=".items[1]"

elif [ $i = "deploy" ]; then

  ITEM=".items[2]"

elif [ $i = "service" ]; then

  ITEM=".items[3]"

elif [ $i = "route" ]; then

  ITEM=".items[4]"

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
  | jq $ITEM
