#!/bin/bash

i="all"
f="template.yml"
e=".env"

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

  HOST=$HOST_DEV
  DB_PASSWORD=$DB_PASSWORD_DEV
  JOB_SECRET=$JOB_SECRET_DEV

fi

if [ $i = "volume-db" ]; then

  ITEM=".items[0]"

elif [ $i = "volume-api" ]; then

  ITEM=".items[1]"

elif [ $i = "config" ]; then

  ITEM=".items[2]"

elif [ $i = "secrets" ]; then

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

elif [ $i = "all" ]; then

  ITEM=""

else

  echo "Object not found"
  exit 1

fi

DB_PASSWORD=$(echo -n $DB_PASSWORD | base64)
FINBIF_ACCESS_TOKEN=$(echo -n $FINBIF_ACCESS_TOKEN | base64)
RCLONE_ACCESS_KEY_ID=$(echo -n $RCLONE_ACCESS_KEY_ID | base64)
RCLONE_SECRET_ACCESS_KEY=$(echo -n $RCLONE_SECRET_ACCESS_KEY | base64)
JOB_SECRET=$(echo -n $JOB_SECRET | base64)

echo "# $(oc project haahka)"

oc process -f $f \
  -p BRANCH="$BRANCH" \
  -p HOST="$HOST" \
  -p FINBIF_ACCESS_TOKEN="$FINBIF_ACCESS_TOKEN" \
  -p FINBIF_API_URL="$FINBIF_API_URL" \
  -p FINBIF_WAREHOUSE="$FINBIF_WAREHOUSE" \
  -p FINBIF_EMAIL="$FINBIF_EMAIL" \
  -p DB_PASSWORD="$DB_PASSWORD" \
  -p SMTP_SERVER="$SMTP_SERVER" \
  -p SMTP_PORT="$SMTP_PORT" \
  -p ERROR_EMAIL_FROM="$ERROR_EMAIL_FROM" \
  -p OBJECT_STORE="$OBJECT_STORE" \
  -p RCLONE_ACCESS_KEY_ID="$RCLONE_ACCESS_KEY_ID" \
  -p RCLONE_SECRET_ACCESS_KEY="$RCLONE_SECRET_ACCESS_KEY" \
  -p JOB_SECRET="$JOB_SECRET" \
  | jq $ITEM
