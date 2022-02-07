#!/bin/bash

stack exec avito-api-exe -- -p "$PORT" -b "$DBNAME" -u "$DBUSER" -w "$DBPASSWORD" -a "$S3USER" -k "$S3SKEY" -l "$S3URL" -r "$S3REGION" &


