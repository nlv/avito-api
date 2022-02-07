#!/bin/bash

stack exec avito-api-exe stack -- -p $PORT -b $DBNAME -u $DBUSER -w $DBPASSWORD -a $S3USER -k $S3PASSWORD -l $S3URL -r $S3REGION &


