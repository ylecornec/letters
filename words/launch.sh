#!/bin/bash
if [ -z "$DB_PASSWORD" ]
then
	echo "environment variable DB_PASSWORD must be set"
	exit 1
fi

if [ -z "$DB_HOST" ]
then
	echo "environment variable DB_HOST must be set"
	exit 1
fi

if [ -z "$DB_PORT" ]
then
	echo "environment variable DB_PORT must be set"
	exit 1
fi

if [ -z "$DB_NAME" ]
then
	echo "environment variable DB_NAME must be set"
	exit 1
fi
eval $(opam env) && make test.opt
