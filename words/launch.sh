#!/bin/bash
echo $DB_PASSWORD
eval $(opam env) && make db-create && make db-schema && make test.opt
