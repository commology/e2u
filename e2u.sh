#!/usr/bin/env bash

APP_ROOT_DIR=`dirname $0`
cd $APP_ROOT_DIR

ERL_LIBS="."
DEPS=`ls deps`
for dep in $DEPS
do
    ERL_LIBS="${ERL_LIBS}:./deps/${dep}"
done

erl -env ERL_LIBS $ERL_LIBS

