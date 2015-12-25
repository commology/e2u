#!/bin/env bash

APP_ROOT_DIR=`dirname $0`

erl -pa $APP_ROOT_DIR/ebin deps/* deps/*/ebin

