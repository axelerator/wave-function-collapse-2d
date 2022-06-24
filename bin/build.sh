#!/bin/sh
trap ctrl_c INT

function ctrl_c() {
        exit 0
}

function append() {
ASSETS="$ASSETS\n$1"
}

function report() {
  touch tmp/build.log
  ERRORS=`cat tmp/build.log`
  if [ -n "$ERRORS" ]; then
    echo "Comiled with errors"
    # to also print errors in console we just compile a second time
    elm make src/Main.elm
    VALUE=`date -r tmp/build.log`
    printf "refresh('" > tmp/timestamp.js
    printf "$VALUE" >>  tmp/timestamp.js
    printf "', " >>  tmp/timestamp.js
    cat tmp/build.log >> tmp/timestamp.js
    printf ");" >> tmp/timestamp.js
  else
    echo "Compiled without errors"
    VALUE=`date -r elm.js`
    TIMESTAMP_JS_TEMPLATE="refresh('${VALUE}')"
    INTERPOLATED=`echo "${TIMESTAMP_JS_TEMPLATE}" | sed "s/VALUE/${VALUE}/" | sed "s/ERROR//" `
    echo "$INTERPOLATED" > tmp/timestamp.js
  fi
}


function buildCode() {
  echo "Compiling ⚔️"
  elm make src/Main.elm --output=elm.js --report=json 2> tmp/build.log
  report
}

while true; do
  buildCode
  fswatch src/ assets/ -1
done
