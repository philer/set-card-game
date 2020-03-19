#!/bin/sh

elm make src/Main.elm --optimize --output=main.js

MINIFIER=$(command -v terser > /dev/null && echo terser || echo uglifyjs)
echo "minifying using $MINIFIER..."
$MINIFIER main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe,unsafe_arrows,ecma=2019' \
	| $MINIFIER --mangle --output=main.js
