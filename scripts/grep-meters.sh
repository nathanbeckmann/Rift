#!/bin/sh
GRAPH_STR="Graph data saved to :"
cat "$1" | grep -B4 "${GRAPH_STR}" | grep -v "${GRAPH_STR}" | grep -v "\-\-"
