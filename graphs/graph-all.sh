#!/bin/bash

ls ../data/*.txt | while read file; do math "$file" < ./Graphs.m; done
