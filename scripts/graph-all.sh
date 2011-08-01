#!/bin/bash

ls ./data/*.txt | while read file; do math "$file" < ./graphs/Graphs.m; done
