#!/bin/bash

RIFT_HOME="../RIFT Game/"

fsc *.scala && scala Simple "${RIFT_HOME}/CombatLog.txt" | tee "${RIFT_HOME}/Meters.txt"
