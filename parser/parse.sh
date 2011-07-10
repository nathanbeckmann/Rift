#!/bin/bash

RIFT_HOME="../../RIFT Game/"

fsc *.scala && scala Simple "${RIFT_HOME}/CombatLog.txt" true | tee "${RIFT_HOME}/Meters.txt"
