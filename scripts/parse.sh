#!/bin/bash

RIFT_HOME="../../RIFT Game/"

cd parser && fsc *.scala && scala Simple "${RIFT_HOME}/CombatLog.txt" true true | tee "${RIFT_HOME}/Meters.txt"
