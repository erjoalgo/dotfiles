#!/bin/bash

BEEP_TIMES=${BEEP_TIMES:-3}
BEEP_DELAY_SECS=${BEEP_DELAY_SECS:-1}
BEEP_REPS=${BEEP_REPS:-5}

for OUTER in $(seq ${BEEP_TIMES}); do 
   for INNER in $(seq ${BEEP_REPS}); do 
     beep 
   done 
   sleep ${BEEP_DELAY_SECS}
done  
