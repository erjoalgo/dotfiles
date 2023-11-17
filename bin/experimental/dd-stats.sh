#!/bin/bash -x
DDPID=(ps aux | grep 'D.*dd '  | awk '{print $2}')
sudo kill -USR1 $DDPID 
