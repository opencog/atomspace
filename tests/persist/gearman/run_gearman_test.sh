#!/bin/bash

TEST_DIR=$1

ps -C gearmand
if [ 1 -eq "$?" ]; then
  gearmand -d -l stderr
fi

${TEST_DIR}/GearmanUTest
