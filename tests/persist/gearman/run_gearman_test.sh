#!/bin/bash

TEST_DIR=$1

ps -C gearmand
if [ 1 -eq "$?" ]; then
  gearmand -d -l stderr
  shutdown_server=0
fi

${TEST_DIR}/GearmanUTest
exit_status="$?"

if [ 0 -eq  "$shutdown_server" ]; then
  gearadmin --shutdown
fi

exit "$exit_status"
