#!/bin/sh
export SBT_OPTS="-Xmx1536M -Xms512M -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=2G -Xss2M -Duser.timezone=GMT"

java -Dsbt.ivy.home=./.ivy2/cache -jar ./lib/sbt-launch.jar "$@"
