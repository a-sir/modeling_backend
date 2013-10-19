#!/bin/sh

java -Dsbt.ivy.home=./.ivy2/cache -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -jar ./lib/sbt-launch.jar "$@"
