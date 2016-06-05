#!/usr/bin/env sh

java -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled \
  -XX:MaxPermSize=256M ${JAVA_OPTS} -Dfile.encoding=UTF-8 \
  -jar `dirname $0`/sbt-launch.jar "$@"
