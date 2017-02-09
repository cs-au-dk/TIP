@echo off
chcp 65001 > nul
set JAVA_OPTS=-Dfile.encoding=UTF-8
sbt "runMain tip.Tip %*"
