#! /bin/bash

OPTS=""
#OPTS="-Xprint-types $OPTS"
#OPTS="-Yshow-syms $OPTS"
OPTS="-Xprint:typer $OPTS"
OPTS="-uniqid $OPTS"

~/LocalInstalls/scala-2.11.8/bin/scalac -classpath ../Orclave/bin/ $OPTS src/orc/scala/test/TestApp.scala
