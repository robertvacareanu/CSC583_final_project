#!/bin/bash

wget https://osf.io/xf29a/download
unzip -q download -d .
sbt -J-Xmx10g 'runMain edu.arizona.cs.apps.SmarterWatsonApp'
