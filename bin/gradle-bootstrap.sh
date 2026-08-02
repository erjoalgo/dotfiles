#!/bin/bash -x

set -euo pipefail

touch empty
gradle wrapper --gradle-version 9.6.1 -u -c empty -b empty
rm empty

./gradlew compileJava

