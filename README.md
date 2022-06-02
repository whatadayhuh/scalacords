[![Continuous Integration](https://github.com/ornicar/scalachess/actions/workflows/ci.yml/badge.svg)](https://github.com/ornicar/scalachess/actions/workflows/ci.yml)

It is entirely functional, immutable, and free of side effects.

INSTALL
-------

Clone scalacords

    git clone git://github.com/whatadayhuh/scalacords/

Get latest sbt on http://www.scala-sbt.org/download.html

Start sbt in scalachess directory

    sbt

In the sbt shell, to compile scalachess, run

    compile

To run the tests (with coverage):

    clean coverage test
    coverageReport

Code formatting
###

This repository uses [scalafmt](https://scalameta.org/scalafmt/).

Please [install it for your code editor](https://scalameta.org/scalafmt/docs/installation.html)

if you're going to contribute to this project.

If you don't install it, please run `scalafmtAll` in the sbt console before committing.
