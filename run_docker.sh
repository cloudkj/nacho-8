#!/bin/sh
set -ex

/usr/local/bin/docker run -it \
                      -e DISPLAY=host.docker.internal:0 \
                      -v "$PWD:/home/nacho-8" \
                      -w /home/nacho-8/ \
                      kelvin/nacho-8 bash
