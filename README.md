NACHO-8
-------

A CHIP-8 emulator, with extensions.

## Extensions

* Monochrome -> Color (learn colors with CNN?)

## Development

Forward X11 from Docker container to macOS host. Pre-requisites on macOS:

* XQuartz installed
* XQuartz Preferences -> "Security" -> "Allow connections from network
  clients"

1. [macOS] Run `xhost +localhost`
2. [macOS] Pass `-e DISPLAY=host.docker.internal:0` to Docker container

## Resources

* http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
* https://github.com/Skosulor/c8int/tree/master/test
* https://gist.github.com/cschiewek/246a244ba23da8b9f0e7b11a68bf3285
