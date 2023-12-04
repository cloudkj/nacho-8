NACHO-8
-------

A CHIP-8 emulator, with extensions.

## Features

* Customizable clock speed, refresh rate

## Testing

Games tested

* Breakout

## Extensions

* [TBD] Monochrome -> Color (learn colors with CNN?)

## Development

* To run the emulator, we use a custom Docker image to run Ubuntu desktkop along
  with a VNC server, which can then be accessed from the browser at
  http://localhost:3000
* Quick start guide:
  * 1. Create a build: `make clean && make build`
  * 2. Start the container: `make desktop`
  * 3. Access Ubuntu via browser-based VNC client at http://localhost:3000
  * 4. Run the emulator: `/home/nacho-8/build/nacho-8`

## Resources

* http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
* https://github.com/Skosulor/c8int/tree/master/test
* https://gist.github.com/cschiewek/246a244ba23da8b9f0e7b11a68bf3285
