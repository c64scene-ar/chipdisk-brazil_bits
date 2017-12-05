# Chipdisk: Brazil Bits

Recommended setup:

- Commodore 64 with SID 8580
- Works with PAL, NTSC, PAL-N (Drean)

## How to compile it

- Download [cc65](http://cc65.github.io/cc65/) and add it to the path
- Download [exomizer](http://hem.bredband.net/magli143/exo/) and add it to the path
- Download [VICE](http://vice-emu.sourceforge.net/) and add it to the path

and then do `make`

        $ git clone https://github.com/c64scene-ar/chipdisk-brazil_bits.git
        $ cd chipdisk-brazil_bits
        $ make

## Original Tracks

The sids are here:

- Sids: [res/sids](res/sids)

## Internals

### Directory

- `src`: includes source code + assets needed by the source code
- `res`: includes the data in its original format: .vcharproj, sids, etc.
- `bin`: the compiled binary


## License

[Apache v2 License](LICENSE)

