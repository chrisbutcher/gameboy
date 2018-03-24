Find first difference between two files
cmp cpu_dump ~/research/Cinoop/cpu_dump


count lines of code
find . -name '*.rs' | xargs wc -l

// TODO get these test roms to try out http://slack.net/~ant/old/gb-tests/

// https://github.com/CTurt/Cinoop/blob/master/source/cpu.c
// https://github.com/CTurt/Cinoop/blob/master/include/cpu.h
// https://github.com/drhelius/Gearboy/blob/master/src/opcodes.cpp

// ALSO SEE https://github.com/mvdnes/rboy/blob/master/src/cpu.rs (Note that cycles in this code are divided by 4)
// http://gameboy.mongenel.com/dmg/lesson1.html

// Specs:
// http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
// http://web.textfiles.com/games/gbspec.txt
// http://bgb.bircd.org/pandocs.htm#aboutthepandocs
// https://www.youtube.com/watch?v=ecTQVa42sJc
// http://robdor.com/2016/08/10/gameboy-emulator-half-carry-flag/
// http://gameboy.mongenel.com/asmschool.html

// Rust sdl2
// http://jadpole.github.io/arcaders/arcaders-1-4
// https://github.com/simias/gb-rs/blob/master/src/ui/sdl2/display.rs#L10

// Test roms:
// http://gbdev.gg8.se/files/roms/blargg-gb-tests/

// Opcodes specifically:
// http://imrannazar.com/Gameboy-Z80-Opcode-Map
// http://gameboy.mongenel.com/dmg/opcodes.html

// Tools:
// https://github.com/mmuszkow/gb-disasm (Gameboy Rom disassembler)

// Tutorials
// https://www.youtube.com/watch?v=_mHdUhVQOb8
// http://imrannazar.com/GameBoy-Emulation-in-JavaScript:-The-CPU
// http://www.codeslinger.co.uk/pages/projects/gameboy/beginning.html
// https://cturt.github.io/cinoop.html
// https://github.com/jedahan/rustboy/blob/master/development_log.md
// https://speakerdeck.com/albertofem/a-journey-into-hardware-emulation-building-a-gameboy-emulator-from-scratch

// Livecoding
// https://www.youtube.com/watch?v=025tC0DcFUI&t=625s
