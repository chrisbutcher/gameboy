# GameBoy

A GAMEBOY emulator written in Rust.

## Getting started
```bash
# SDL2 is the only dependency not fetched as a crate
brew install sdl2 sdl2_image sdl2_gfx
# ^ Mac OS-specific
# See https://github.com/Rust-SDL2/rust-sdl2 for Windows, Linux instructions

cargo run
# Or
cargo run -r tetris.gb # Optional: Defaults to running tetris.gb

cargo doc --open # To see docs in your browser
```

## Controls
```
A = Z
B = X
Start = Enter
Select = Right shift
Up, Down, Left, Right (What you'd expect)
```

## Screenshots

![](https://user-images.githubusercontent.com/1916444/37868856-1cd661a4-2f84-11e8-8c99-3ffabd67568d.png)
![](https://user-images.githubusercontent.com/1916444/37868857-1ce55722-2f84-11e8-9690-417bd86efa55.png)

## Features

* [x] CPU (implemented enough to play Tetris but only tested with Tetris)
* [x] PPU (no double-size sprite support yet)
* [x] MMU (no memory bank switching yet)
* [x] Input
* [x] Runs at 60fps :)
* [x] No `unsafe` blocks
* [ ] Sound
* [ ] Serial port
