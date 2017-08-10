debug:
	RUST_LOG=debug cargo run
run:
	cargo run --release

test:
	parallel ::: "cargo run > cpu_dump" "~/research/Cinoop/cinoop tetris.gb > cinoop_cpu_dump"

test_cpu:
	parallel ::: "cargo run -- --rom=cpu_instrs.gb > cpu_dump" "~/research/Cinoop/cinoop cpu_instrs.gb > cinoop_cpu_dump"

diff:
	cmp chris_cpu_regs_dump.log ~/research/gameboy-emulators/mooneye-gb/mooneye_cpu_regs_dump.log; #read -p "Press enter to continue"; #vim -O chris_cpu_regs_dump.log ~/research/gameboy-emulators/mooneye-gb/mooneye_cpu_regs_dump.log

# In Vim, to switch panes, press Ctrl W
