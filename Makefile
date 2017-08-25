debug:
	RUST_LOG=debug cargo run
run:
	cargo run --release

test:
	parallel ::: "cargo run > cpu_dump" "~/research/Cinoop/cinoop tetris.gb > cinoop_cpu_dump"

test_cpu:
	parallel ::: "cargo run -- --rom=cpu_instrs.gb > cpu_dump" "~/research/Cinoop/cinoop cpu_instrs.gb > cinoop_cpu_dump"

diff:
	# sed -i '/^Reading from 000b, read_result: fb/d' chris_cpu_regs_dump.log; sed -i '/^Reading from 000b, read_result: fb/d' ~/research/gameboy-emulators/mooneye-gb/mooneye_cpu_regs_dump.log; sed -i '/^Reading from 004c, read_result: 08/d' chris_cpu_regs_dump.log; sed -i '/^Reading from 004c, read_result: 08/d' ~/research/gameboy-emulators/mooneye-gb/mooneye_cpu_regs_dump.log; cmp chris_cpu_regs_dump.log ~/research/gameboy-emulators/mooneye-gb/mooneye_cpu_regs_dump.log;
	cmp chris_cpu_regs_dump.log ~/research/gameboy-emulators/mooneye-gb/mooneye_cpu_regs_dump.log;

# In Vim, to switch panes, press Ctrl W
