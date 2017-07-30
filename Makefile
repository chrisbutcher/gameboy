debug:
	RUST_LOG=debug cargo run
run:
	cargo run --release

test:
	parallel ::: "cargo run > cpu_dump" "~/research/Cinoop/cinoop tetris.gb > cinoop_cpu_dump"

test_cpu:
	parallel ::: "cargo run -- --rom=cpu_instrs.gb > cpu_dump" "~/research/Cinoop/cinoop cpu_instrs.gb > cinoop_cpu_dump"

diff:
	cmp cpu_dump ~/research/Cinoop/cpu_dump; read -p "Press enter to continue";	vim -O cpu_dump cinoop_cpu_dump
