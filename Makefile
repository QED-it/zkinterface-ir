ci:
	cd rust && cargo test

fbs:
	cd rust/src && flatc --rust ../../sieve_ir.fbs
