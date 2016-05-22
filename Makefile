all:
	make -C frontend
	cd rust-bindgen && cargo build
	make -C backend
clean:
	make -C frontend clean
	make -C backend clean
