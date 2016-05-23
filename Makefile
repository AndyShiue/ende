all:
	cd rust-bindgen && cargo build
	make -C frontend
	make -C backend
clean:
	make -C frontend clean
	make -C backend clean
