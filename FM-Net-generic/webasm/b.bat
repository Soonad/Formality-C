@clang -std=c++17 -Wall -Wextra -O3 --target=wasm32 -c -o fmc.o fmc.cpp
@wasm-ld --no-entry --export process -o fmc.wasm fmc.o
