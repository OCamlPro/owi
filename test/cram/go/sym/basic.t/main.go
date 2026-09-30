package main

//go:wasm-module owi
//export i32_symbol
func i32_symbol() int32

func main() {
	var x int32 = i32_symbol()
	var i int32 = 42
	if (i - x) == 0 {
		panic(":S")
	}
}
