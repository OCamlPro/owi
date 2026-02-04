package main

func f(x int32) int32 {
	arr := [4]int32{1, 2, 0, 4}

	if x >= 0 && x < 4 {
		return 10 / arr[x]
	}

	return -1
}


//go:wasm-module owi
//export i32_symbol
func i32_symbol() int32

//go:export check
func check() {
	var x int32 = i32_symbol()
	var _ int32 = f(x)
}

// main is required by the tinygo compiler, even if it isn't used.
func main() {}
