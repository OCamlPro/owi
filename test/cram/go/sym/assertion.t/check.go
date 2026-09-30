package main

//go:wasm-module owi
//export assert
func owi_assert(bool)


func mean1(x, y int32) int32 {
	return (x & y) + ((x ^ y) >> 1)
}

func mean2(x, y int32) int32 {
	return (x + y) / 2
}

//go:export check
func check(x, y int32) {
	owi_assert(mean1(x, y) == mean2(x, y))
}

// main is required by the tinygo compiler, even if it isn't used.
func main() {}
