package main

//go:export f
func f(x int32) int32 {
	arr := [4]int32{1, 2, 0, 4}

	if x >= 0 && x < 4 {
		return 10 / arr[x]
	}

	return -1
}

// main is required by the tinygo compiler, even if it isn't used.
func main() {}
