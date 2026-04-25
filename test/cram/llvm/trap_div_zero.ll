target triple = "wasm32-unknown-unknown"

define i32 @main() {
entry:
  %x = sdiv i32 1, 0
  ret i32 %x
}
