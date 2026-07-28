(module
  (func $main
    v128.const i8x16 10 23 56 15 25 29 92 45 36 4 78 12 34 25 72 82
    v128.const i8x16 0 2 5 7 3 5 9 2 4 6 1 7 0 3 5 1
    i8x16.swizzle

    i8x16.extract_lane_u 6
    drop
  )
  (start $main)
)
