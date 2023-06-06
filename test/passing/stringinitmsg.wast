(assert_malformed
  (module quote
    "(module (data (memory 0) \"\\b\"))"
  )
  "illegal escape"
)

(assert_malformed
  (module quote
    "(module (data (memory 0) \"\\b<\"))"
  )
  "illegal escape"
)
