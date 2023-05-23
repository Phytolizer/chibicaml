open Helpers

let%expect_test "additive operators" =
  run "int main() { return 5+20-4; }";
  [%expect {| 21 |}]

let%expect_test "additive operators with whitespace" =
  run "int main() { return  12 + 34 - 5 ; }";
  [%expect {| 41 |}]

let%expect_test "add/multiply precedence" =
  run "int main() { return 5+6*7; }";
  [%expect {| 47 |}]

let%expect_test "precedence with parens" =
  run "int main() { return 5*(9-6); }";
  [%expect {| 15 |}]

let%expect_test "parens first" =
  run "int main() { return (3+5)/2; }";
  [%expect {| 4 |}]

let%expect_test "unary vs binary" =
  run "int main() { return -10+20; }";
  [%expect {| 10 |}]

let%expect_test "two unaries" =
  run "int main() { return - -10; }";
  [%expect {| 10 |}]

let%expect_test "three unaries" =
  run "int main() { return - - +10; }";
  [%expect {| 10 |}]
