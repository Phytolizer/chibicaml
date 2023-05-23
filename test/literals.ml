open Helpers

let%expect_test "0" =
  run "int main() { return 0; }";
  [%expect {| 0 |}]

let%expect_test "42" =
  run "int main() { return 42; }";
  [%expect {| 42 |}]
