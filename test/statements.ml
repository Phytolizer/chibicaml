open Helpers

let%expect_test "return first" =
  run "int main() { return 1; 2; 3; }";
  [%expect {| 1 |}]

let%expect_test "return second" =
  run "int main() { 1; return 2; 3; }";
  [%expect {| 2 |}]

let%expect_test "return third" =
  run "int main() { 1; 2; return 3; }";
  [%expect {| 3 |}]

let%expect_test "nested blocks" =
  run "int main() { {1; {2;} return 3;} }";
  [%expect {| 3 |}]

let%expect_test "null statement" =
  run "int main() { ;;; return 5; }";
  [%expect {| 5 |}]

let%expect_test "if false" =
  run "int main() { if (0) return 2; return 3; }";
  [%expect {| 3 |}]

let%expect_test "if false (more complex condition)" =
  run "int main() { if (1-1) return 2; return 3; }";
  [%expect {| 3 |}]

let%expect_test "if true" =
  run "int main() { if (1) return 2; return 3; }";
  [%expect {| 2 |}]

let%expect_test "if true (more complex condition)" =
  run "int main() { if (2-1) return 2; return 3; }";
  [%expect {| 2 |}]

let%expect_test "if false with else" =
  run "int main() { if (0) { 1; 2; return 3; } else { return 4; } }";
  [%expect {| 4 |}]

let%expect_test "if true with else" =
  run "int main() { if (1) { 1; 2; return 3; } else { return 4; } }";
  [%expect {| 3 |}]

let%expect_test "full for statement" =
  run
    "int main() { int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }";
  [%expect {| 55 |}]

let%expect_test "minimal for statement" =
  run "int main() { for (;;) {return 3;} return 5; }";
  [%expect {| 3 |}]

let%expect_test "simple while statement" =
  run "int main() { int i=0; while (i<10) { i=i+1; } return i; }";
  [%expect {| 10 |}]

let%expect_test "more complex while statement" =
  run
    "int main() { int i=0; int j=0; while (i<=10) { j=i+j; i=i+1; } return j; }";
  [%expect {| 55 |}]
