open Helpers

let%expect_test "addr/deref in same expr" =
  run "int main() { int x=3; return *&x; }";
  [%expect {| 3 |}]

let%expect_test "double pointer" =
  run "int main() { int x=3; int *y=&x; int **z=&y; return **z; }";
  [%expect {| 3 |}]

let%expect_test "pointer arith between vars" =
  run "int main() { int x=3; int y=5; return *(&x+1); }";
  [%expect {| 5 |}]

let%expect_test "pointer arith between vars (subtract)" =
  run "int main() { int x=3; int y=5; return *(&y-1); }";
  [%expect {| 3 |}]

let%expect_test "aliasing variable with pointer" =
  run "int main() { int x=3; int *y=&x; *y=5; return x; }";
  [%expect {| 5 |}]

let%expect_test "assign to pointer arith expr" =
  run "int main() { int x=3; int y=5; *(&x+1)=7; return y; }";
  [%expect {| 7 |}]

let%expect_test "assign to pointer arith expr (subtract)" =
  run "int main() { int x=3; int y=5; *(&y-2+1)=7; return x; }";
  [%expect {| 7 |}]

let%expect_test "subtract 2 pointers" =
  run "int main() { int x=3; return (&x+2)-&x+3; }";
  [%expect {| 5 |}]
