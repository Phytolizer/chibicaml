open Helpers

let%expect_test "simple array" =
  run "int main() { int x[2]; int *y=&x; *y=3; return *x; }";
  [%expect {| 3 |}]

let%expect_test "arrays" =
  run "int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *x; }";
  [%expect {| 3 |}]

let%expect_test "arrays 2" =
  run "int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *(x+1); }";
  [%expect {| 4 |}]

let%expect_test "arrays 3" =
  run "int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *(x+2); }";
  [%expect {| 5 |}]
