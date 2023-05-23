open Helpers

let%expect_test _ =
  run "int main() { int x; return sizeof(x); }";
  [%expect {| 8 |}]

let%expect_test _ =
  run "int main() { int x; return sizeof x; }";
  [%expect {| 8 |}]

let%expect_test _ =
  run "int main() { int *x; return sizeof(x); }";
  [%expect {| 8 |}]

let%expect_test _ =
  run "int main() { int x[4]; return sizeof(x); }";
  [%expect {| 32 |}]

let%expect_test _ =
  run "int main() { int x[3][4]; return sizeof(x); }";
  [%expect {| 96 |}]

let%expect_test _ =
  run "int main() { int x[3][4]; return sizeof(*x); }";
  [%expect {| 32 |}]

let%expect_test _ =
  run "int main() { int x[3][4]; return sizeof(**x); }";
  [%expect {| 8 |}]

let%expect_test _ =
  run "int main() { int x[3][4]; return sizeof(**x) + 1; }";
  [%expect {| 9 |}]

let%expect_test _ =
  run "int main() { int x[3][4]; return sizeof **x + 1; }";
  [%expect {| 9 |}]

let%expect_test _ =
  run "int main() { int x[3][4]; return sizeof(**x + 1); }";
  [%expect {| 8 |}]

let%expect_test _ =
  run "int main() { int x=1; return sizeof(x=2); }";
  [%expect {| 8 |}]

let%expect_test _ =
  run "int main() { int x=1; sizeof(x=2); return x; }";
  [%expect {| 1 |}]
