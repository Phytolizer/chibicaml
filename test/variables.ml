open Helpers

let%expect_test "separate decl/assignment" =
  run "int main() { int a; a=3; return a; }";
  [%expect {| 3 |}]

let%expect_test "joined decl/assignment" =
  run "int main() { int a=3; return a; }";
  [%expect {| 3 |}]

let%expect_test "multiple vars" =
  run "int main() { int a=3; int z=5; return a+z; }";
  [%expect {| 8 |}]

let%expect_test "multiple assignments at once" =
  run "int main() { int a; int b; a=b=3; return a+b; }";
  [%expect {| 6 |}]

let%expect_test "longer name" =
  run "int main() { int foo=3; return foo; }";
  [%expect {| 3 |}]

let%expect_test "longer name (multiple)" =
  run "int main() { int foo123=3; int bar=5; return foo123+bar; }";
  [%expect {| 8 |}]

let%expect_test "multi declaration" =
  run "int main() { int x, y; x=3; y=5; return x+y; }";
  [%expect {| 8 |}]

let%expect_test "multi declaration with initializers" =
  run "int main() { int x=3, y=5; return x+y; }";
  [%expect {| 8 |}]
