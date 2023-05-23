open Helpers

let%expect_test "simple decl (no args)" =
  run "int main() { return ret32(); } int ret32() { return 32; }";
  [%expect {| 32 |}]

let%expect_test "decl with args" =
  run "int main() { return add2(3,4); } int add2(int x, int y) { return x+y; }";
  [%expect {| 7 |}]

let%expect_test "decl with args 2" =
  run "int main() { return sub2(4,3); } int sub2(int x, int y) { return x-y; }";
  [%expect {| 1 |}]

let%expect_test "recursive function" =
  run
    "int main() { return fib(9); } int fib(int x) { if (x<=1) return 1; return \
     fib(x-1) + fib(x-2); }";
  [%expect {| 55 |}]
