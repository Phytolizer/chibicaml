open Helpers

let%expect_test "equals false" =
  run "int main() { return 0==1; }";
  [%expect {| 0 |}]

let%expect_test "equals true" =
  run "int main() { return 42==42; }";
  [%expect {| 1 |}]

let%expect_test "not-equals true" =
  run "int main() { return 0!=1; }";
  [%expect {| 1 |}]

let%expect_test "not-equals false" =
  run "int main() { return 42!=42; }";
  [%expect {| 0 |}]

let%expect_test "less when less" =
  run "int main() { return 0<1; }";
  [%expect {| 1 |}]

let%expect_test "less when equal" =
  run "int main() { return 1<1; }";
  [%expect {| 0 |}]

let%expect_test "less when greater" =
  run "int main() { return 2<1; }";
  [%expect {| 0 |}]

let%expect_test "leq when less" =
  run "int main() { return 0<=1; }";
  [%expect {| 1 |}]

let%expect_test "leq when equal" =
  run "int main() { return 1<=1; }";
  [%expect {| 1 |}]

let%expect_test "leq when greater" =
  run "int main() { return 2<=1; }";
  [%expect {| 0 |}]

let%expect_test "greater when less" =
  run "int main() { return 0>1; }";
  [%expect {| 0 |}]

let%expect_test "greater when equal" =
  run "int main() { return 1>1; }";
  [%expect {| 0 |}]

let%expect_test "greater when greater" =
  run "int main() { return 2>1; }";
  [%expect {| 1 |}]

let%expect_test "geq when less" =
  run "int main() { return 0>=1; }";
  [%expect {| 0 |}]

let%expect_test "geq when equal" =
  run "int main() { return 1>=1; }";
  [%expect {| 1 |}]

let%expect_test "geq when greater" =
  run "int main() { return 2>=1; }";
  [%expect {| 1 |}]
