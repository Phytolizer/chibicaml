open Helpers

let%expect_test "simple call" =
  run "int main() { return ret3(); }";
  [%expect {| 3 |}]

let%expect_test "simple call 2" =
  run "int main() { return ret5(); }";
  [%expect {| 5 |}]

let%expect_test "call with args" =
  run "int main() { return add(3, 5); }";
  [%expect {| 8 |}]

let%expect_test "call with args 2" =
  run "int main() { return sub(5, 3); }";
  [%expect {| 2 |}]

let%expect_test "call with many args" =
  run "int main() { return add6(1,2,3,4,5,6); }";
  [%expect {| 21 |}]

let%expect_test "nested calls" =
  run "int main() { return add6(1,2,add6(3,4,5,6,7,8),9,10,11); }";
  [%expect {| 66 |}]

let%expect_test "even more nested calls" =
  run
    "int main() { return \
     add6(1,2,add6(3,add6(4,5,6,7,8,9),10,11,12,13),14,15,16); }";
  [%expect {| 136 |}]
