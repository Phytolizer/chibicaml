  $ ./scripts/runcode.sh "{ return 0; }"
  $ ./scripts/runcode.sh "{ return 42; }"
  [42]
  $ ./scripts/runcode.sh "{ return 5+20-4; }"
  [21]
  $ ./scripts/runcode.sh "{ return  12 + 34 - 5 ; }"
  [41]
  $ ./scripts/runcode.sh "{ return 5+6*7; }"
  [47]
  $ ./scripts/runcode.sh "{ return 5*(9-6); }"
  [15]
  $ ./scripts/runcode.sh "{ return (3+5)/2; }"
  [4]
  $ ./scripts/runcode.sh "{ return -10+20; }"
  [10]
  $ ./scripts/runcode.sh "{ return - -10; }"
  [10]
  $ ./scripts/runcode.sh "{ return - - +10; }"
  [10]

Equality comparisons
  $ ./scripts/runcode.sh "{ return 0==1; }"
  $ ./scripts/runcode.sh "{ return 42==42; }"
  [1]
  $ ./scripts/runcode.sh "{ return 0!=1; }"
  [1]
  $ ./scripts/runcode.sh "{ return 42!=42; }"

Relational comparisons
  $ ./scripts/runcode.sh "{ return 0<1; }"
  [1]
  $ ./scripts/runcode.sh "{ return 1<1; }"
  $ ./scripts/runcode.sh "{ return 2<1; }"
  $ ./scripts/runcode.sh "{ return 0<=1; }"
  [1]
  $ ./scripts/runcode.sh "{ return 1<=1; }"
  [1]
  $ ./scripts/runcode.sh "{ return 2<=1; }"

  $ ./scripts/runcode.sh "{ return 1>0; }"
  [1]
  $ ./scripts/runcode.sh "{ return 1>1; }"
  $ ./scripts/runcode.sh "{ return 1>2; }"
  $ ./scripts/runcode.sh "{ return 1>=0; }"
  [1]
  $ ./scripts/runcode.sh "{ return 1>=1; }"
  [1]
  $ ./scripts/runcode.sh "{ return 1>=2; }"

Multi statements
  $ ./scripts/runcode.sh "{ return 1; 2; 3; }"
  [1]
  $ ./scripts/runcode.sh "{ 1; return 2; 3; }"
  [2]
  $ ./scripts/runcode.sh "{ 1; 2; return 3; }"
  [3]

Simple locals
  $ ./scripts/runcode.sh "{ a=3; return a; }"
  [3]
  $ ./scripts/runcode.sh "{ a=3; z=5; return a+z; }"
  [8]
  $ ./scripts/runcode.sh "{ a=b=3; return a+b; }"
  [6]

Longer local names
  $ ./scripts/runcode.sh "{ foo=3; return foo; }"
  [3]
  $ ./scripts/runcode.sh "{ foo123=3; bar=5; return foo123+bar; }"
  [8]

Nested blocks
  $ ./scripts/runcode.sh "{ {1; {2;} return 3;} }"
  [3]

Null statement
  $ ./scripts/runcode.sh "{ ;;; return 5; }"
  [5]

If/else
  $ ./scripts/runcode.sh "{ if (0) return 2; return 3; }"
  [3]
  $ ./scripts/runcode.sh "{ if (1-1) return 2; return 3; }"
  [3]
  $ ./scripts/runcode.sh "{ if (1) return 2; return 3; }"
  [2]
  $ ./scripts/runcode.sh "{ if (2-1) return 2; return 3; }"
  [2]
  $ ./scripts/runcode.sh "{ if (0) { 1; 2; return 3; } else { return 4; } }"
  [4]
  $ ./scripts/runcode.sh "{ if (1) { 1; 2; return 3; } else { return 4; } }"
  [3]

For statement
  $ ./scripts/runcode.sh "{ i=0; j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }"
  [55]
  $ ./scripts/runcode.sh "{ for (;;) {return 3;} return 5; }"
  [3]
