Write aux file for upcoming tests
  $ cat <<EOM | gcc -xc -c -o tmp2.o -
  > int ret3() { return 3; }
  > int ret5() { return 5; }
  > int add(int x, int y) { return x+y; }
  > int sub(int x, int y) { return x-y; }
  > 
  > int add6(int a, int b, int c, int d, int e, int f) {
  >   return a+b+c+d+e+f;
  > }
  > EOM

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
  $ ./scripts/runcode.sh "{ int a; a=3; return a; }"
  [3]
  $ ./scripts/runcode.sh "{ int a=3; return a; }"
  [3]
  $ ./scripts/runcode.sh "{ int a=3; int z=5; return a+z; }"
  [8]
  $ ./scripts/runcode.sh "{ int a; int b; a=b=3; return a+b; }"
  [6]

Longer local names
  $ ./scripts/runcode.sh "{ int foo=3; return foo; }"
  [3]
  $ ./scripts/runcode.sh "{ int foo123=3; int bar=5; return foo123+bar; }"
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
  $ ./scripts/runcode.sh "{ int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }"
  [55]
  $ ./scripts/runcode.sh "{ for (;;) {return 3;} return 5; }"
  [3]

While statement
  $ ./scripts/runcode.sh "{ int i=0; while (i<10) { i=i+1; } return i; }"
  [10]
  $ ./scripts/runcode.sh "{ int i=0; int j=0; while (i<=10) { j=i+j; i=i+1; } return j; }"
  [55]

Address/deref expressions
  $ ./scripts/runcode.sh "{ int x=3; return *&x; }"
  [3]
  $ ./scripts/runcode.sh "{ int x=3; int *y=&x; int **z=&y; return **z; }"
  [3]
  $ ./scripts/runcode.sh "{ int x=3; int y=5; return *(&x+1); }"
  [5]
  $ ./scripts/runcode.sh "{ int x=3; int y=5; return *(&y-1); }"
  [3]
  $ ./scripts/runcode.sh "{ int x=3; int *y=&x; *y=5; return x; }"
  [5]
  $ ./scripts/runcode.sh "{ int x=3; int y=5; *(&x+1)=7; return y; }"
  [7]
  $ ./scripts/runcode.sh "{ int x=3; int y=5; *(&y-2+1)=7; return x; }"
  [7]
  $ ./scripts/runcode.sh "{ int x=3; return (&x+2)-&x+3; }"
  [5]
  $ ./scripts/runcode.sh "{ int x, y; x=3; y=5; return x+y; }"
  [8]
  $ ./scripts/runcode.sh "{ int x=3, y=5; return x+y; }"
  [8]

Function calls
  $ ./scripts/runcode.sh "{ return ret3(); }"
  [3]
  $ ./scripts/runcode.sh "{ return ret5(); }"
  [5]

...with parameters
  $ ./scripts/runcode.sh "{ return add(3, 5); }"
  [8]
  $ ./scripts/runcode.sh "{ return sub(5, 3); }"
  [2]
  $ ./scripts/runcode.sh "{ return add6(1,2,3,4,5,6); }"
  [21]
  $ ./scripts/runcode.sh "{ return add6(1,2,add6(3,4,5,6,7,8),9,10,11); }"
  [66]
  $ ./scripts/runcode.sh "{ return add6(1,2,add6(3,add6(4,5,6,7,8,9),10,11,12,13),14,15,16); }"
  [136]
