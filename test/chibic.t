  $ ./scripts/runcode.sh "0;"
  $ ./scripts/runcode.sh "42;"
  [42]
  $ ./scripts/runcode.sh "5+20-4;"
  [21]
  $ ./scripts/runcode.sh " 12 + 34 - 5 ;"
  [41]
  $ ./scripts/runcode.sh "5+6*7;"
  [47]
  $ ./scripts/runcode.sh "5*(9-6);"
  [15]
  $ ./scripts/runcode.sh "(3+5)/2;"
  [4]
  $ ./scripts/runcode.sh "-10+20;"
  [10]
  $ ./scripts/runcode.sh "- -10;"
  [10]
  $ ./scripts/runcode.sh "- - +10;"
  [10]

Equality comparisons
  $ ./scripts/runcode.sh "0==1;"
  $ ./scripts/runcode.sh "42==42;"
  [1]
  $ ./scripts/runcode.sh "0!=1;"
  [1]
  $ ./scripts/runcode.sh "42!=42;"

Relational comparisons
  $ ./scripts/runcode.sh "0<1;"
  [1]
  $ ./scripts/runcode.sh "1<1;"
  $ ./scripts/runcode.sh "2<1;"
  $ ./scripts/runcode.sh "0<=1;"
  [1]
  $ ./scripts/runcode.sh "1<=1;"
  [1]
  $ ./scripts/runcode.sh "2<=1;"

  $ ./scripts/runcode.sh "1>0;"
  [1]
  $ ./scripts/runcode.sh "1>1;"
  $ ./scripts/runcode.sh "1>2;"
  $ ./scripts/runcode.sh "1>=0;"
  [1]
  $ ./scripts/runcode.sh "1>=1;"
  [1]
  $ ./scripts/runcode.sh "1>=2;"

Multi statements
  $ ./scripts/runcode.sh "1; 2; 3;"
  [3]

Simple locals
  $ ./scripts/runcode.sh "a=3; a;"
  [3]
  $ ./scripts/runcode.sh "a=3; z=5; a+z;"
  [8]
  $ ./scripts/runcode.sh "a=b=3; a+b;"
  [6]

Longer local names
  $ ./scripts/runcode.sh "foo=3; foo;"
  [3]
  $ ./scripts/runcode.sh "foo123=3; bar=5; foo123+bar;"
  [8]
