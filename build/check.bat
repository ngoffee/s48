@echo off

set runnable="%~1"

echo ,batch > check.input
echo ,exec ,load =scheme48/debug/check.scm >> check.input
echo ,config ,load =scheme48/test-packages.scm >> check.input
echo ,open big-test >> check.input
echo ,open sockets-test >> check.input
echo ,open env-test >> check.input
echo ,config ,load =scheme48/sort/test-packages.scm >> check.input
echo ,open sort-test >> check.input
echo ,config ,load =scheme48/misc/packages.scm >> check.input
echo ,config ,load =scheme48/misc/test-packages.scm >> check.input
echo ,open misc-test >> check.input
echo ,config ,load =scheme48/r6rs/test-packages.scm >> check.input
echo ,open r6rs-test >> check.input
echo ,config ,load =scheme48/cml/test-packages.scm >> check.input
echo ,open cml-test >> check.input
echo ,config ,load =scheme48/srfi/test-packages.scm >> check.input
echo ,open portable-srfi-test >> check.input
echo ,open test-suites >> check.input
echo (define-test-suite all-tests (compiler-tests big-tests misc-tests tcp-sockets-tests sort-tests env-tests r6rs-tests cml-tests portable-srfi-tests)) >> check.input
echo (run-test-suite all-tests) >> check.input

%runnable% -h 8000000 < check.input
