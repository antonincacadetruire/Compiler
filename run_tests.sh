# Compile the tests in the test directory and run them. 
# If an argument new is passed with a string, it will skip everything and create
# The according c file in the ./tests directory, with its corresponding .exp file

if [ "$1" = "new" ]
then
    if [ "$2" = "" ]
    then
        echo "Please provide a name for the new test"
        exit 1
    fi
    echo "Creating new test $2"
    touch ./tests/$2.c
    touch ./tests/$2.exp
    echo "Please fill in the content of the test in ./tests/$2.c and the expected output in ./tests/$2.exp"
    exit 0
fi

# Compile the compiler
ocamlc -o compiler.exe compiler.ml

build_dir="build_tests"
err_dir="err_tests"
test_dir="tests"
test_files=$(find $test_dir -name "*.c")

# empties the build and err directory
rm -rf $build_dir
rm -rf $err_dir

echo "-------------------"
echo "Compile tests :"
echo "-------------------"

# Compile them using ./compiler.exe
mkdir -p $build_dir
mkdir -p $err_dir
list_of_correctly_compiled_files=""
for test_file in $test_files
do
    echo "Compiling $test_file"
    ./compiler.exe ./std/standard.c $test_file > $build_dir/$(basename $test_file .c) 2> $err_dir/$(basename $test_file .c).err
    if [ $? -eq 0 ]
    then
        list_of_correctly_compiled_files="$list_of_correctly_compiled_files $build_dir/$(basename $test_file .c)"
    else
        echo "Error compiling $test_file"
    fi
done

echo "-------------------"
echo "Running tests :"
echo "-------------------"

# Run the tests

chmod +x $build_dir/*
chmod +x ./msm/msm

# Expected std out of each test can be found at ./tests/$name_of_file.exp
# Here test_file includes the build directory
for test_file in $list_of_correctly_compiled_files
do
    echo "Running $test_file"
    # create .out files for each test in the build directory
    if [ $test_file = "build_tests/std1" ]
    then
        echo "-------------------"
        echo "press enter to pass next test"
        echo "test de read function"
        echo "-------------------"
    fi
    ./msm/msm $test_file > $test_file.out
    # create exp file if it does not exists, and echoes a message to user to fill it in
    if [ ! -f ./tests/$(basename $test_file).exp ]
    then
        echo "Expected output file for $(basename $test_file) does not exist. Please fill it in."
        touch ./tests/$(basename $test_file).exp
    fi

    difference=$(diff $test_file.out ./tests/$(basename $test_file).exp)

    # compare the output of the test with the expected output
    diff $test_file.out ./tests/$(basename $test_file).exp > /dev/null
    if [ $? -eq 0 ]
    then
        echo "Test passed"
    else
        echo "$test_file failed : "
        echo "$difference"
    fi
done