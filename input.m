type Int64 = @i64

type Int8 = @i8

// function is_ten = (Int64 a, Int64 b) Int8
//     if @assert_type(@eq(a, b), Int8)
//         1
//     else
//         0

// function add2 = (Int64 a, Int64 b) Int64
//     @iadd(a, b)

type Int32 = @i32

function fib = (Int64 n) Int64
    if @assert_type(@lt(n, 2), Int8)
        n
    else
        @iadd(fib(@iadd(n, -1)), fib(@iadd(n, -2)))
