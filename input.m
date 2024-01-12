type Int64 = @i64

function add = (Int64 a, Int64 b) Int64
    @iadd(a, add2(a, b))

function add2 = (Int64 a, Int64 b) Int64
    @iadd(a, b)

type Int32 = @i32


// function fib = (Int64 n) Int64
//     if @lt(n, 2)
//         n
//     else
//         @iadd(fib(@isub(n, 1)), fib(@isub(n, 2)))
