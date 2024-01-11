type Int64 = @i64

type Int32 = @i32

function add = (Int64 a, Int64 b) Int64
    @iadd(a, b)

// function fib = (Int64 n) Int64
//     if @lt(n, 2)
//         n
//     else
//         @iadd(fib(@isub(n, 1)), fib(@isub(n, 2)))
