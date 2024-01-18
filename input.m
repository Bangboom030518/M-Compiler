type UInt64 = @u64
end

type Point = struct
    UInt64 x
    UInt64 y
end

// type Int8 = @i8
// end

// fn sub = (UInt64 a, UInt64 b) UInt64
//     @sub(a, b)
// end

// fn add = (UInt64 a, UInt64 b) UInt64
//     @add(a, b)
// end

// fn less_than = (UInt64 a, UInt64 b) Int8
//     @lt(a, b)
// end

// type Int32 = @i32
// end

// fn fib = (UInt64 n) UInt64
//     let two = @assert_type(2, UInt64)
//     if less_than(n, two)
//         n
//     else
//         add(fib(sub(n, 1)), fib(sub(n, 2)))
//     end
// end

fn x = (Point p)
fn x = (UInt64 x, UInt64 y)

fn new_point = (UInt64 x, UInt64 y) UInt64
    let x = x
    let y = y
    let point = Point
        x = x,
        y = 1,
    end
end
