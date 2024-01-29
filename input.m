type UInt64 = @u64
end

type Point = struct
    UInt64 x
    UInt64 y
end

type Int8 = @i8
end

// fn sub = (UInt64 a, UInt64 b) UInt64
//     @sub(a, b)
// end

// fn add = (UInt64 a, UInt64 b) UInt64
//     @add(a, b)
// end

// fn less_than = (UInt64 a, UInt64 b) Int8
//     @lt(a, b)
// end

// fn fib = (UInt64 n) UInt64
//     let two = @assert_type(2, UInt64)
//     if less_than(n, two)
//         n
//     else
//         add(fib(sub(n, 1)), fib(sub(n, 2)))
//     end
// end

// type MutablePointyPointer @mutable_pointer(Point)

type MutablePointyPointer = @mutable_pointer(Point)
end

// fn new_point = (UInt64 x, UInt64 y) UInt64
//     let point = Point
//         x = x,
//         y = y,
//     end
//     add_1_to_x(@mutable_pointer(point))
//     @add(point.x, point.y)
// end

// fn add_1_to_x = (MutablePointyPointer point) UInt64
//     point.x = add(point.x, 100)
//     0
// end

fn a = () UInt64
    0
end