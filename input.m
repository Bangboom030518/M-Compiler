type UInt64 = @u64
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
//     let two = 2
//     if less_than(n, two)
//         n
//     else
//         add(fib(sub(n, 1)), fib(sub(n, 2)))
//     end
// end
type Point = struct
    UInt64 x
    UInt64 y
end

// TODO: structs in structs
type Rectangle = struct
    Point top_left
    Point bottom_right
end

type MutablePointyPointer = @mutable_pointer(Point)
end

type UInt64Ptr = @mutable_pointer(UInt64)
end

type MutablePointyPointerPointer = @mutable_pointer(MutablePointyPointer)
end

fn new_point = (UInt64 x, UInt64 y) UInt64
    let point = Point
        x = x,
        y = y,
    end

    let rect = Rectangle
        top_left = point,
        bottom_right = Point
            x = 1,
            y = 1,
        end
    end

    // let point_ptr = @assert_type(@mutable_pointer(point), MutablePointyPointer)
    // @deref(@assert_type(point_ptr.x, UInt64Ptr)) = 100
    // let point_ptr_ptr = @assert_type(@mutable_pointer(point_ptr), MutablePointyPointerPointer)
    // @deref(point_ptr_ptr) = @assert_type(@mutable_pointer(point2), MutablePointyPointer)

    let top_left = rect.top_left
    top_left.x
end
