type UInt64 = @u64
end

type Point = struct
    UInt64 x
    UInt64 y
end

// TODO: structs in structs
// type Rectangle = struct 
//     Point top_left
//     Point bottom_right
// end

type Int8 = @i8
end

fn sub = (UInt64 a, UInt64 b) UInt64
    @sub(a, b)
end

fn add = (UInt64 a, UInt64 b) UInt64
    @add(a, b)
end

fn less_than = (UInt64 a, UInt64 b) Int8
    @lt(a, b)
end

fn fib = (UInt64 n) UInt64
    let two = 2
    if less_than(n, two)
        n
    else
        add(fib(sub(n, 1)), fib(sub(n, 2)))
    end
end

// type MutablePointyPointer @mutable_pointer(Point)

type MutablePointyPointer = @mutable_pointer(Point)
end

fn new_point = (UInt64 x, UInt64 y) UInt64
    let point = Point
        x = x,
        y = y,
    end

    point = Point
        x = fib(10),
        y = add(point.y, 1),
    end

    // let rect = Rectangle
    //     top_left = point,
    //     bottom_right = Point
    //         x = 1,
    //         y = 1,
    //     end,
    // end

    add_1_to_x(@mutable_pointer(point))
    // @add(point.x, point.y)
    point.x
end

fn add_1_to_x = (MutablePointyPointer point) UInt64
    point = Point
        x = add(point.x, 1),
        y = point.y,
    end
end

