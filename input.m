// type Int32 = @i32
//     function add = (Self self, Self other) Self
//         // if @icmp(self, 2) & @icmp(other, 2)
//         //     return 5
//         @iadd(self, other)
type Int32 = @i32

function add = (Int32 self, Int32 other) Int32
    // if @icmp(self, 2) & @icmp(other, 2)
    //     return 5
    // @iadd(self, other)
    let a = @assert_type(1, Int32)
    a

// type Point = struct
//     UInt8 x
//     UInt8 y
//     UInt8 z
//     UInt8 w

//     type Inner = struct
//         String a

// type Bool =
//     true
//     false

// function main = () Nil ->
//     1 * 2 * 3
//     1 ** 2 ** 3