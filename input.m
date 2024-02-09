type UInt8 = @u8
end

type Hi = struct
    UInt8 byte0
    UInt8 byte1
    UInt8 byte2
    UInt8 byte3
end

type HiPtr = @mutable_pointer(Hi)
end

// fn hi = () Hi
//     // "Hi\n\0"
//     Hi
//         byte0 = 0x48, // 'H',
//         byte1 = 0x69, // 'i',
//         byte2 = 0x0a, // '\n',
//         byte3 = 0x00, // '\0',
//     end
// end

fn puts = @extern("puts", fn(HiPtr) UInt8)

fn main = () UInt8
    let string = hi()
    puts(@mutable_pointer(string))
    0
end
