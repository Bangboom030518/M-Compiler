type UInt8 = @u8
end

type UInt64 = @u64
end

type Hi = struct
    UInt8 byte0
    UInt8 byte1
    UInt8 byte2
end

// type HiString = @array(UInt8, 3)
// type HiString = @mutable_pointer(@array(UInt8, N))
// @mutable_slice(UInt64) --> @mutable_pointer(@array(UInt8, UNKNOWN))

type HiPtr = @mutable_pointer(Hi)
end

fn puts = @extern("puts", fn(HiPtr) UInt8)

fn print = (Hi pointer) UInt8
    let hi = Hi
        byte0 = 0x48, // 'H',
        byte1 = 0x69, // 'i',
        byte2 = 0x00, // '\0',
    end
    
    puts(@mutable_pointer(hi))
    0
end

fn str_test = () UInt8
    let hi = "hi"
    hi = "hello"
    0
end