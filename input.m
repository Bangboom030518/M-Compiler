type UInt8 @u8
end

// type USize @usize
// end

// fn puts @extern("puts", fn(USize) UInt8)
fn UInt8 add(UInt8 a, UInt8 b)
    @add(a, b)
end

fn UInt8 main()
    add(2, 2)
end