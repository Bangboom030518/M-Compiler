type UInt8 @u8
end

type USize @usize
end

type Slice struct
    USize ptr,
    USize length,
end

type ByteArray @array(4, UInt8)
end

fn puts @extern("puts", fn(USize) UInt8)
fn malloc @extern("malloc", fn(USize) USize)
fn free @extern("free", fn(USize) UInt8)

fn UInt8 add(UInt8 a, UInt8 b)
    @add(a, b)
end

fn UInt8 puts_s(Slice data)
    puts(data.ptr)
    0
end

fn Slice hi()
    let str = @assert_type("Hi!\0", ByteArray)
    Slice
        ptr = @addr(str),
        length = 4,
    end
end

fn UInt8 main()
    // let ptr = malloc(8)
    // @store(ptr_add(ptr, 0), @assert_type(0x48, UInt8)) // 'H'
    // @store(ptr_add(ptr, 1), @assert_type(0x69, UInt8)) // 'i'
    // @store(ptr_add(ptr, 2), @assert_type(0x21, UInt8)) // '!'
    // @store(ptr_add(ptr, 3), @assert_type(0x00, UInt8)) // '\0'
    let a = hi()
    puts_s(a.ptr)
    0
end
