type UInt8 @u8
end

type USize @usize
end

type Slice struct
    USize length,
    USize ptr,
end

type[T, @length L] Array @array(L, T)
end

// type[T, @length L] Array @array(L, T)
// end

// fn[T, @length L] Slice[T] as_slice(Array[T, L] arr)
//     Slice
//         ptr = @addr(arr),
//         length = L, 
//     end
// end

fn puts @extern("puts", fn(USize) UInt8)
fn malloc @extern("malloc", fn(USize) USize)
fn free @extern("free", fn(USize) UInt8)
fn memcpy @extern("memcpy", fn(USize, USize, USize) UInt8)

fn UInt8 add(UInt8 a, UInt8 b)
    @add(a, b)
end

// fn [@add T] T add(T a, T b)
//     add[T](1, 2)
//     a + b
// end

fn UInt8 print(Slice data)
    let ptr = malloc(@add(data.length, 1))
    memcpy(ptr, data.ptr, data.length)
    @store(@assert_type(@add(ptr, data.length), USize), @assert_type(0, UInt8))
    puts(ptr)
    free(ptr)
    0
end

fn Slice hi()
    let str = @assert_type("Hi!", ByteArray)
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
    print(a)
    0
end
