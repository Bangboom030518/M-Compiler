type UInt8 @u8 end
type USize @usize end
type[T, @length L] Array @array(L, T) end

type[T] Slice struct
    USize length,
    USize ptr,
end

fn puts @extern("puts", fn(USize) UInt8)
fn malloc @extern("malloc", fn(USize) USize)
fn free @extern("free", fn(USize) UInt8)
fn memcpy @extern("memcpy", fn(USize, USize, USize) UInt8)

fn UInt8 add(UInt8 a, UInt8 b)
    @add(a, b)
end

fn[T, @length L] Slice[T] slice(Array[T, L] array)
    Slice[T]
        ptr = @addr(str),
        length = L,
    end
end

fn UInt8 print(Slice[UInt8] data)
    let ptr = malloc(@add(data.length, 1))
    memcpy(ptr, data.ptr, data.length)
    @store(@add(ptr, data.length), @assert_type(0, UInt8)) // '\0'
    puts(ptr)
    free(ptr)
    0
end

fn UInt8 main()
    let a = slice[UInt8, 3]("Hi!")
    print(a)
    0
end
