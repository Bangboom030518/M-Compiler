type UInt8 @u8 end
type UInt32 @u32 end
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
// void srand(unsigned int seed)
fn srand @extern("srand", fn(UInt32) UInt8)
fn rand @extern("rand", fn() UInt32)
fn printf @extern("printf", fn(USize, UInt32) UInt8)

fn[T, @length L] Slice[T] slice(Array[T, L] array)
    Slice[T]
        ptr = @addr(array),
        length = 3,
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

fn UInt32 not_rand()
    42
end

fn UInt8 main()
    let data = slice[UInt8, 3]("Hi!")
    srand(1230)
    // let a = not_rand()
    // @assert_type(@eq(rand(), 100), UInt8)
	
    let result = if @assert_type(@eq(rand(), 100), UInt8)
        1
    else
        print(data)
        2
    end
	printf(@addr(@assert_type("%u\n\0", Array[UInt8, 4])), result)
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
