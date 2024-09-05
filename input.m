type UInt8 @u8 end
type UInt32 @u32 end
type USize @usize end
type Void @void end

type[T, @length L] Array @array(L, T) end

type[T] Slice struct
    USize length,
    USize ptr,
end

fn puts @extern("puts", fn(USize) UInt32)
fn malloc @extern("malloc", fn(USize) USize)
fn free @extern("free", fn(USize) Void)
fn memcpy @extern("memcpy", fn(USize, USize, USize) Void)
// void srand(unsigned int seed)
fn srand @extern("srand", fn(UInt32) Void)
// fn rand @extern("rand", fn() UInt32)
// fn printf @extern("printf", fn(USize, USize) UInt32)
fn print_int @extern("print_int", fn(USize) USize)

fn[T, @length L] Slice[T] slice(Array[T, L] array)
    Slice[T]
        ptr = @addr(array),
        length = 3,
    end
end

fn UInt8 print(Slice[UInt8] data)
	// printf(@addr(@assert_type("%p\n\0", Array[UInt8, 4])), data.ptr)
    let ptr = malloc(@add(data.length, 1))
    memcpy(ptr, data.ptr, data.length)
    @store(@add(ptr, data.length), @assert_type(0, UInt8)) // '\0'
    @store(@add(ptr, 0), @assert_type(@load(data.ptr, UInt8), UInt8)) // '\0'
    puts(ptr)
    free(ptr)
    0
end

fn UInt32 not_rand()
    42
end

fn UInt8 main()
    let data = slice[UInt8, 3]("Hi!")

	print_int(data.ptr)
	print_int(data.ptr)

    let result = if @assert_type(@eq(not_rand(), 100), UInt8)
        @assert_type(1, UInt8)
    else
		// printf(@addr(@assert_type("%p\n\0", Array[UInt8, 4])), data.ptr)
		print(data)
        @assert_type(2, UInt8)
    end
	
	// printf(@addr(@assert_type("%u\n\0", Array[UInt8, 4])), result)
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
