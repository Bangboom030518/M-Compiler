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

// void srand(unsigned int seed)
fn srand @extern("srand", fn(UInt32) Void)
// fn rand @extern("rand", fn() UInt32)

fn print_int @extern("print_int", fn(USize) USize)
fn alloc_rs @extern("alloc_rs", fn(USize) USize)
fn dealloc_rs @extern("dealloc_rs", fn(USize, USize) USize)
fn copy_rs @extern("copy_rs", fn(USize, USize, USize) Void)
fn print_str @extern("print_str", fn(USize, USize) Void)

fn[T, @length L] Slice[T] slice(Array[T, L] array)
    Slice[T]
        ptr = @addr(array),
        length = 4,
    end
end

fn Void print(Slice[UInt8] data)
    print_str(data.ptr, data.length)
	0
end

fn UInt32 not_rand()
    42
end

fn UInt8 main()
    let data = slice[UInt8, 4]("Hi!\n")

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
