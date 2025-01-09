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
fn print_int @extern("print_int", fn(UInt32) UInt32)
fn alloc_rs @extern("alloc_rs", fn(USize) USize)
fn dealloc_rs @extern("dealloc_rs", fn(USize, USize) USize)
fn copy_rs @extern("copy_rs", fn(USize, USize, USize) Void)
fn print_str @extern("print_str", fn(USize, USize) Void)

fn UInt8 print(Slice[UInt8] data)
    print_str(data.ptr, data.length)
	0
end

fn[T, @length L] Slice[T] slice(Array[T, L] array)
    Slice[T]
        ptr = @addr(array),
        length = L,
    end
end

fn UInt8 main()
    let data = slice("Hi!\n")
	print(data)
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
