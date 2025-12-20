type UInt8 @u8 end
type USize @usize end
type UInt32 @u32 end
type UInt16 @u16 end
type Void @void end
type Bool @bool end

type[T, @length L] Array @array(L, T) end

type[T] Slice struct
    USize length,
    USize ptr,
end

fn print_int @extern("print_int", fn(UInt32) UInt32)
fn alloc_rs @extern("alloc_rs", fn(USize) USize)
fn dealloc_rs @extern("dealloc_rs", fn(USize, USize) USize)
fn print_str @extern("print_str", fn(USize, USize) Void)
fn copy_rs @extern("copy_rs", fn(USize, USize, USize) Void)

fn[T] T mul(T lhs, T rhs)
	@mul(lhs, rhs)
end

fn[T, @length L] Slice[T] slice(Array[T, L] array)
	let ptr = alloc_rs(mul(@assert_type(L, USize), @sizeof(T)))
	copy_rs(@addr(array), ptr, mul(@assert_type(L, Usize), @sizeof(T)))
    Slice[T]
        ptr = ptr,
        length = L,
    end
end

fn UInt32 main()
	let coconut = slice[UInt8, 7]("COCONUT")
  0
end
