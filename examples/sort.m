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

fn UInt8 print(Slice[UInt8] data)
    print_str(data.ptr, data.length)
	0
end

fn[T, @length L] Slice[T] slice(Array[T, L] array)
	let ptr = alloc_rs(@mul(L, @sizeof(T)))
	copy_rs(@addr(array), ptr, @mul(L, @sizeof(T)))
    Slice[T]
        ptr = ptr,
        length = L,
    end
end

fn[T] T get_element(Slice[T] list, USize index)
	@load(@add(list.ptr, @mul(index, @sizeof(T))), T)
end

fn[T] UInt8 set_element(Slice[T] list, USize index, T value)
	@store(@add(list.ptr, @mul(index, @sizeof(T))), value)
	0
end

fn[T] UInt8 deinit_slice(Slice[T] slice)
	dealloc_rs(slice.ptr, slice.length)
	0
end

fn[T] Bool lte(T lhs, T rhs)
	@lte(lhs, rhs)
end

fn[T] Bool eq(T lhs, T rhs)
	@eq(lhs, rhs)
end

fn[T] UInt8 pass(Slice[T] list, USize index)
	if eq(index, @sub(list.length, 1)) then
		return 0
	else
		@assert_type(0, UInt8)
	end

	let lhs = get_element(list, index)
	let rhs = get_element(list, @add(index, 1))
	if lte(rhs, lhs) then
		set_element(list, index, rhs)
		set_element(list, @add(index, 1), lhs)
	else
		@assert_type(0, UInt8)
	end

	pass[T](list, @add(index, 1))
end

fn[T] UInt8 bubble_sort(Slice[T] list)
	if lte(list.length, 1) then
		return 0
	else
		@assert_type(0, UInt8)
	end
	
	pass[T](list, 0)

	bubble_sort[T](Slice[T]
		ptr = list.ptr,
		length = @sub(list.length, 1)
	end)
end

fn UInt32 main()
	let coconut = slice("COCONUT")
	bubble_sort(coconut)
	print(coconut)
	deinit_slice(coconut)
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
