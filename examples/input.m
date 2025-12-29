type UInt32 @u32 end

fn print_int @extern("print_int", fn(UInt32) UInt32)

fn[T] T add(T a, T b)
	@add(a, b)
end

fn UInt32 main()
	print_int(add(2, @assert_type(2, UInt32))) // this errors (mismatched types)
	// print_int(add[UInt32](2, 2)) // this works!
	0
end

