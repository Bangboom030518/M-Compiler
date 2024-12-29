type UInt8 @u8 end
type UInt32 @u32 end
type Void @void end

type[T] Wrapper struct
	T value
end

fn print_int @extern("print_int", fn(UInt32) UInt32)

fn UInt32 not_rand()
    42
end

fn[T] UInt8 eq(Wrapper[T] left, T right)
	@eq(left, right)
end

fn UInt32 main()
	let lhs = Wrapper value = not_rand() end
    let result = if eq(lhs, 42) then 1 else 2 end
	print_int(result)
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
