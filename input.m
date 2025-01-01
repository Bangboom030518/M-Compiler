type UInt8 @u8 end
type UInt32 @u32 end
type Void @void end

fn print_int @extern("print_int", fn(UInt32) UInt32)

fn UInt32 not_rand()
    42
end

fn[T] UInt8 eq(T left, T right)
	@eq(left, right)
end

fn UInt32 main()
    let result = if eq(not_rand(), 42) then 1 else 2 end
	print_int(result)
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
