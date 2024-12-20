type UInt32 @u32 end
type UInt8 @u8 end
type Void @void end

fn print_int @extern("print_int", fn(UInt32) Void)

fn UInt8 lte(UInt32 left, UInt32 right)
	@lte(left, right)
end

fn UInt32 mul(UInt32 left, UInt32 right)
	@mul(left, right)
end

fn UInt32 sub(UInt32 left, UInt32 right)
	@sub(left, right)
end

fn UInt32 div(UInt32 left, UInt32 right)
	@div(left, right)
end

fn UInt32 factorial(UInt32 n)
	if lte(n, 1) then
		1
	else
		mul(n, factorial(sub(n, 1)))
	end
end

fn UInt32 combinations(UInt32 n, UInt32 r)
	div(factorial(n), mul(factorial(sub(n, r)), factorial(r)))
end

fn UInt8 main()
	print_int(combinations(10, 6))
	0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
