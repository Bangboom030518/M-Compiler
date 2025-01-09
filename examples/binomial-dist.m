type UInt32 @u32 end
type UInt8 @u8 end
type Float32 @f32 end
type Void @void end

fn print_int @extern("print_int", fn(UInt32) Void)
fn print_float @extern("print_float", fn(Float32) Void)

fn UInt8 lte(UInt32 left, UInt32 right)
	@lte(left, right)
end

fn UInt8 eq(UInt32 left, UInt32 right)
	@eq(left, right)
end

fn Float32 fmul(Float32 left, Float32 right)
	@mul(left, right)
end

fn UInt32 mul(UInt32 left, UInt32 right)
	@mul(left, right)
end

fn UInt32 sub(UInt32 left, UInt32 right)
	@sub(left, right)
end

fn Float32 fsub(Float32 left, Float32 right)
	@sub(left, right)
end

fn UInt32 div(UInt32 left, UInt32 right)
	@div(left, right)
end

fn Float32 pow(Float32 base, UInt32 power)
	if eq(power, 1) then
		base
	else
		if eq(power, 0) then 1
		else fmul(base, pow(base, sub(power, 1)))
		end
	end
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

fn Float32 px(UInt32 n, UInt32 x, Float32 p)
	let c = combinations(n, x)
	let p = fmul(pow(p, x), pow(fsub(1, p), sub(n, x)))
	fmul(c, p)
end

fn UInt8 main()
	// print_float(px(20, 10, 0.5))
	print_float(pow(0.5, 2))
	0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
