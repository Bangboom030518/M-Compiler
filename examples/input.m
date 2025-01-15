type UInt8 @u8 end
type USize @usize end
type UInt32 @u32 end
type Bool @bool end

type Bool = union(UInt8)
	false = 0,
	true = 1,
end

fn[T] UInt8 lte(T lhs, T rhs)
	@lte(lhs, rhs)
end

fn[T] UInt8 sus()
	lte[T](0, 0)
end

fn UInt32 main()
	lte[UInt32](0, 0)

	if @assert_type(true, Bool) then
		@assert_type(0, UInt8)
	else
		@assert_type(0, UInt8)
	end
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
