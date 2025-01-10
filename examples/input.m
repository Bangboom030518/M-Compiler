type UInt8 @u8 end
type USize @usize end
type UInt32 @u32 end

fn[T] UInt8 lte(T lhs, T rhs)
	@lte(lhs, rhs)
end

fn[T] UInt8 sus()
	lte[T](0, 0)
end

fn UInt32 main()
	// lte[UInt8](0, 0)
	lte[UInt32](0, 0)
	sus[UInt32]()
	// lte[USize](0, 0)
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
