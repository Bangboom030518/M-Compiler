type UInt8 @u8 end
type USize @usize end
type UInt32 @u32 end
type Bool @bool end
type Void @void end

fn Bool true()
	@true()
end

// fn Void sus()
	
// end

fn UInt8 sus(Void a)
	0
end

fn UInt32 main()
	// @assert_type(0, Void)
	sus(if true() then
	end)
	if true() then
		true()
	end

	// if true() then
	// 	@assert_type(0, UInt8)
	// else
	// 	@assert_type(0, UInt8)
	// end
    0
end

//# vim: commentstring=//%s tabstop=4 shiftwidth=4
