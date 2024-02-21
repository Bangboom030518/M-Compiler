type UInt8 = @u8
end

type UInt64 = @u64
end

type ByteList = @mutable_slice(UInt8)
end

fn puts = @extern("puts", fn(USize) UInt8)
fn sus = @extern("sus", fn(UInt64) UInt8)

type Slice = struct
    USize ptr,
    USize length,
end

fn m_puts = (ByteList data)
    puts(@addrof(ByteList) + Math.Log(2, USize.MAX))
end

type Str10 = @array(UInt8, 10)
end

type Str = @mutable_slice(UInt64)
end
    
fn deref = (self) T
    @deref(self.ptr, T)
end

fn str_test = (mut Ptr<Point> point) UInt8
    @deref(point).x = 2
    // TODO: this is wrong - where's the length?????
    let mut string = "money money money!!!"
    string = "12"
    
    let a = 32
    @array(USize, a)
    puts_byte_list(@slice_pointer(string))
    0
end