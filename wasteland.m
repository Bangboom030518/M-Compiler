type UInt64 = @u64
end

type Int8 = @i8
end

fn sub = (UInt64 a, UInt64 b) UInt64
    @sub(a, b)
end

fn add = (UInt64 a, UInt64 b) UInt64
    @add(a, b)
end

fn less_than = (UInt64 a, UInt64 b) Int8
    @lt(a, b)
end

fn fib = (UInt64 n) UInt64
    let two = 2
    if less_than(n, two)
        n
    else
        add(fib(sub(n, 1)), fib(sub(n, 2)))
    end
end

type Point = struct
    UInt64 x
    UInt64 y
end

type Rectangle = struct
    Point top_left
    Point bottom_right
end

type MutablePointyPointer = @mutable_pointer(Point)
end

type UInt64Ptr = @mutable_pointer(UInt64)
end

type MutablePointyPointerPointer = @mutable_pointer(MutablePointyPointer)
end

fn new_point = ()
    let point = Point
        x = x,
        y = y,
    end

    let rect = Rectangle
        top_left = point,
        bottom_right = Point
            x = 1,
            y = 1,
        end
    end

    let point_ptr = @assert_type(@mutable_pointer(point), MutablePointyPointer)
    @deref(@assert_type(point_ptr.x, UInt64Ptr)) = 100
    let point_ptr_ptr = @assert_type(@mutable_pointer(point_ptr), MutablePointyPointerPointer)
    @deref(point_ptr_ptr) = @assert_type(@mutable_pointer(point2), MutablePointyPointer)

    let top_left = rect.top_left
    top_left.x
end

type PointList = @mutable_many_pointer(Point)
end

type SliceMut<T> = struct
    @mutable_many_pointer(T) ptr
    USize length
end

type DynArray<T> = struct
    @mutable_many_pointer(T) ptr
    USize length
    USize capacity
end

type PointList10 = @array(Point, 10)
end

type HelloWorld = struct
    UInt8 byte0
    UInt8 byte1
    UInt8 byte2
    UInt8 byte3
    UInt8 byte4
    UInt8 byte5
    UInt8 byte6
    UInt8 byte7
    UInt8 byte8
    UInt8 byte9
    UInt8 byte10
    UInt8 byte11
    UInt8 byte12
end

type HelloWorldPtr = @mutable_pointer(HelloWorld)
end

fn hello_world = () HelloWorld
    "Hello World!\0"
    HelloWorld
        byte0 = 'H',
        byte1 = 'e',
        byte2 = 'l',
        byte3 = 'l',
        byte4 = 'o',
        byte5 = ' ',
        byte6 = 'W',
        byte7 = 'o',
        byte8 = 'r',
        byte9 = 'l',
        byte10 = 'd',
        byte11 = '!',
        byte12 = '\0',
    end
end

fn puts = @extern("puts", fn(HelloWorldPtr) ())
end

fn main = () UInt64
    let string = hello_world()
    puts(@mutable_pointer(hello_world))
    0
end

interface ToString
    fn String to_string(Self self)
end

interface Iterator<T>
    fn T next(mut Ptr<Self> self)
end

type Map<T, U> struct
    Iterator<T> iterator
    Fn(T) -> U mapping
    
    implement Iterator<T>
        fn next(self)
            self.mapping(self.iterator.next())
        end
    end
end

fn<T, U> Map<T, U> map(Iterator<T> iterator, Fn<(T), U> mapping)
    Map
        iterator = iterator,
        mapping = mapping,
    end
end

type Null struct
end

fn main()
    print("Hello World")
end

type Counter struct
    count USize

    implement Iterator<USize>
        fn next(self)
            let count = self.counter;
            self.counter += 1;
            count
        end
    end

    implement Iterator<UInt64>
        fn next(self)
            let count = self.counter |> to_u64()
            self.counter += 1;
            count
        end
    end
end

fn a()
    capitalize(text)

    [0, 1, 2, 3]
        |> Array.into_iterator()
        |> map()
        |> collect_to_string()
        |> string.capitalize()
end

fn<I: Iterator> map Map<I> ()

type Point struct
    UInt8 x,
    UInt8 y,

    implement ToString
        fn to_string(Self self)
            sprintf("%u %u", self.x, self.y)
        end
    end
end

type UInt64 @u64
end

type ByteList @mutable_slice(UInt8)
end

fn puts @extern("puts", fn(USize) UInt8)

fn m_puts(ByteList data)
    puts(@addrof(ByteList) + Math.Log(2, USize.MAX))
end

type Str10 @array(UInt8, 10)
end

type Str @mutable_slice(UInt64)
end
    
fn deref = (self) T
    @deref(self.ptr, T)
end

fn UInt8 str_test(mut Ptr<Point> point)
    @deref(point).x = 2
    // TODO: this is wrong - where's the length?????
    let mut string = "money money money!!!"
    string = "12"
    
    let a = 32
    @array(USize, a)
    puts_byte_list(@slice_pointer(string))
    0
end