type FunctionIterator<F, I> = struct where F: FuncMut<(), Optional<I>>
    generator F
    
    extend Iterator
        type Item = I
        
        fn next = (self)
            self.generator()

interface Iterator
    type Item

    fn next = (self MutRef<Self>) Optional<Item>

    fn map = <F, N>(self Self) Map<Self.Item, F, N> where F: FuncMut<(Self.Item), N>

type Map<I, F, N> = struct where I: Iterator, F: FuncMut<(I.Item), N>
    iterator I
    closure F

    extend Iterator
        type Item = I.Item

        fn next = (self)
            self.iterator.next().map(self.closure())

type Null = ()

type Optional<T> = union { T, Null }
    fn map<N, F> = (self Self, mapping F) Optional<N> where F: FuncMut<(T), N>
        match self
            value as T -> mapping(value)
            _ as Null -> Null()
    
    fn with_error<E>(self Self, error E) Fallible<T, E>
        match self
            value as T -> value
            error as E -> error

    fn unwrap_or = (self Self, default T) T
        match self
            value as T -> value
            _ as Null -> default

type Fallible<T, E> = union { T, E }
    fn map<F, N> = (self Self, mapping F) Result<N, E> where F: FuncMut<(T), N>
        match self
            value as T -> mapping(value)
            value as E -> value    
    
    fn map_error<F, N> = (self Self, mapping F) Result<T, N> where F: FuncMut<(E), N>
        match self
            value as T -> value
            value as E -> mapping(value)
    
    fn unwrap_or = (self Self, default T) T
        match self
            value as T -> value
            _ as Null -> default

interface operators.Construct
    fn construct(..fields Fields<Self>) Self
        Self { ..fields }
