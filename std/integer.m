interface Number: operator.Plus + operator.Subtract + operator.Divide + operator.Multiply
    const MAX Self
    const SIZE Size

interface Float: Number 

interface ProperFraction: Number<MAX = 1>

type ProperF32 = @pf32
type ProperF64 = @pf64

type Angle<N> = struct where N: number
    number N

    fn sin<F>(Self self) F where F: ProperFraction
