EXPONENT_BITS = 8

# 0 to MAX
MAX = 2 ** (EXPONENT_BITS - 1)

# -MAX_SHIFTED to MAX_SHIFTED
MAX_SHIFTED = MAX / 2

# 
def get_float_exponent(number: int) -> int:
    return number - MAX_SHIFTED


def main():
    print(f'{MAX=}')
    print(f'{MAX_SHIFTED=}')
    float_exponent = get_float_exponent(4)
    print(f'{float_exponent=}')


if __name__ == '__main__':
    main()
