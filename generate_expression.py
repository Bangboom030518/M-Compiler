import random

operators = [ '*', '/', '+', '-', '%' ]

def main():
    output = str(random_term())
    for _ in range(100000):
        output += f' {random.choice(operators)} {random_term()}'

    #with open('input.txt', 'w') as file:
    #    file.write(output + ';')

    print(eval(output))

def random_term() -> int:
    return random.randint(1, 100)

if __name__ == '__main__': main()
