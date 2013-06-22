numeral_map = zip(
    (1000, 900, 500,  400, 100,  90,  50,   40,   10,  9,    5,   4,    1),
    ('M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I'))

def int_to_roman(i):
    if not (0 < i < 4000):
        raise ValueError("Integer must be in the range 0 > n > 4000")
    result = []
    for integer, numeral in numeral_map:
        count = int(i / integer)
        result.append(numeral * count)
        i -= integer * count
    return ''.join(result)
