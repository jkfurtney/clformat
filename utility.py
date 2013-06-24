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

def base10toN(num,n):
    """Change a  to a base-n number.
    Up to base-36 is supported without special notation."""
    #http://stackoverflow.com/questions/2267362/convert-integer-to-a-string-in-a-given-numeric-base-in-python
    num_rep={10:'a', 11:'b', 12:'c', 13:'d', 14:'e', 15:'f', 16:'g',
             17:'h', 18:'i', 19:'j', 20:'k', 21:'l', 22:'m', 23:'n',
             24:'o', 25:'p', 26:'q', 27:'r', 28:'s', 29:'t', 30:'u',
             31:'v', 32:'w', 33:'x', 34:'y', 35:'z'}
    new_num_string=''
    current=num
    while current!=0:
        remainder=current%n
        if 36>remainder>9:
            remainder_string=num_rep[remainder]
        elif remainder>=36:
            remainder_string='('+str(remainder)+')'
        else:
            remainder_string=str(remainder)
        new_num_string=remainder_string+new_num_string
        current=current/n
    return new_num_string
