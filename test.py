from clformat import clformat
def test():
    """ 
>>> clformat("| ~{~<|~%| ~,33:;~2d ~>~}|", [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99])
|  0  1  2  3  4  5  6  7  8  9 |
| 10 11 12 13 14 15 16 17 18 19 |
| 20 21 22 23 24 25 26 27 28 29 |
| 30 31 32 33 34 35 36 37 38 39 |
| 40 41 42 43 44 45 46 47 48 49 |
| 50 51 52 53 54 55 56 57 58 59 |
| 60 61 62 63 64 65 66 67 68 69 |
| 70 71 72 73 74 75 76 77 78 79 |
| 80 81 82 83 84 85 86 87 88 89 |
| 90 91 92 93 94 95 96 97 98 99 |
>>> clformat("~5t~a ~15t~a ~25t~a~%", "thunder", "thunder", "rain")
     thunder   thunder   rain

>>> clformat("~5t~a ~15t~a ~25t~a~%", "iPhone", "cat", "stipulate")
     iPhone    cat       stipulate

>>> clformat("~5t~a ~15t~a ~25t~a~%", "lightning", "iPhone", "lightning")
     lightning  iPhone   lightning

>>> clformat("~5t~a ~15t~a ~25t~a~%", "rain", "pattern", "iPhone")
     rain      pattern   iPhone

>>> clformat("~30<~a~;~a~;~a~>~%", "cat", "rain", "cat")
cat          rain          cat

>>> clformat("~30<~a~;~a~;~a~>~%", "stipulate", "thunder", "thunder")
stipulate   thunder    thunder

>>> clformat("~30<~a~;~a~;~a~>~%", "lightning", "cat", "stipulate")
lightning    cat     stipulate

>>> clformat("~30<~a~;~a~;~a~>~%", "lightning", "stipulate", "cat")
lightning    stipulate     cat

>>> clformat("~30<~a~;~a~;~a~>~%", "lightning", "rain", "thunder")
lightning     rain     thunder

>>> clformat("~30<~a~;~a~;~a~>~%", "rain", "lightning", "cat")
rain       lightning       cat

>>> clformat("~30<~a~;~a~;~a~>~%", "iPhone", "iPhone", "stipulate")
iPhone    iPhone     stipulate

>>> clformat("~30<~a~;~a~;~a~>~%", "cat", "rain", "thunder")
cat        rain        thunder

>>> clformat("~30<~a~;~a~;~a~>~%", "iPhone", "thunder", "rain")
iPhone      thunder       rain

>>> clformat("~30<~a~;~a~;~a~>~%", "stipulate", "rain", "stipulate")
stipulate    rain    stipulate

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "iPhone", "rain", "thunder")
   iPhone   
    rain    
  thunder   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "stipulate", "stipulate", "lightning")
 stipulate  
 stipulate  
 lightning  

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "cat", "rain", "stipulate")
    cat     
    rain    
 stipulate  

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "thunder", "thunder")
  pattern   
  thunder   
  thunder   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "rain", "rain", "stipulate")
    rain    
    rain    
 stipulate  

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "stipulate", "thunder", "iPhone")
 stipulate  
  thunder   
   iPhone   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "cat", "thunder", "cat")
    cat     
  thunder   
    cat     

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "iPhone", "pattern")
  pattern   
   iPhone   
  pattern   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "rain", "rain", "pattern")
    rain    
    rain    
  pattern   

>>> clformat("~12:@<~a~>~%~12:@<~a~>~%~12:@<~a~>~%", "pattern", "iPhone", "cat")
  pattern   
   iPhone   
    cat     

    """ 