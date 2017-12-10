--let x = 5 in x
l1 = n
 where n = 5
-- let x = 5 in x * x
l2 = x * x
 where
  x = 5
-- let x = 5; y :6 in x * y
l3 = x * y
 where 
  x = ((\j -> j * 2)2)
  y = 6
-- let x = 3; y = 1000 in x + 3	
l4 = x + 3
 where 
  x = 3
  y = 1000
 -- let x = 3; y=1000 in x * 3 + y
l5 = x * 3 + y
 where 
  x = 3
  y = 1000
-- let y = 10; x = 10 * 5 + y in x * 5
l6 = x * 5
 where
  y = 10
  x = 10 * 5 + y
-- let x = 7 y = negate x; z = y * 10  in z / x + y
l7 = z / x + y
 where
 x = 7
 y = negate x
 z = y * 10
--

waxOn x = x * 5
waxOff x = triple x
triple x = x * 3