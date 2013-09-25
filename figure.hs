type Point = (Float, Float)

data Figure = Circle Point Float
                        | Rectangle Point Float Float
                        | Union Figure Figure
                        | Intersection Figure Figure
                        deriving Show
                
cir1 = Circle (0,0) 5
rec1 = Rectangle (-1,-1) 2 2
un = Union cir1 rec1
ins = Intersection cir1 rec1

click :: Figure -> Point -> Bool
click (Circle (x0,y0) r) (x,y) = (x-x0)**2+(y-y0)**2 < r**2
click (Rectangle (x0,y0) a b) (x,y) = (x-x0) >= 0 && (x-x0) <= a && (y-y0) >= 0 && (y-y0) <= b
click (Intersection a b) (x, y) = click a (x,y) && click b (x,y)
click (Union a b) (x, y) = click a (x,y) || click b (x,y)