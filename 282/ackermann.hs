ack 0 = (+1)
ack n = (\x y -> x (y + 1) 1) (rec (ack (n - 1)))
rec f 0 = (\x -> x)
rec f n = f . (rec f (n - 1))
