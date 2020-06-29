import Test.QuickCheck


emptyQueueL = []
enqueueL a  = (++ [a])
frontL      = head
dequeueL    = tail
sizeL       = length


toAbstract :: Queue -> [Int]
toAbstract (Q f sf r sr) = f ++ reverse r

prop_empty_ref = toAbstract emptyQueue == emptyQueueL

prop_enqueue_ref fq x = toAbstract (enqueue x fq)
                     == enqueueL x (toAbstract fq)

prop_size_ref fq = size fq == sizeL (toAbstract fq)

prop_front_ref fq = size fq > 0 ==> front fq == frontL (toAbstract fq)
prop_deq_ref fq = size fq > 0 ==>  toAbstract (dequeue fq)
                                == dequeueL (toAbstract fq)

prop_wf_empty = wellformed emptyQueue
prop_wf_enq x q = wellformed q ==> wellformed (enqueue x q)
prop_wf_deq x q = wellformed q && size q > 0 ==> wellformed (dequeue q)

data Queue = Q [Int] -- front of the queue
               Int   -- size of the front
               [Int] -- rear of the queue
               Int   -- size of the rear
             deriving (Show, Eq)

wellformed :: Queue -> Bool
wellformed (Q f sf r sr) = length f == sf && length r == sr
                        && sf >= sr

instance Arbitrary Queue where
  arbitrary = do
    NonNegative sf' <- arbitrary
    NonNegative sr <- arbitrary
    let sf = sf' + sr
    f <- vectorOf sf arbitrary
    r <- vectorOf sr arbitrary
    pure (Q f sf r sr)


inv3 :: Queue -> Queue
inv3 (Q f sf r sr)
   | sf < sr   = Q (f ++ reverse r) (sf + sr) [] 0
   | otherwise = Q f sf r sr

emptyQueue :: Queue
emptyQueue = Q [] 0 [] 0

enqueue :: Int -> Queue -> Queue
enqueue x (Q f sf r sr) = inv3 (Q f sf (x:r) (sr+1))

front :: Queue -> Int   -- partial
front (Q (x:f) sf r sr) = x

dequeue :: Queue -> Queue -- partial
dequeue (Q (x:f) sf r sr) = inv3 (Q f (sf -1) r sr)

size    :: Queue -> Int
size (Q f sf r sr) = sf + sr
