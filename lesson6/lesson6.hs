isPalindrome word = word == reverse word

takeLast n aList = reverse (take n (reverse aList))

assignToGroups n aList = zip groups aList
  where groups = cycle [1..n]

repeat x n = take n $ cycle [x]

-- This version will consume stack space, which `repeat` may not.
repeat2 x 0 = []
repeat2 x n = x : repeat2 x (n-1)

subseq s e l = take (e-s) $ drop s l

inFirstHalf x l = elem x firstHalf
  where halfIndex = quot (length l) 2
        firstHalf = subseq 0 halfIndex l
