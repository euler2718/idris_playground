||| Find the longer string
||| @word1 first string
||| @word2 second string
longer : (word1 : String) -> (word2 : String) -> Nat
longer w1 w2
    = let len1 = length w1
          len2 = length w2 in
          if len1 > len2 then len1 else len2