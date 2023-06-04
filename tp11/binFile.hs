data DigBin = O | I 

type NBin = [DigBin]

data BinFile = Line (Maybe NBin) BinFile | EL BinFile | End

data Change = Add Int (Maybe NBin) | Remove Int | Replace Int (Maybe NBin) | Duplicate Int | Repeat Int Int (Maybe NBin)

construct :: [Change] -> BinFile
-- unconstruct :: BinFile -> [Change]
-- compress :: BinFile -> [(Int, NBin)]
-- uncompress :: [(Int, NBin)] -> BinFile

construct []  = End
construct cs  = foldr applyChange End cs

applyChange :: Change -> BinFile -> BinFile
applyChange (Add n nb)      = applyAdd n nb
applyChange (Remove n)      = applyRem n
applyChange (Replace n nb)  = applyReplace n nb
applyChange (Duplicate n)   = applyDup n
applyChange (Repeat n m nb) = applyRep n m nb

many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x) 

applyAdd :: Int -> Maybe NBin -> BinFile -> BinFile
applyAdd 0 nb binf              = addNBin nb binf
applyAdd n nb End               = EL (applyAdd (n-1) nb End)
applyAdd n nb (EL binf)         = EL (applyAdd (n-1) nb binf)
applyAdd n nb (Line nb' binf)   = Line nb' (applyAdd (n-1) nb binf)

addNBin :: Maybe NBin -> BinFile -> BinFile
addNBin Nothing     = EL
addNBin jnb   = Line jnb

applyRem :: Int -> BinFile -> BinFile
applyRem 0 binf             = removeL binf
applyRem n End              = End
applyRem n (EL binf)        = EL (applyRem (n-1) binf) 
applyRem n (Line nb binf)   = Line nb (applyRem (n-1) binf)

removeL :: BinFile -> BinFile
removeL End             = End
removeL (EL binf)       = binf
removeL (Line _ binf)   = binf

applyReplace :: Int -> Maybe NBin -> BinFile -> BinFile
applyReplace 0 nb binf             = replaceL nb binf
applyReplace n nb End              = End
applyReplace n nb (EL binf)        = EL (applyReplace (n-1) nb binf) 
applyReplace n nb (Line nb' binf)   = Line nb' (applyReplace (n-1) nb binf)

replaceL :: Maybe NBin -> BinFile -> BinFile
replaceL nb End             = End
replaceL nb (EL binf)       = addNBin nb binf
replaceL nb (Line nb' binf) = addNBin nb binf

applyDup :: Int -> BinFile -> BinFile
applyDup 0 binf             = duplicateL binf
applyDup n End              = End
applyDup n (EL binf)        = EL (applyDup (n-1) binf)
applyDup n (Line nb binf)   = Line nb (applyDup (n-1) binf)

duplicateL :: BinFile -> BinFile
duplicateL End              = End
duplicateL (EL binf)         = EL (EL binf)
duplicateL (Line nb binf)   = Line nb (Line nb binf)

applyRep :: Int -> Int -> Maybe NBin -> BinFile -> BinFile
applyRep 0 m nb binf             = many m (addNBin nb) binf
applyRep n m nb End              = End
applyRep n m nb (EL binf)        = EL (applyRep (n-1) m nb binf)
applyRep n m nb (Line nb' binf)   = Line nb' (applyRep (n-1) m nb binf)

