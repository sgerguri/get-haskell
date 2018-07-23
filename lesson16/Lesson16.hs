module Lesson16 where

type FirstName = String
type LastName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName LastName
          | TwoInitialsWithLast Char Char LastName

data Author = Author Name
data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

data Book = Book {
  author    :: Creator,
  isbn      :: String,
  bookTitle :: String,
  bookYear  :: Int,
  bookPrice :: Double
}

data VinylRecord = VinylRecord {
  artist :: Creator,
  recordTitle :: String,
  recordYear :: Int,
  recordPrice :: Double
}

data Pamphlet = Pamphlet {
  pamphletTitle :: String,
  pamphletDescription :: String,
  contact :: String
}

data StoreItem = BookItem Book | RecordItem VinylRecord | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (PamphletItem _) = 0

data Shape = Circle Double | Square Double | Rectangle Double Double
  deriving (Eq, Show)

perimeter :: Shape -> Double
perimeter (Circle r) = 2*pi*r
perimeter (Square a) = 4*a
perimeter (Rectangle a b) = 2*a*b

area :: Shape -> Double
area (Circle r) = pi*r*r
area (Square a) = a*a
area (Rectangle a b) = a*b