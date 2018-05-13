ifEven f x = if even x
                then f x
                else x

genIfEven x = \f -> ifEven f x

ifEvenInc = ifEven (+1)
ifEvenDouble = ifEven (*2)
ifEvenSquare = ifEven (**2)

getRequest host apiKey resource id =
  host ++
    "/" ++
      resource ++
        "/" ++
          id ++
            "?token=" ++
              apiKey

genApiRequestBuilder hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

genBookUrl = getRequest "http://example.com" "1337hAsk3ll" "book"

subtract2 = flip (-) 2

binaryPartialApplication f x = \y -> f x y
