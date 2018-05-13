ifEven f x = if even x
             then f x
             else x

names = [("Ian", "Curtis"),
         ("Bernard", "Sumner"),
         ("Peter", "Hook"),
         ("Stephen", "Morris")]

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT else EQ
  where lastName1 = snd name1
        lastName2 = snd name2

compareNames name1 name2 = if lastRes == EQ
                           then firstRes
                           else lastRes
  where (first1, last1) = name1
        (first2, last2) = name2
        firstRes = compare first1 first2
        lastRes = compare last1 last2

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

sfOffice name = if lastName < "L"
                then nameText
                  ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                  ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = fst name ++ " " ++ snd name

dcOffice name = nameText ++ " - PO Box 567 - Washington DC, DC 1234"
  where nameText = fst name ++ " " ++ snd name ++ " Esq"

getLocationFunction location = case location of
                                 "ny" -> nyOffice
                                 "sf" -> sfOffice
                                 "reno" -> renoOffice
                                 "dc" -> dcOffice
                                 _ -> (\name -> fst name ++ " " ++ snd name)
