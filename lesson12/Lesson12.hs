module Lesson12 where

type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

data Sex = Male | Female deriving (Show)

data RhType = Pos | Neg deriving (Show)
data ABOType = A | B | AB | O deriving (Show)
data BloodType = BloodType ABOType RhType deriving (Show)

data Patient = Patient { name :: String,
                         sex :: Sex,
                         age :: Int,
                         height :: Int,
                         weight :: Int,
                         bloodType :: BloodType }

canDonateTo :: Patient -> Patient -> Bool
canDonateTo Patient {bloodType=BloodType O _} Patient {} = True
canDonateTo Patient {} Patient {bloodType=BloodType AB _} = True
canDonateTo Patient {bloodType=BloodType A _} Patient {bloodType=BloodType A _} = True
canDonateTo Patient {bloodType=BloodType B _} Patient {bloodType=BloodType B _} = True
canDonateTo Patient {} Patient {} = False

patientSummary :: Patient -> String
patientSummary p = delimiter ++ "\n" ++
  "Patient Name: " ++ show (name p) ++ "\n" ++
  "Sex: " ++ show (sex p) ++ "\n" ++
  "Age: " ++ show (age p) ++ "\n" ++
  "Height: " ++ show (height p) ++ " in.\n" ++
  "Weight: " ++ show (weight p) ++ " lbs.\n" ++
  "Blood Type: " ++ show (bloodType p) ++ "\n" ++
  delimiter
  where delimiter = "**************"
