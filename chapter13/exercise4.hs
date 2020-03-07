type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++ " Age was: " ++ show age

reportParsedPerson :: Either PersonInvalid Person -> String
reportParsedPerson (Right person) = "Yay! Successfully got a person: " ++ show person
reportParsedPerson (Left invalid) = "Invalid: " ++ show invalid

gimmePerson :: IO String
gimmePerson = do
  ageStr <- getLine
  let age = read ageStr :: Integer
  name <- getLine
  return (reportParsedPerson $ mkPerson name age)

