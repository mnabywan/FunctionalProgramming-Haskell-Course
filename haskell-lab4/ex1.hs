polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r, phi) = (r* cos phi, r* sin phi)

type CartesianCoord' a = (a,a)
type PolarCooord' a = (a,a)

polarToCartesian' :: Floating a => PolarCooord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r* cos phi, r* sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCooord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCooord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r* cos phi, r* sin phi)

--newtype [constr. of a newtype must have exactly one field!]
--type [synonim, alias]

personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType' 
personInfoToString' (a,b,c)= ("name:" ++ a ++", suraneme: " ++ b++ ", address: " ++c)



--newtype PersonInfo1  a =  MkPersonalInfo (a,a,a)
--personInfoToString'' :: PersonInfo1 a -> String
--personInfoToString'' (MkPersonalInfo (name,surname,address)) = ("name:" ++ name++", suraneme: " ++ surname++ ", address: " ++address)


