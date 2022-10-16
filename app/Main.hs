module Main where

-- Import Dependency Library
import Data.List
import Data.Time
import Data.Time.Format
import qualified Data.Char as Foo
import Data.Time.Clock
import Data.Time.Calendar
import System.Console.ANSI

-- Data Type Setup Configuration
type CircuProduct = (ProductName, CirCycle,AssetChar,CircuType)
type AssetReg = (ProductName,AssetChar,SystemDate)
type AssetRegistered = (ProductName,SystemDate,DateReg)
type AssetRegisteredByName = (ProductName,AssetChar,CirProb,CircuType,CirCycle)
type ProductIdentity = (ProductName,CirCycle,CirProb)
type ProductName = String
type CirCycle = String
type AssetChar = [String]
type Symptons2 = String
type CircuType = String
type SystemDate = String
type DateReg = String
type CircularAssetName = String
type CirProb = Integer

-- Parse Date Data
current_data :: Day
current_data = parseDay "2022-10-12"

-- List Function for Circular Product and Asset Registration
list_circularproduct :: [CircuProduct]
list_circularproduct = []
list_cirasset :: [AssetReg]
list_cirasset = []
list_circularproduct_registered :: [AssetRegistered]
list_circularproduct_registered = []


-- Check and Count how many values in two lists are equal
checkIfContains :: [String] -> String -> Integer
checkIfContains x y = case elemIndex y x of
                    Nothing -> 0
                    Just n  -> 1

checkIfContainsList :: [String] -> [String] -> Integer
checkIfContainsList _ [] = 0
checkIfContainsList [] _ = 0
checkIfContainsList x (y:ys) = if (checkIfContains x y >= 1) then 1 + checkIfContainsList x ys else checkIfContainsList x ys


-- Makes comparisons between the Product Registered and all the Circular Asset Category to discover Recycle Circular Product are identified 
-- (in the search by name -> Circular Asset Category | returns whether the Circular Asset Category is Recycle )

compareAssetCharAll :: AssetReg -> [CircuProduct] -> [ProductIdentity]
compareAssetCharAll _ [] = []
compareAssetCharAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::ProductName,yc::CirCycle,checkIfContainsList xd yd::CirProb)::ProductIdentity):compareAssetCharAll (xn,xd,xp) ys

compareAssetCharAllByName :: String -> [AssetReg] -> [CircuProduct] -> [ProductIdentity]
compareAssetCharAllByName name [] [] = []
compareAssetCharAllByName name [] _ = []
compareAssetCharAllByName name ((xn,xd,xp):xs) y = if (xn == name) then (compareAssetCharAll (xn,xd,xp) y) ++ (compareAssetCharAllByName name xs y) else (compareAssetCharAllByName name xs y) 

max_list :: [ProductIdentity] -> String
max_list [(n,v,c)] = "Product Name: " ++ n ++ " -> Circular Category: " ++ v 
max_list ((x1,v1,c1):(x2,v2,c2):xs) = if (c1 > c2) then max_list ((x1,v1,c1):xs) else max_list((x2,v2,c2):xs)


-- Makes comparisons between all Product or Asset with Circular Database to discover circular identity 
-- of Product/ Asset  (returns a list of Product/ Asset  and Product Characteristic/ Identity )

circuTypeCompareAssetCharAll :: AssetReg -> [CircuProduct] -> [AssetRegisteredByName]
circuTypeCompareAssetCharAll _ [] = []
circuTypeCompareAssetCharAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::ProductName,xd::AssetChar,checkIfContainsList xd yd::CirProb, yp::CircuType, yc::CirCycle)::AssetRegisteredByName):circuTypeCompareAssetCharAll (xn,xd,xp) ys

circuTypecompareAssetCharAllByName :: String -> [AssetReg] -> [CircuProduct] -> [AssetRegisteredByName]
circuTypecompareAssetCharAllByName name [] [] = []
circuTypecompareAssetCharAllByName name [] _ = []
circuTypecompareAssetCharAllByName name _ [] = []
circuTypecompareAssetCharAllByName name ((xn,xd,xp):xs) y = if (xn == name) then (circuTypeCompareAssetCharAll (xn,xd,xp) y) ++ (circuTypecompareAssetCharAllByName name xs y) else (circuTypecompareAssetCharAllByName name xs y) 

circuTypeMax_list :: [AssetRegisteredByName] -> String
circuTypeMax_list [(n,s,c,q,v)] = q
circuTypeMax_list ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then circuTypeMax_list ((n1,s1,c1,q1,v1):xs) else circuTypeMax_list((n2,s2,c2,q2,v2):xs)

circularproductMax_listGraph :: [AssetRegisteredByName] -> String
circularproductMax_listGraph [(n,s,c,q,v)] = "\n Product Asset Name-Status:  " ++ n  ++ "\n -- Circular Asset Type: " ++ v ++ "\n -- Circular Method Recommendation: " ++ q ++ ";" ++ "\n "
circularproductMax_listGraph ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then circularproductMax_listGraph ((n1,s1,c1,q1,v1):xs) else circularproductMax_listGraph((n2,s2,c2,q2,v2):xs)


-- Function That converts DATA-STRING to DATA-DAY
parseDay :: String -> Day
parseDay s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s

-- ******************************************
-- Functions to Register Circular Economy Product/Asset Database 
-- https://ipfs.io/ipfs/QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH?filename=circularproduct.txt
insert_cirprod :: IO()
insert_cirprod = do clearScreen 
                    putStr "********************************************"
                    putStr "** Circular Product Database Registration **"
                    putStr "********************************************"
                    putStrLn(" \n")
                    putStr "Product Name: (xxxx-xxxx-xxxx) \n"
                    n <- getLine
                    putStr "Product Cycles Category: (1-Biological 2-Technical) \n"
                    c <- getLine
                    putStr "Product Characteristic: (xxxxx,xxxxx,....,xxxx)  \n"
                    d <- getLine
                    putStr "Circular Category: (1-New Product, 2-Reused/Refurbished , 3-Recycle) \n"
                    p <- getLine
                    --appendFile "https://ipfs.io/ipfs/QmbFMke1KXqnYyBBWxB74N4c5SBnJMVAiMNRcGu6x1AwQH?filename=circularproduct.txt" (n ++ "\t" ++ c ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                    appendFile "circularproduct.txt" (n ++ "\t" ++ c ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                    putStr "Insert another one? (y/n): \n"
                    resp <- getLine
                    if (resp=="y" || resp=="Y") then insert_cirprod else return()

-- Function to Register Product/ Asset to be Analyzed
insert_prodreg :: IO()
insert_prodreg = do clearScreen 
                    putStr "**********************************"
                    putStr "**  Circular  Asset  Registered **"
                    putStr "**********************************"
                    putStrLn(" \n")
                    putStr "Product Name: \n"
                    n <- getLine
                    putStr "Asset Defect Identified Registered: \n"
                    d <- getLine
                    putStr "Asset Date Registered (yyyy-mm-dd): \n"
                    p <- getLine
                    print("Asset Registered Processed... \n")
                    appendFile "circularproductregistered.txt" (n ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                    print("Data Saved Success! \n")
                    putStr "Add another one? : \n"
                    resp <- getLine
                    if (resp=="y" || resp=="Y") then insert_prodreg else return()


-- Functions that load the table of Circular Product Registered (from TXT files )
loadTab_cirasset = do s <-readFile "circularproductregistered.txt"
                      return (gerlist_cirasset (map words (lines s)))

gerlist_cirasset [] = []
gerlist_cirasset ([n,d,p]:xs) = ((n::ProductName,split d::AssetChar,p::SystemDate)::AssetReg):(gerlist_cirasset xs)

print_lst_cirproduct [] =""
print_lst_cirproduct ((n,d,p):xs) = "Product Name = " ++ n ++ ", Product Characteristic = [ " ++ print_symptons_each d ++ "], Registration Date = " ++ p ++ "\n"  ++ (print_lst_cirproduct xs) 

gerlist_circutype [] = []
gerlist_circutype ([n,d,p]:xs) = ((n::ProductName,split d::AssetChar,p::SystemDate)::AssetReg):(gerlist_cirasset xs)

loadTab_circularproduct = do s <-readFile "circularproduct.txt"
                             return (gerlist_circuproduct (map words (lines s)))

gerlist_circuproduct [] = []
gerlist_circuproduct ([n,c,d,p]:xs) = ((n::ProductName,c::CirCycle,split d::AssetChar,p::CircuType)::CircuProduct):(gerlist_circuproduct xs)


-- Function that loads the table of Product/ Asset Circular Type -> Based on the table of Product Asset Circular Database.
loadTab_circuType p d = do return (isCircuType p d)

isCircuType :: [AssetReg] -> [CircuProduct] -> [AssetRegistered]
isCircuType [] [] = []
isCircuType _ [] = []
isCircuType [] _ = []
isCircuType ((xn,xd,xp):xs) y = if (circuTypeMax_list (circuTypecompareAssetCharAllByName xn ((xn,xd,xp):xs) y) == "3-Recycle") then return ((xn::ProductName,xp::SystemDate,showGregorian (addDays 40 (parseDay xp))::DateReg)::AssetRegistered) ++ isCircuType xs y else isCircuType xs y


-- Functions to Print Product/ Asset Circular Registered Tables
print_lst_circuproduct [] =""
print_lst_circuproduct ((n,c,d,p):xs) = "Product = " ++ n ++ ", Cycle Category = " ++ c ++ ", Prod. Characteristic = [ " ++ print_symptons_each d ++ "], Circular Cat. = " ++ p ++ "\n" ++ (print_lst_circuproduct xs)

print_lst_circutype date [] =""
print_lst_circutype date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) < 0) then "Product = " ++ n ++ ", Date Registered = " ++ c ++ "\n" ++ (print_lst_circutype date xs) else (print_lst_circutype date xs)

print_lst_circutype_newDate_still date [] =""
print_lst_circutype_newDate_still date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) <= 0) then "Product = " ++ n ++ ", Date Registered = " ++ c ++ "\n" ++ (print_lst_circutype_newDate_still date xs) else (print_lst_circutype_newDate_still date xs)

print_lst_circutype_newDate_out date [] =""
print_lst_circutype_newDate_out date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) >= 0) then "Product = " ++ n ++ ", Date Registered = " ++ c ++ "\n" ++ (print_lst_circutype_newDate_out date xs) else (print_lst_circutype_newDate_out date xs)


-- Function that counts the number of Selected Product or Asset Circular Type
count_circuType [] = 0
count_circuType (x:xs) = 1 + count_circuType xs


-- Function that converts a STRING to [STRING] (entering symptoms separated by commas in a LIST)
split str = case break (==',') str of
                (a, ',':b) -> a : split b
                (a, "")    -> [a]

print_symptons_each [] = ""
print_symptons_each (x:xs) = x ++ ", " ++ print_symptons_each xs


-- Function that loads the TXT with the current system date
load_date = do s <-readFile "datesystem.txt"
               return (parseDay s)


-- Function that generates a graph of ( Product Name -> Circular Cycle )
graph :: [AssetReg] -> [CircuProduct] -> String
graph [] [] = ""
graph _ [] = ""
graph [] _ = ""
graph ((xn,xd,xp):xs) y = circularproductMax_listGraph (circuTypecompareAssetCharAllByName xn ((xn,xd,xp):xs) y) ++ graph xs y


-- Main Function that Contains Menu Selection Applicable within Application
main :: IO()
main = do list_cirasset <- loadTab_cirasset
          list_circularproduct <- loadTab_circularproduct
          current_data <- load_date
          now <- getCurrentTime
          let (year, month, day) = toGregorian $ utctDay now
          list_circularproduct_registered <- loadTab_circuType list_cirasset list_circularproduct         
          putStrLn(" ")
          putStrLn "************************************************************************************"     
          putStrLn "**        PRODUCT CIRCULAR ECONOMY TRACKING & RECOMMENDER MANAGEMENT SYSTEM       **"
          putStrLn "** IoT - AI Data Analytics (CCTV Live Image & UT Scanned) - Blockchain Technology **"
          putStrLn "************************************************************************************"
          putStr ("[ Current System Date: " ++ show day ++ "-" ++ show month ++ "-" ++ show year ++ " ]\n")
          putStr "[1] Add Circular Product/Asset Database \n"
          putStr "[2] Add Product/Asset Registered ->> Future Input through IoT-AI Material Classification \n"
          putStr "[3] List All Circular Product/Asset Database \n"
          putStr "[4] List All Product/Asset Registered \n"
          putStr "[5] List of Top 3 (Three) Circular Product Registered \n"
          putStr "[6] Counting Total Product in Recycle Category \n"
          putStr "[7] Search for Product Items \n"
          putStr "[8] Updated Date System \n"
          putStr "[9] Infographic ->> Future Insight on Web3 Android IOS dApps \n"
          putStr "Option Selection: \n"
          resp <- getLine
          if (resp=="1") then do insert_cirprod
                              
          else if (resp=="2") then do insert_prodreg

          else if (resp=="3") then do putStr(print_lst_circuproduct list_circularproduct)

          else if (resp=="4") then do putStr(print_lst_cirproduct list_cirasset)
          
          else if (resp=="5") then do let resul = print_lst_circutype current_data list_circularproduct_registered
                                      if (resul == "") then putStr("\n No Circular Product in List Registered!\n") else putStr(resul)

          else if (resp=="6") then do putStr("Total Circular Product in Recycle Category: ")
                                      print(count_circuType list_circularproduct_registered)

          else if (resp=="7") then do putStr "Search Product Name: \n"
                                      name <- getLine
                                      let resultado = compareAssetCharAllByName name list_cirasset list_circularproduct
                                      if (resultado == []) then print("Product Name Not Found") else print(max_list resultado)

          else if (resp=="8") then do putStr "Set Date (yyyy-mm-dd): \n"
                                      newdata <- getLine
                                      let newdate = parseDay newdata
                                      putStr "\nUpdated System DateTime: \n"
                                      writeFile "datesystem.txt" (showGregorian newdate)
                                      let newlist = print_lst_circutype_newDate_still newdate list_circularproduct_registered
                                      if (newlist == "") then putStr(" - No Product/Asset in this Category.\n") else putStr(newlist)
                                      
                                      putStr "\nUpdated Product/Asset Category List with New Date]:\n"
                                      let newlist = print_lst_circutype_newDate_out newdate list_circularproduct_registered
                                      if (newlist == "") then putStr(" - No Product/Asset within this Category.\n") else putStr(newlist)

          else if (resp=="9") then do putStr("\nProduct Circular Infographic {" ++ graph list_cirasset list_circularproduct ++"\n}\n")                                 

          else error "Selected Option Not Found \n"

          putStr "\nDo you wish to continue (y/n)? : \n"
          resp <- getLine
          if (resp=="y" || resp=="Y") then main else return()

