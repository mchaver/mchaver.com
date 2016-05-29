{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative

import Data.Attoparsec.Text
import Data.List
import Data.Text (Text)


data QIFHeader = 
  BankAccountTransactions       |
  CashAccountTransactions       |
  CreditCardAccountTransactions |
  InvestmentAccountTransactions |
  AssetAccountTransactions      |
  LiabilityAccountTransactions  |
  AccountList                   |
  CategoryList                  |
  ClassList                     |
  MemorizedTransactionList      
  deriving (Eq,Read,Show)  

parseHeader :: Parser QIFHeader
parseHeader = do
  header <- (string "!Type:Bank"      *> pure BankAccountTransactions)
        <|> (string "!Type:Cash"      *> pure CashAccountTransactions)
        <|> (string "!Type:CCard"     *> pure CreditCardAccountTransactions)
        <|> (string "!Type:Invst"     *> pure InvestmentAccountTransactions)
        <|> (string "!Type:Oth A"     *> pure AssetAccountTransactions)
        <|> (string "!Type:Oth L"     *> pure LiabilityAccountTransactions)
        <|> (string "!Account"        *> pure AccountList)
        <|> (string "!Type:Cat"       *> pure CategoryList)
        <|> (string "!Type:Class"     *> pure ClassList)
        <|> (string "!Type:Memorized" *> pure MemorizedTransactionList)
  endOfLine <|> endOfInput
  return header

-- if BankAccountTransactions, CashAccountTransactions, CreditCardAccountTransactions

nonInvestmentNecessaryParsers :: [NonInvestmentItem]
nonInvestmentNecessaryParsers = [NITDate "", NITAmount 0, NITPayee ""]

nonInvestmentOptionalParsers :: [NonInvestmentItem]
nonInvestmentOptionalParsers = [NITClearedStatus NotCleared, NITCategory "", NITNum "", NITPayee "", NITMemo "", NITAddress "", NITReimbursableBusinessExpense]

parseSplit :: Parser Split
parseSplit = do
  splitAmount <- parseSplitAmount
  
  mSplitCategory <- (Just <$> parseSplitCategory) <|> pure Nothing
  mSplitMemo     <- (Just <$> parseSplitMemo    ) <|> pure Nothing
  
  return $ Split splitAmount mSplitCategory mSplitMemo

parseClearedStatus :: Parser ClearedStatus
parseClearedStatus = do
  _ <- char 'C' 
  status <- char 'C' *> pure Cleared
        <|> char 'c' *> pure Cleared
        <|> char '*' *> pure Cleared
        <|> char 'X' *> pure Reconciled
        <|> char 'R' *> pure Reconciled
        <|> pure NotCleared

  endOfLine
  return status

--  NITClearedStatus ClearedStatus | -- C: "C" not cleared, "C*" cleared, "Cc" cleared, "CX" reconciled, "CR" reconciled

parseSplitAmount :: Parser Double
parseSplitAmount = char '$' *> double <* endOfLine

parseSplitCategory :: Parser Text
parseSplitCategory = char 'S' *> (takeTill isEndOfLine) <* endOfLine 

parseSplitMemo :: Parser Text
parseSplitMemo = char 'E' *> (takeTill isEndOfLine) <* endOfLine

testParser = (char 'D' *> takeTill isEndOfLine <* endOfLine)

getNonInvestmentItemParser :: NonInvestmentItem -> Parser NonInvestmentItem
getNonInvestmentItemParser (NITDate _)          = NITDate <$>  (char 'D' *> takeTill isEndOfLine <* endOfLine)
getNonInvestmentItemParser (NITAmount _)        = NITAmount <$> (char 'T' *> double <* endOfLine)
getNonInvestmentItemParser (NITClearedStatus _) = NITClearedStatus <$> parseClearedStatus
getNonInvestmentItemParser (NITNum _)           = NITNum <$> (char 'N' *> takeTill isEndOfLine <* endOfLine)
getNonInvestmentItemParser (NITPayee _)         = NITPayee <$> (char 'P' *> takeTill isEndOfLine <* endOfLine)
getNonInvestmentItemParser (NITMemo _)          = NITMemo <$> (char 'M' *> takeTill isEndOfLine <* endOfLine)
getNonInvestmentItemParser (NITAddress _)       = NITAddress <$> (char 'A' *> takeTill isEndOfLine <* endOfLine)
getNonInvestmentItemParser (NITCategory _)      = NITCategory <$> (char 'L' *> takeTill isEndOfLine <* endOfLine)
getNonInvestmentItemParser (NITSplit _)         = NITSplit <$> parseSplit
getNonInvestmentItemParser (NITReimbursableBusinessExpense) = char 'F' *> endOfLine *> pure NITReimbursableBusinessExpense

getNITDate :: [NonInvestmentItem] -> Maybe Text
getNITDate (x:xs) =
  case x of
    (NITDate y) -> Just y
    _ -> getNITDate xs
getNITDate _ = Nothing

getNITAmount :: [NonInvestmentItem] -> Maybe Double
getNITAmount (x:xs) =
  case x of
    (NITAmount y) -> Just y
    _ -> getNITAmount xs
getNITAmount _ = Nothing


getNITClearedStatus :: [NonInvestmentItem] -> Maybe ClearedStatus
getNITClearedStatus (x:xs) =
  case x of
    (NITClearedStatus y) -> Just y
    _ -> getNITClearedStatus xs
getNITClearedStatus _ = Nothing

getNITNum :: [NonInvestmentItem] -> Maybe Text
getNITNum (x:xs) =
  case x of
    (NITNum y) -> Just y
    _ -> getNITNum xs
getNITNum _ = Nothing

getNITPayee :: [NonInvestmentItem] -> Maybe Text
getNITPayee (x:xs) =
  case x of
    (NITPayee y) -> Just y
    _ -> getNITPayee xs
getNITPayee _ = Nothing

getNITMemo :: [NonInvestmentItem] -> Maybe Text
getNITMemo (x:xs) =
  case x of
    (NITMemo y) -> Just y
    _ -> getNITMemo xs
getNITMemo _ = Nothing

getNITAddress :: [NonInvestmentItem] -> Maybe Text
getNITAddress (x:xs) =
  case x of
    (NITAddress y) -> Just y
    _ -> getNITAddress xs
getNITAddress _ = Nothing

getNITCategory :: [NonInvestmentItem] -> Maybe Text
getNITCategory (x:xs) =
  case x of
    (NITCategory y) -> Just y
    _ -> getNITCategory xs
getNITCategory _ = Nothing

getNITSplits :: [NonInvestmentItem] -> [Split]
getNITSplits (x:xs) =
  case x of
    (NITSplit y) -> [y] ++ getNITSplits xs
    _ -> getNITSplits xs
getNITSplits _ = []


parseNonInvestment :: Parser NonInvestment
parseNonInvestment = do
  nits <- parseNonInvestmentTransaction
  _ <- char '^'
  endOfLine <|> endOfInput

  let mNonInvestment = NonInvestment <$> (getNITDate nits) 
                                     <*> (getNITAmount nits)
                                     <*> pure (getNITClearedStatus nits)
                                     <*> pure (getNITNum nits)
                                     <*> (getNITPayee nits)
                                     <*> pure (getNITMemo nits)
                                     <*> pure (getNITAddress nits)
                                     <*> pure (getNITCategory nits)
                                     <*> pure (getNITSplits nits)

  case mNonInvestment of
    Nothing -> fail ""
    Just nonInvestment -> return nonInvestment


parseNonInvestmentTransaction :: Parser [NonInvestmentItem]
parseNonInvestmentTransaction = parseNonInvestmentTransaction' []

parseNonInvestmentTransaction' :: [NonInvestmentItem] -> Parser [NonInvestmentItem]
parseNonInvestmentTransaction' parsedItems = do
  -- don't remove parseSplit, there can be multiple ones
  let parsers = [NITSplit $ Split 0.0 Nothing Nothing] ++ (deleteItems parsedItems (nonInvestmentNecessaryParsers ++ nonInvestmentOptionalParsers))
  mResult <- (Just <$> choice (map getNonInvestmentItemParser parsers)) <|> pure Nothing
  case mResult of
    Nothing -> 
      -- make sure all the nonInvestmentNecessaryParsers have been parsed
      if and $ map (flip elem nonInvestmentNecessaryParsers) parsers
        then return parsedItems
        else fail "Did not parse necessary parser"
    Just result -> parseNonInvestmentTransaction' (parsedItems ++ [result]) <|> pure (parsedItems ++ [result])
{-
mustParse = [NITDate "", NITAmount 0, NITPayee ""]
optionalParse = []

if not have mustParse then fail
if parse not succeed, end
-- set
parseNonInvestmentTransaction :: [NonInvestmentItem] -> Parser NonInvestment
parseNonInvestmentTransaction = do
  try parse
-}

data NonInvestmentItem =
  NITDate Text | -- D: in various formats
  NITAmount Double | -- T: currency ammount, commas allowed
  NITClearedStatus ClearedStatus | -- C: "C" not cleared, "C*" cleared, "Cc" cleared, "CX" reconciled, "CR" reconciled
  NITNum Text | -- N: check number or reference number
  NITPayee Text | -- P: name of person or company whom payed
  NITMemo  Text | -- M: anything you want to write about the transaction
  NITAddress  Text  | -- A: up to five lines, sixth line for message on check
  NITCategory Text  | -- L: category, ":" has subcategory
  NITSplit    Split | 
  NITReimbursableBusinessExpense -- F 
  deriving (Eq,Read,Show)


data ClearedStatus = 
  NotCleared | 
  Cleared    | 
  Reconciled 
  deriving (Eq,Read,Show)

data NonInvestment = NonInvestment {
  _getDate     :: Text
, _getAmount   :: Double
, _getStatus   :: Maybe ClearedStatus 
, _getNum      :: Maybe Text
, _getPayee    :: Text
, _getMemo     :: Maybe Text
, _getAddress  :: Maybe Text
, _getCategory :: Maybe Text
, _getSplits   :: [Split] -- (Amount of Split, Category, Memo)
} deriving (Eq,Read,Show)

-- must parse date, amount, payee, rest is optional

data Split = Split {
  _getSplitAmount   :: Double
, _getSplitCategory :: Maybe Text
, _getSplitMemo     :: Maybe Text
} deriving (Eq,Read,Show)

data Investment = Investment {
  _getIDate             :: Text -- | D: date
, _getIAction           :: Text -- | N: action
, _getISecurity         :: Text -- | Y: security
, _getIPrice            :: Double -- | I: price
, _getIQuantity         :: Int    -- | Q: quantity
, _getIClearedStatus    :: ClearedStatus -- | C: cleared status
, _getReminder          :: Text -- | P: text in the first line for transfers and reminders
, _getIMemo             :: Text -- | M: memo
, _getICommission       :: Text -- | O: Commission
, _getITransferAccount  :: Text -- | L: account for the transfer
, _getITransferedAmount :: Double -- | $: amount transferred 
} deriving (Eq,Read,Show)




deleteItems :: (Eq a) => [a] -> [a] -> [a]
deleteItems (x:xs) ys = deleteItems xs $ delete x ys 
deleteItems _ ys = nub ys


-- some are required, some are optional

-- extensible version
data QIFHeaderExtensible a =
  H2 |
  CustomHeaders a 
  deriving (Eq,Read,Show)

data NoCustomHeaders = NoCustomHeaders deriving (Eq,Read,Show)

parseHeaderExtensible :: Maybe (Parser a) -> Parser (QIFHeaderExtensible a)
parseHeaderExtensible mCustomParser = do
  (string "something" *> pure H2)
  <|> case mCustomParser of
        Nothing -> fail "parse failed"
        Just customParser -> CustomHeaders <$> customParser

data CustomTest = CustomTest deriving (Eq,Read,Show)

parseCustomTest :: Parser CustomTest
parseCustomTest = string "custom" *> pure CustomTest

stuff :: Parser (QIFHeaderExtensible CustomTest)
stuff = parseHeaderExtensible (Just parseCustomTest)
{-

selectDataParser

headerParser
itemParser

Maybe structure


data QIFHeader a =
  ... |
  CustomHeaders a

data NoCustomHeaders -- shouldn't be parsed


Maybe (Parser a) -> parseHeader

data NonInvestmentItem =
  Date
  Amount
  ClearedStatus
  Num
  Payee
  Memo
  Address
  Category
  CategoryInSplit
  MemoInSplit
  DollarAmountOfSplit

data InvestmentItem =
  Date
  Action
  Security
  Price
  Quantity
  TransactionAmount
  ClearedStatus
  FirstLine
  Memo
  Commission
  TransferAcount
  AmountTransferred

data AccountItem =
  Name
  Type
  Description
  CreditLimit
  StatementBalanceDate
  StatementBalanceAmount
  
data CategoryListItem =
  CategoryName
  Description
  Tax
  Income
  Expense
  Budget
  Tax

data ClassListItem =
  ClassName        Text |
  ClassDescription Text

data MemorizedTransactionListItem =
  CheckTransaction
  DepositTransaction
  PaymentTransaction
  InvestmentTransaction
  ElectronicPayyTransaction
  Amount
  ClearedStatus
  Payee
  Memo
  Address
  Category
  ClsasInSplit
  MemoInspliat
  sollarAmmountOFsplit
  FirstPaymentDate
  TotalYearsLoan
  NumberOfPaymentsAlreadyMade
  NumberOfPeriodsPerYear
  InterestRate
  CurrentLoanBalance
  OriginalLoanAmount

parseDetailItem


qif internal quicken information
 !Account
NAccount Name
TAccount Type
D"Account Description"  Account list or which account follows

 !Type:Cat
N"Category Name"
D"Category description"   Category list

 !Type:Class  Class list

 !Type:Memorized  Memorized transaction list

follow by \n^ then Header\n
-}

parseEndOfRecord :: Parser ()
parseEndOfRecord = do
  _ <- char '^'
  endOfLine <|> endOfInput
  return ()

-- Detail Items
data DetailItem = 
  Date                         | -- All: D
  AmountOfItem                 | -- All: T
  Memo                         | -- All: M any text about the item
  ClearedStatus                | -- All: C Cleared status. C,C*,Cc,CX,CR Values are blank (not cleared), "*" or "c" (cleared) and "X" or "R" (reconciled).
  NumberOfTheCheck             | -- Banking, Splits: N Number of the check. Can also be "Deposit", "Transfer", "Print", "ATM", "EFT".
  Payee                        | -- Banking, Investment: P Payee. Or a description for depositis, transfers, et.c
  AddressOfPayee               | -- Banking, Splits: A Address of Payee. Up to 5 address lines are allowed. A 6th address line is a message that prints on the check. 1st line is normally the same as the payee line--the name of the payee.
  CategoryTransferClass        |  
  ReimbursableBusinessExpense  |
  SplitCategory                |
  SplitMemo                    |
  SplitItemAmount              |
  Percent                      |
  InvestmentAction             |
  SecurityName                 |
  Price                        |
  QuantityOfShare              |  
  ComissionCost                |
  AmountTransferred            |
  ExtendedDataQB               |
  ShiptToAddress               |
  InvoiceTransactionType       | 
  InvoiceDueDate               |
  TaxAccount                   |
  TaxRate                      |
  TaxAmount                    |
  LineItemDescription          |  
  LineItemCategory             | 
  LineItePricerPerUnit         |
  LineItemTaxableFlag
{-
P Payee
A Address of Payee
L Category or Transfer and Class
F reimbursable business expense
S split category
E split memo
$ amount for thi ssplite of the item. same as T
% percent
N investnment action
Y sercurity name
I prive
Q quantity of share
O comission cost
$ amount transferred
X extended data for quicken business
XA ship-to address
XI invoice transaction type: 1 for invoice, 3 for payment XI1, invoice, XI3 payment
XE invoice due data
XC tax account
XR tax rate
XT tax amount
XS line item description
XN line item category name
X# line item quantity
X$ line item price per unit (multiply by X# for line item amount)
XF line item taxable flag

The S, E, $, and % fields are repeated for each split of this transaction.
    Investment Actions track common investment activities such as Dividends, Reinvestment of income, and Capital Gains (CG). Codes that end in X indicate that the transaction was generated by the account, but the cash is transferred to a different account (in which case the Category field is the destination account name). Action codes include: CGLong (Capital Gains Long Term), CGLongX, CGMid, CGMidX, CGShort, CGShortX, Div, DivX, IntInc, IntIncX, MargInt, MargIntX, RtnCap, RtnCapX, XIn, XOut, Added, Removed, StkSplit
    If the line immediately following an XS record does not begin with ^ or X, that is considered a continuation of the XS record.

All the fields in detail items are optional—if not included, that field will be left blank in the imported transaction. Also, Quicken seems to do little error checking on these items; if the same field is included twice in the detail item, the second one will just overwrite the first one.
-}


{-
extenable parser
accept a parser, use it to parse things
detail item parser
-}