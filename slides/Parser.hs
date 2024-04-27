module Parser where
 
import UU.Parsing
import Scanner
import AbstractGrammar
 

pSlides = Slides <$> pList pSlide

pSlide = Slide <$> pTitleSlide <*> pBodySlide <* pEndSlide "***"

pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings
 
pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarckdownBlock <* pEndBlock "}"  
 
pMarckdownBlock = MdParagraph <$> pStrings
                <|> MdHeaderH1 <$ pKeyword "#" <*> pStrings
                <|> pImage
                <|> pTable
                <|> MdH2 <$ pKeyword "##" <*> pStrings
                <|> MdH3 <$ pKeyword "###" <*> pStrings
                <|> Style <$ pKeyword "+" <*> pStrings

pImage = ImageSlide <$ pKeyword "%" <*> pStrings

pColumn = Column <$> pStrings
pRow = Row <$ pOpenRow "<<" <*> pList pColumn <* pEndRow ">>"
pTable = TableSlide <$ pOpenTable "<" <*> pList pRow <* pEndTable ">" 


instance Symbol Token
 
getValue:: Token -> String
getValue (Token _ v _ _) = v
 
tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)
 
tStr = getValue <$> pSym (Token String "" 0 0)
 
pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword
 
pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock
 
pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock
 
pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide
 
pEndRow :: String -> Parser Token String
pEndRow = tSym EndRow
 
pOpenRow :: String -> Parser Token String
pOpenRow = tSym OpenRow
 
pEndTable :: String -> Parser Token String
pEndTable = tSym EndTable
 
pOpenTable :: String -> Parser Token String
pOpenTable = tSym OpenTable 

pStrings :: Parser Token String
pStrings = tStr