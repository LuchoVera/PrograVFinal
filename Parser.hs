module Parser where
 
import UU.Parsing
import Scanner
import Abstract
 
pSlides = Slides <$> pList pSlide
 
pSlide = Slide <$> pTitleSlide <*> pBackground <*> pBodySlide <* pEndSlide "---"
pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings

pBackground = Background <$ pKeyword "^" <*> pStrings
 
pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarckdownBlock <* pEndBlock "}"
 
pMarckdownBlock = MdParagraph <$> pStrings
                <|> MdH1 <$ pH1 "#" <*> pStrings
                <|> MdH2 <$ pH2 "##" <*> pStrings
                <|> MdH3 <$ pH3 "###" <*> pStrings
                <|> MdBold <$ pBold "**" <*> pStrings
                <|> MdItalik <$ pItalik "*" <*> pStrings
                <|> MdImg <$ pImg "~" <*> pStrings
                <|> MdList <$ pElemList "$" <*> pStrings
                

                
 
--- Join Scanner with Parser
instance Symbol Token
 
getValue:: Token -> String
getValue (Token _ v _ _) = v
 
tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)
 
tStr = getValue <$> pSym (Token String "" 0 0)
 
pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword
 
pEndLine :: String -> Parser Token String
pEndLine = tSym EndLine


pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock

pElemList :: String -> Parser Token String
pElemList = tSym ElemList


pImg :: String -> Parser Token String
pImg = tSym Img

pBold :: String -> Parser Token String
pBold = tSym Bold 
 
pItalik :: String -> Parser Token String
pItalik = tSym Italik

pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock
 
pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide

pH1 :: String -> Parser Token String
pH1 = tSym H1

pH2 :: String -> Parser Token String
pH2 = tSym H2

pH3 :: String -> Parser Token String
pH3 = tSym H3



pStrings :: Parser Token String
pStrings = tStr