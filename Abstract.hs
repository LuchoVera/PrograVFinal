
module Abstract where


type Strings = String
type H1 = String
type H2 = String
type H3 = String
type Img = String
type Bold = String
type Italik = String
type List = String

data Slides = Slides [Slide]
    deriving Show 

data Background = Background Strings
    deriving Show

data Slide = Slide TitleSlide Background BodySlide
    deriving Show 

data TitleSlide = TitleSlide Strings
    deriving Show

data BodySlide = BodySlide [MarckdownBlock]
    deriving Show

data MarckdownBlock = MdParagraph Strings 
                    |MdH1 H1
                    |MdH2 H2
                    |MdH3 H3
                    |MdImg Img
                    |MdBold Bold
                    |MdItalik Italik
                    |MdList List
        deriving Show