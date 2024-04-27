module AbstractGrammar where

type Strings = String

data Slides = Slides [Slide]
    deriving Show

data Slide = Slide TitleSlide BodySlide
    deriving Show

data TitleSlide = TitleSlide Strings 
    deriving Show


data BodySlide = BodySlide [MarkdownBlock]
    deriving Show



data MarkdownBlock = MdParagraph Strings
                    | ImageSlide Strings
                    | TableSlide [Row]
                    | MdHeaderH1 Strings
                    | MdH2 Strings
                    | MdH3 Strings
                    | Style Strings
    deriving Show

data Row = Row [Column]
    deriving Show

data Column = Column Strings
    deriving Show