module Conversor where

import UU.Parsing
import Scanner
import AbstractGrammar
import Parser

getGeneralStyle :: String
getGeneralStyle =
  "<style>\n" ++
  "    #slides {\n" ++
  "        position: relative;\n" ++
  "        width: 92%;\n" ++
  "        height: 98vh; \n" ++
  "        margin: auto;\n" ++
  "        overflow: hidden;\n" ++
  "    }\n" ++
  "    #slides section {\n" ++
  "        width: 100%;\n" ++
  "        height: 100%;\n" ++
  "        display: none;\n" ++
  "    }\n" ++
  "    #prevBtn, #nextBtn {\n" ++

  "        position: absolute;\n" ++
  "        top: 50%;\n" ++
  "        transform: translateY(-50%);\n" ++
  "        cursor: pointer;\n" ++
  "        background-color: #ffffff;\n" ++
  "        border: none;\n" ++
  "        padding: 10px;\n" ++
  "        z-index: 1;\n" ++
  "        font-size: 2rem;\n" ++
  "        font-weight: 550;\n" ++
  "    }\n" ++
  "    #prevBtn {\n" ++
  "        left: 0;\n" ++
  "    }\n" ++
  "    #nextBtn {\n" ++
  "        right: 0;\n" ++
  "    }\n" ++
  "</style>\n"

getBody :: Slides -> String
getBody (Slides slides) =
  "<div class=\"carousel\" style=\"display: flex; width: 100%; flex-direction: column; justify-content: center; align-items: center;\">\n" ++
  slidesToHTML (Slides slides) ++ "</div>\n" ++
  "    <button id=\"prevBtn\" onclick=\"prevSlide()\"><</button>\n" ++
  "    <button id=\"nextBtn\" onclick=\"nextSlide()\">></button>\n"

getScript :: String
getScript =
  "<script>\n" ++
  "  let slideIndex = 0;\n" ++
  "  const slides = document.querySelectorAll('#slides section');\n" ++
  "\n" ++
  "  function showSlide() {\n" ++
  "    slides.forEach(slide => {slide.style.display = 'none';});\n" ++
  "    slides[slideIndex].style.display = 'block';\n" ++
  "  }\n" ++
  "\n" ++
  "  function nextSlide() {\n" ++
  "    slideIndex++;\n" ++
  "    if (slideIndex >= slides.length) {\n" ++
  "      slideIndex = 0;\n" ++
  "    }\n" ++
  "    showSlide();\n" ++
  "  }\n" ++
  "\n" ++
  "  function prevSlide() {\n" ++
  "    slideIndex--;\n" ++
  "    if (slideIndex < 0) {\n" ++
  "      slideIndex = slides.length - 1;\n" ++
  "    }\n" ++
  "    showSlide();\n" ++
  "  }\n" ++
  "\n" ++
  "  showSlide();\n" ++
  "</script>\n"

buildCarousel :: Slides -> String
buildCarousel slides =
  getGeneralStyle ++
  getBody slides ++
  getScript


slidesToHTML :: Slides -> String
slidesToHTML (Slides slides) =
  "  <div id=\"slides\" text-align:center;\">\n" ++
  concatMap slideToHTML slides ++
  "  </div>\n"



slideToHTML :: Slide -> String
slideToHTML (Slide titleSlide bodySlide) =
  "   <section class=\"slide\">\n" ++
  titleSlideToHTML titleSlide ++
  bodySlideToHTML bodySlide ++
  "   </section>\n"


titleSlideToHTML :: TitleSlide -> String
titleSlideToHTML (TitleSlide title) = "     <h1>" ++ title ++ "</h1>\n"

bodySlideToHTML :: BodySlide -> String
bodySlideToHTML (BodySlide markdownBlocks) =
    if isStyleBlock markdownBlocks
        then "      <div style="++ "\""++ getStyle markdownBlocks ++ "\"" ++ ">\n" ++ concatMap markdownBlockToHTML markdownBlocks ++ "     </div>\n"
        else "      <div class=\"other-body\">\n" ++ concatMap markdownBlockToHTML markdownBlocks ++ "      </div>\n"
  where
    isStyleBlock :: [MarkdownBlock] -> Bool
    isStyleBlock [] = False
    isStyleBlock (Style _ : _) = True
    isStyleBlock _ = False


getStyle :: [MarkdownBlock] -> String
getStyle [] = ""
getStyle (Style text : _) = text
-- getStyle (_ : xs) = getStyle xs

imageToHtml :: String -> String
imageToHtml text = "       <img style=\"object-fit: contain;\" src=\"" ++ text ++ "\" alt=\"image\">\n"

tableToHtml :: [Row] -> String
tableToHtml cols = "       <table style=\"border-collapse: collapse; width: 100%;\">\n" ++ getRows cols ++ "        </table>\n"

getRows :: [Row] -> String
getRows [] = "";
getRows (x:rows) = "         <tr>\n"++ getRowCols x ++"\n          </tr>\n" ++ getRows rows

getRowCols :: Row -> String
getRowCols (Row cols) = concatMap columnToHtml cols

columnToHtml :: Column -> String
columnToHtml (Column content) = "            <td style=\"border: 1px solid #dddddd; text-align: left; padding: 5px;\">" ++ content ++ "</td>\n"

markdownBlockToHTML :: MarkdownBlock -> String
markdownBlockToHTML (Style text) = ""
markdownBlockToHTML (ImageSlide text) = imageToHtml text
markdownBlockToHTML (TableSlide rows) = tableToHtml rows
markdownBlockToHTML (MdParagraph text) = "        " ++ text ++ "\n"
markdownBlockToHTML (MdHeaderH1 text) = "       <h1>" ++ text ++ "</h1>\n"
markdownBlockToHTML (MdH2 text) = "       <h2>" ++ text ++ "</h2>\n"
markdownBlockToHTML (MdH3 text) = "       <h3>" ++ text ++ "</h3>\n"
