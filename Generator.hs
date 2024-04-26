module Generator where

import Abstract

markdownBlockToHTML :: MarckdownBlock -> String
markdownBlockToHTML (MdParagraph text) = "<p style='font-size: 1.2vw'>" ++ text ++ "</p>\n"
markdownBlockToHTML (MdH1 text)        = "<h1>" ++ text ++ "</h1>\n"
markdownBlockToHTML (MdH2 text)        = "<h2>" ++ text ++ "</h2>\n"
markdownBlockToHTML (MdH3 text)        = "<h3>" ++ text ++ "</h3>\n"
markdownBlockToHTML (MdImg src)        = "<img src='" ++ src ++ "' width='50%' height='50%'>\n"
markdownBlockToHTML (MdBold text)      = "<b style='font-size: 1.2vw'>" ++ text ++ "</b>\n"
markdownBlockToHTML (MdItalik text)    = "<i style='font-size: 1.2vw'>" ++ text ++ "</i>\n"
markdownBlockToHTML (MdList text)      = "<ul><li style='font-size: 1.2vw'>" ++ text ++ "</li></ul>\n"


bodySlideToHTML :: BodySlide -> String
bodySlideToHTML (BodySlide blocks) = concatMap markdownBlockToHTML blocks 

getBackgroundURL :: Background -> String
getBackgroundURL (Background url) = url

slideToHTML :: Slide -> String
slideToHTML (Slide titleSlide background bodySlide) =
    "<div class='slide' style='background-image: url(" ++ getBackgroundURL background ++ ");'>\n" ++
    titleSlideToHTML titleSlide ++
    bodySlideToHTML bodySlide ++
    "</div>\n"

titleSlideToHTML :: TitleSlide -> String
titleSlideToHTML (TitleSlide title) = "<div style='font-size: 2.2vw; font-weight: bold;'>" ++ title ++ "</div>\n"

slidesToHTML :: Slides -> String
slidesToHTML (Slides slides) =
    "<html>\n\
    \    <head>\n\
    \        <meta name='viewport' content='width=device-width, initial-scale=1'>\n\
    \        <link rel='stylesheet' type='text/css' href='styles.css'>\n\
    \        <title>Slides</title>\n\
    \    </head>\n\
    \    <body>\n\
    \        <div class='slides-container'>\n" ++
    concatMap slideToHTML slides ++
    "\n\
    \        </div>\n\
    \        <div class='arrow arrow--left active' id='left-arrow' onclick='prevSlide()'></div>\n\
    \        <div class='arrow arrow--right' id='right-arrow' onclick='nextSlide()'></div>\n\
    \        <script src='script.js'></script>\n\
    \    </body>\n\
    \</html>"


slidesToHTMLFile :: Slides -> IO ()
slidesToHTMLFile (Slides slides) = writeFile "FIN.html" (slidesToHTML (Slides slides))
