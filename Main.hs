module Main where
    import Control.Monad (when)
    import Control.Monad.Trans (lift)
    import Data.Maybe(catMaybes, fromMaybe)
    import Network.URI
    import Data.HTML
    import Data.HTML.Parser (fromURL)
    import Data.HTML.Selector (findByTag)
    import Data.Crawler
    import Data.OCTable
    
    startURL = "http://support.hfm.io/1.2/api/"
    
    handler::OCTableT String IO String
    handler = do
        url <- popTaskT
        html <- lift $ fromURL url
        trueHtml <- return $ fromMaybe (Text "") html
        title <- return $ findByTag "title" trueHtml ?>> getContent
        newLinks <- return $ findByTag "a" trueHtml ?>> getAttr "href"
        s <- return $ map (takeWhile (\x->x/='#')) $ catMaybes $ map (resolvePath url) newLinks
        sequence_ $ fmap pushTaskT s
        return $ case title of
            [] -> ""
            h:_ -> h
            
    resolvePath::String->String->Maybe String
    resolvePath currentPath targetPath = 
        let currentURI = parseURI currentPath
            targetURI = if isAbsoluteURI targetPath then
                    parseURI targetPath
                else
                    parseRelativeReference targetPath
         in do
             c <- currentURI
             t <- targetURI
             return $ show $ nonStrictRelativeTo t c
             
    datahandler = \x -> when (x /= "") (print x)

    crawler = Crawler startURL handler datahandler

    main = runCrawler crawler
           