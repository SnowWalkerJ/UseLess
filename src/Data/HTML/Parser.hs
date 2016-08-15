module Data.HTML.Parser (parseHTML, fromURL) where
    import Data.Function
    import Data.Misc (right)
    import Control.Monad
    import qualified Control.Applicative ((<|>))
    import Data.HTML
    import Data.HTML.Selector
    import Data.Either
    import Network.HTTP
    import Network.URI
    import Text.ParserCombinators.Parsec

    blank::CharParser () String
    blank = many (oneOf "\t\n\r ")

    tagParser::CharParser () Tag
    tagParser = do
        blank
        char '<'
        blank
        tagName <- many (noneOf " /\\>")
        attrs <- try (char ' '>>many (try attrParser)) <|> return []
        char '>'
        blank
        children <- htmlParser tagName
        let children' = filter (\x->x/=(Text "")) children
        blank
        string ("</" ++ tagName ++ ">")
        blank
        return $ Tag tagName attrs children'
        
    htmlParser tagName = many $ choice [try commentParser, 
                                        try singleTagParser, 
                                        try tagParser, 
                                        contentParser,
                                        try (badClosing tagName)>>return (Text "")]
        
    badClosing::String->CharParser () ()
    badClosing thisTagName = do
        tagName <- between (string "</") (char '>') (many (noneOf ">"))
        if tagName /= thisTagName then
            return ()
        else
            fail ""
        
    singleTagParser::CharParser () Tag
    singleTagParser = do
        blank
        char '<'
        blank
        tagName <- many (noneOf " /\\>")
        attrs <- try (char ' '>>many (try attrParser)) <|> return []
        blank
        if (not $ tagName `elem` contentLess) 
        then oneOf "/\\"
        else (oneOf "/\\" <|> return ' ')
        blank
        char '>'
        blank
        return $ Tag tagName attrs []
    
    commentParser1::CharParser () Tag
    commentParser1 = do
        blank 
        annotation <- between (string "<!") (char '>') (many (noneOf ">"))
        blank
        return $ Comment annotation   
             
    commentParser2::CharParser () Tag
    commentParser2 = do
        annotation <- between blank (string "-->" >> blank) $ string "<!--" >> manyTill anyChar (lookAhead (try $ string "-->"))
        return $ Comment annotation
        
    commentParser = try commentParser2 <|> try commentParser1

    contentParser::CharParser () Tag    
    contentParser = do
        s <- many1 (noneOf "<>")
        return $ Text s
    
    attrParser::CharParser () Attr
    attrParser = do
        attrName <- many1 (noneOf " =\t\n>")
        blank
        let withBracket = between (oneOf "\"'") (oneOf "\"'") (many (noneOf ">\"'"))::CharParser () String
        let withoutBracket = many (noneOf ">\"' ")::CharParser () String
        attrVal <- (char '=' >> blank >> (try withBracket <|> try withoutBracket)) <|> return "true"
        blank
        return  (attrName, attrVal)

    parseHTML::String->Maybe Tag
    parseHTML html =
        let parsed = right $ parse (htmlParser "") "" html
        in  case parsed of
                Nothing -> Nothing
                Just lst -> case lst ||> createSelector "html" of
                    [] -> Nothing
                    h:xs -> Just h
    
    fromURL::String->IO (Maybe Tag)
    fromURL url = do
        htmlBody <- (do
            resp <- simpleHTTP (getRequest url)
            getResponseBody resp) Control.Applicative.<|> return ""
        return $ parseHTML htmlBody
    
