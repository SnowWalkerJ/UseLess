module Data.HTML where
    import Control.Monad
    import Data.Maybe (maybeToList)
    
    data Tag = 
        Tag {
            tagname::String,
            attrs::Attrs,
            children::[Tag]
        } 
        | Text {
            text::String
        }
        |Comment {
            text::String
        }
        deriving Eq
        
    instance Show Tag where
        show (Comment annotation) = "<!-- " ++ annotation ++ " -->\n"
        show (Text txt) = txt
        show tag = 
            let tagName = tagname tag
                attributions = attrs tag
                child = children tag
                attrs_txt = join $ Prelude.map (\x->" " ++ fst x ++ "=\"" ++snd x ++ "\"") attributions
            in if tagName `elem` contentLess 
                then "<" ++ tagName ++ attrs_txt ++ ">\n"
                else if null (children tag) then "<" ++ tagName ++ attrs_txt ++ "/>\n"
                    else "<" ++ tagName ++ attrs_txt ++ ">\n" ++ join (Prelude.map show child) ++ "\n</" ++ tagName ++ ">\n"
    
    type Attr = (String, String)
    type Attrs = [Attr]
    
    contentLess = ["br", "hr", "img", "input", "link", "meta", "area", "base", "col", "command", "embed", "keygen", "param", "source", "track", "wbr"]
    
    getTagName::Tag->Maybe String
    getTagName (Tag tgname _ _) = Just tgname
    getTagName _ = Nothing
    
    getAttr::String->Tag->Maybe String
    getAttr attrName tag = lookup attrName (attrs tag)
    
    getContent::Tag->Maybe String
    getContent tag = liftM join $ sequence $ do
        childtag <- children tag
        case childtag of
            Text t -> return $ Just t
            _ -> return Nothing
    
    (?>>)::[Tag]->(Tag->Maybe String)->[String]
    tags ?>> g = do
        thisTag <- tags
        maybeToList (g thisTag)

    (!)::Tag->String->Maybe String
    tag ! attrName = lookup attrName (attrs tag)
    
    (!!)::[Tag]->String->[String]
    tags !! attrName = do
        tag <- tags
        let attrValue = tag ! attrName
        maybeToList attrValue
            
    
    
