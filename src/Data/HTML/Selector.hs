module Data.HTML.Selector (createSelector, 
                           select, 
                           applySelector,
                           findByTag,
                           findById,
                           findByClass,
                           (|>>),
                           (||>)) where
    import Data.List.Split
    import Data.List hiding ((!!))
    import Data.HTML
    import Data.Misc (right)
    import Data.Maybe (fromMaybe, fromJust)
    import Control.Applicative ((<|>))
    import Text.ParserCombinators.Parsec hiding ((<|>))
    

    data Selector = Selector {
        tagS::[String],
        clsS::[String],
        idS::[String]
    } deriving Show
    
    createSelector::String->Selector
    createSelector pattern = 
        let (tags, clss, ids) = fromJust $ right $ parse selectParser "" pattern
        in  Selector tags clss ids
    
    applySelector::Selector->Tag->[Tag]
    applySelector selector tag = applySelector' selector [tag]
    
    select::String->Tag->[Tag]
    select pattern root = applySelector (createSelector pattern) root 
        
    findByTag::String->Tag->[Tag]
    findByTag tagName tag = 
        if null $ "#. " `intersect` tagName
        then select tagName tag
        else []
        
    findById::String->Tag->[Tag]
    findById idFullName@('.':idName) tag = 
        if null $ "#. " `intersect` idName
        then select idFullName tag
        else []
    findById _ _ = []
    
    findByClass::String->Tag->[Tag]
    findByClass classFullName@('#':className) tag = 
        if null $ "#. " `intersect` className
        then select classFullName tag
        else []
    findByClass _ _ = []
    
    (|>>) = flip applySelector'
    
    (||>) [] _ = []
    (||>) tags@(h:xs) selector = do
        tag <- tags
        case tag of
            (Tag _ _ _) -> if matchSelector selector tag
                then
                    return tag
                else 
                    []
            _ -> []


    ------ hidden ------
    blank = many $ char ' '::CharParser () String
    selectParser::CharParser () ([String], [String], [String])
    selectParser = do
        blank
        c <- anyChar
        if c `elem` "#."
        then do
            name <- many (noneOf ".# ")
            (tags, clss, ids) <- selectParser <|> return ([], [], [])
            if c == '#' then return (tags, name:clss, ids)
            else return (tags, clss, name:ids)
        else do
            rest <- many (noneOf ".# ")
            let name = c:rest
            (tags, clss, ids) <- selectParser <|> return ([], [], [])
            return (name:tags, clss, ids)

    matchSelector::Selector->Tag->Bool
    matchSelector _ (Text _) = False
    matchSelector _ (Comment _) = False
    matchSelector selector (Tag tagName attrList _) = tagMatch && classMatch && idMatch
        where tagMatch = tagName `elem` tagS selector
              classMatch = null classes || null (clsS selector) || any (\x->x `elem` clsS selector) classes
              idMatch = null ids || null (idS selector) || not (null (intersect (idS selector) ids))
              classes = (splitOn " " $ fromMaybe "" $ (lookup "class" attrList)) <|> return []
              ids = (splitOn " " $ fromMaybe "" $ (lookup "id" attrList)) <|> return []
              
              
    applySelector' _ [] = []
    applySelector' selector tags@(h:xs) = do
        tag <- tags
        case tag of
            (Tag _ _ _) -> if matchSelector selector tag 
                then
                    tag:applySelector' selector (children tag)
                else 
                    applySelector' selector (children tag)
            _ -> []

    
    

