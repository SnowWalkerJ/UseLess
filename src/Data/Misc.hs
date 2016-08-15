module Data.Misc where
    right either = case either of
        Left _ -> Nothing
        Right r -> Just r