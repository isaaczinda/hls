module Frame (Data.Map.empty, module Frame) where

import Data.Map

data Frame a =
        Local (Map String a) (Frame a) |
        Global (Map String a)
    deriving (Show, Eq)

emptyFrame :: Frame a
emptyFrame = Global empty

-- creates a new variable in the outermost frame
newVar :: Frame a -> String -> a -> Maybe (Frame a)
newVar frame name value =
    case frame of
        (Local m rest) ->
            if (member name m)
                then Nothing
                else Just (Local (insert name value m) rest)
        (Global m)     ->
            if (member name m)
                then Nothing
                else Just (Global (insert name value m))

changeVar :: Frame a -> String -> a -> Maybe (Frame a)
changeVar (Local m frame') name value =
    case member name m of
        -- if the value is already present in this frame, update it
        True   -> Just (Local (insert name value m) frame')
        -- if the value is not present in this frame, try the next one
        False  ->
            do
                lowerFrames <- changeVar frame' name value
                return (Local m lowerFrames)

changeVar (Global m) name value =
    case member name m of
        True  -> Just (Global (insert name value m))
        False -> Nothing

getVar :: Frame a -> String -> Maybe a
getVar frame name =
    case frame of
        (Local m frame') ->
            case Data.Map.lookup name m of
                Just val -> Just val
                Nothing  -> getVar frame' name
        (Global m)       -> Data.Map.lookup name m
