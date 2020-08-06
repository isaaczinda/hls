{-|
Module       : ParserBase
Description  : Primitive parsers and parser combinators.
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)

NOTE: You do not need to understand the code in this file. It uses some features of
Haskell that we have not yet learned.
-}


module ParserBase (Parser,pfail,get,parse,
                   (<|>), some,many,Alternative, MonadPlus, empty,
                   join, mfilter, (<=>), (<+>), (<++>), (<:>), (>>=:), (<+->),
                   (<-+>), (<??>), (<???>), inv, chainl1, optional, (-->),
                   ParseString, parseForError) where

-- Also, is instances of Functor, Applicative, Monad and MonadPlus

import Control.Applicative hiding (optional)
import Control.Monad

-- Our parser carries both the status of the most recent parse attempted
-- and the "deepest error" found so far.  That way, when the whole parse
-- fails, we can give better error messages.

type ParsePosn = (Int, Int) -- Line, Column
type ParseInput = (String, ParsePosn)
type ParseError = (String, ParsePosn)

-- (start, end)
type ParseString = (ParsePosn, ParsePosn)

-- | Has the capacity to invoke a parsing function on an input string, and look for
--   a result of type 'a'.
newtype Parser a = ParsingFunction (ParseError -> ParseInput -> ParseResult a)
data ParseStatus a = Success a ParseInput
                   | Failure ParseError
                        deriving Show
type ParseResult a = (ParseError, ParseStatus a)

-- keeps whichever error is older
worseErr :: ParseError -> ParseError -> ParseError
worseErr e1@(str1, pos1) e2@(str2, pos2) =
    if pos1 > pos2 then e1
    else e2

-- Whenever there is a failure, make sure to pass along the best error,
-- whether is comes from this parse or an older try.
failure prevErr@(prevMsg,prevPosn) msg posn = (bestErr, Failure newErr)
    where newErr                    = (msg,posn)
          bestErr | prevPosn > posn = prevErr
                  | otherwise       = newErr

-- | A parser that always fails.
pfail :: Parser a
pfail = fail "No parse (via pfail)"
-- pfail = ParsingFunction (\err -> \(_,posn) -> (err, Failure ("No parse.",posn)))

-- | Get a single character from the input. Fails if the input is empty.
get :: Parser Char
get = ParsingFunction readChar
    where readChar e ('\n':t, (line,_))   = (e, Success '\n' (t,(line+1,1)))
          readChar e (h:t,    (line,col)) = (e, Success h    (t,(line,col+1)))
          readChar e (_,      posn)       = failure e "Unexpected EOF" posn

-- works just like parse function, but returns only error
-- TODO: refactor to depend on parse, so no doubled up code
parseForError :: Parser a -> String -> Maybe ParseError
parseForError (ParsingFunction f) inputString =
    case f ("No (known) error", (0,0)) (inputString,(1,1)) of
        (_,   Success result ("", _)) ->
            Nothing
        (bErr@(bMsg,bPosn), Success result (_,  posn))  ->
          if bPosn >= posn then Just bErr
                           else Just ("EOF expected", posn)
        (err, _) -> Just err

parse :: Parser a -> String -> a
parse (ParsingFunction f) inputString =
    case f ("No (known) error", (0,0)) (inputString,(1,1)) of
        (_,   Success result ("", _)) ->
            result
        (bErr@(bMsg,bPosn), Success result (_,  posn))  ->
            makeError $ if bPosn >= posn then bErr
                                         else ("EOF expected", posn)
        (err, _) ->
            makeError err
    where
        makeError (msg, (line,col)) =
            error (show line ++ ":" ++ show col ++ " -- " ++ msg)



-- Combine two parsers using an 'or' type operation -- this is the
-- code used for mplus and <|>
orElseWithMergedErr :: Parser a -> Parser a -> Parser a
orElseWithMergedErr (ParsingFunction f) (ParsingFunction g) =
   ParsingFunction f_or_g
   where f_or_g err1 state =
             case f err1 state of
                 (err2, Failure ffail@(why_f,pos_f)) ->
                     case g err2 state of
                         (err3, Failure gfail@(why_g,pos_g)) ->
                             -- Keep whatever is the "worst error" up front.
                             if pos_f > pos_g then (worseErr err3 ffail, Failure ffail) -- worseErr err3 ffail
                                              else (worseErr err3 gfail, Failure gfail)
                         result -> result
                 success -> success

-- The core functions

instance Monad Parser where
    -- | A parser that always succeeds and returns 'x'
    return x  = ParsingFunction (\err -> \state -> (err, Success x state))
    -- | A parser that always fails, with the given message
    fail msg  = ParsingFunction (\err -> \(_,posn) -> failure err msg posn)
    -- | The "bind" (or "and-then") operator
    ParsingFunction f >>= makeG = ParsingFunction f_then_g
        where f_then_g err1 str =
                  case f err1 str of
                      (err2, Success x state') ->
                          let ParsingFunction g = makeG x
                          in  g err2 state'
                      (err2, Failure whypos) -> (err2, Failure whypos)


(-->) :: Parser a -> (a -> ParseString -> Parser b) -> Parser b
ParsingFunction f --> makeG = ParsingFunction f_then_g
    where f_then_g err1 str@(input, start_pos) =
            case f err1 str of
                (err2, Success x state'@(_, (end_col, end_row))) ->
                    -- in addition to passing parsed object to "then"
                    -- function, pass parse range as well
                    -- end_pos - 1 because this points to the NEXT unread
                    -- character, not the last character read
                    let ParsingFunction g = makeG x (start_pos, (end_col, end_row - 1))
                    in  g err2 state'
                (err2, Failure whypos) -> (err2, Failure whypos)


-- Derive other monads using existing/derived functions

instance Functor Parser where
    fmap = liftM

instance MonadPlus Parser where
    mzero = pfail
    mplus = orElseWithMergedErr

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Alternative Parser where
    empty = mzero
    -- | Alternatives: succeeds if either parser does
    (<|>) = mplus


-- custom shit
(<=>) :: Parser a -> (a -> Bool) -> Parser a
p <=> isOkay =
    p >>= \ret_val ->
        if (isOkay ret_val) then (return ret_val) else pfail

-- And operator
(<+>) :: Parser a -> Parser b -> Parser (a, b)
p <+> q =
    p >>= (\r1 -> (q >>= \r2 -> return (r1, r2)))

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
p <++> q =
    (p <+> q) >>= \(a,b) -> (return (a ++ b))

-- Cons operator
(<:>) :: Parser a -> Parser [a] -> Parser [a]
p <:> q =
    (p <+> q) >>= \(a,b) -> (return (a:b))

(>>=:) :: Parser a -> (a -> b) -> Parser b
p >>=: f =
    p >>= \x -> (return (f x))

(<+->) :: Parser a -> Parser b -> Parser a
p <+-> q =
    (p <+> q) >>= \(a,_) -> (return a)

(<-+>) :: Parser a -> Parser b -> Parser b
p <-+> q =
    (p <+> q) >>= \(_,b) -> (return b)

{-
parser which doesn't consume any input and inverts the fail / success of a
parser
if p fails, then inv p will in fact succeed
if p succeeds, then inv p will in fact fail
-}
inv :: Parser a -> Parser ()
inv p =
        parserFailed >>= \b ->
            case b of
                True  -> return ()
                False -> pfail

    where
        -- a parser which never fails
        parserFailed :: Parser Bool
        parserFailed = (p >>= (\_ -> (return False))) <|> (return True)


-- If the parser fails, return an error which points to the first character we couldn't parse
-- and overrides the default error message with a custom one.
-- Override any errors produced by child parsers -- even if they go deeper
infixl 3 <??>
(<??>) :: Parser a -> String -> Parser a
(<??>) (ParsingFunction f) message =
        ParsingFunction f_error
    where f_error err1 state@(_, start_pos) =
            case f err1 state of
                (err2, Failure (why, end_pos)) ->
                    -- Compares error just produced with modified message
                    -- and worst error so far to find if worst error should
                    -- be updated.
                    let
                        thiserr = (message, start_pos)

                        -- ignore any errors produced by the parsers that
                        -- make up f
                        worsterr = worseErr thiserr err1

                    in (worsterr, Failure thiserr)
                success -> success



-- If the parser wasn't able to consume any of the input, override with this
-- error message. Otherwise, pass along the parser's error message.
infixl 3 <???>
(<???>) :: Parser a -> String -> Parser a
ParsingFunction f <???> message = ParsingFunction f_error
    where f_error err1 str@(input, start_pos) =
            case f err1 str of
                -- If the parser succeeded, just pass along the output
                out@(err2, Success x state'@(_, pos_f)) -> out

                -- If there was a failure but the parser made progress,
                -- use the parser's own message. If no progress was made,
                -- use the substituted message.
                out@(err2, Failure (why, end_pos)) ->
                    if end_pos > start_pos then out
                    else (worseErr err2 (message, start_pos), Failure (message, start_pos))

-- | Adapts 'foldl' to work on parse results
{-
takes
    1) a parser that outputs a
    2) a parser that outputs a "combiner" function, which takes a, a and outputs a
returns: a parser for the final, combined a
-}
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op =
    p <+> many (op <+> p)
    >>=: \(head,oprs) -> foldl (\a (f,rest) -> f a rest) head oprs

optional :: Parser a -> Parser (Maybe a)
optional p = (p >>=: \x -> Just x) <|> (return Nothing)
