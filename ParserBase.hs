{-|
Module       : ParserBase
Description  : Primitive parsers and parser combinators.
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)

NOTE: You do not need to understand the code in this file. It uses some features of
Haskell that we have not yet learned.
-}


module ParserBase (Parser,pfail,get,parse,parseFile,parseNamed,
                   succeeding,eof,(<|>), some,many,Alternative, MonadPlus, empty,
                   join, mfilter, (<=>), (<+>), (<++>), (<:>), (>>=:), (<+->),
                   (<-+>), (<??>), (<???>), chainl1, optional, (-->),
                   (-->:), ParseString) where

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
    where readChar e ('\n':t, (line,_))   = (e, Success '\n' (t,(line+1,0)))
          readChar e (h:t,    (line,col)) = (e, Success h    (t,(line,col+1)))
          readChar e (_,      posn)       = failure e "Unexpected EOF" posn

eof :: Parser ()
eof = ParsingFunction tryRead
    where tryRead e state@("", _)    = (e, Success () state)
          tryRead e       (_,  posn) = failure e "EOF expected" posn

-- | Given a 'Parser a' and an input string, return a value of type 'a' if the parser
--   matches the entire input string.
parse :: Parser a -> String -> a
parse p = parseNamed p "<input>"

-- | Given a 'Parser a' and the path to a file, return a value of type 'IO a' if the parser
--   matches the entire contents of the file.
parseFile :: Parser a -> String -> IO a
parseFile parser fileName = readFile fileName
                            >>= return . parseNamed parser fileName

parseNamed :: Parser a -> String -> String -> a
parseNamed (ParsingFunction f) fileName inputString =
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
            error (fileName ++ ":" ++ show line ++ ":" ++ show col ++ " -- "
                   ++ msg)

-- Combine two parsers using an 'or' type operation -- this is the
-- code used for mplus and <|>
orElseWithMergedErr :: Parser a -> Parser a -> Parser a
orElseWithMergedErr (ParsingFunction f) (ParsingFunction g) =
   ParsingFunction f_or_g
   where f_or_g err1 state =
             case f err1 state of
                 (err2@(why_ff,pos_ff), Failure ffail@(why_f,pos_f)) ->
                     case g err2 state of
                         (err3@(why_gg,pos_gg), Failure gfail@(why_g,pos_g)) ->
                             -- (( (show pos_g) ++ " " ++ (show  pos_f) ++ " " ++ (show pos_gg) ++ " " ++ (show pos_ff), pos_g), Failure (why_g, pos_g))

                             -- if the positions are the same, show both errors !!
                             if pos_f == pos_g then
                                 let e = (why_f ++ " or " ++ why_g, pos_g) in (e, Failure e)

                             -- if the positions are different, show whichever
                             -- got farther
                             else
                                 if pos_f > pos_g then ((why_f, pos_f), Failure (why_f, pos_f))
                                                  else ((why_g, pos_g), Failure (why_g, pos_g))
                         result -> result
                 success -> success

-- succeeding x p tries to parse p, and if it succeeds, all is good, otherwise
-- it will return x.  It's almost identical to
--    succeeding x p = p <|> return x
-- except that the *guaranteed* success is useful if you're trying to parse
-- lazily.

succeeding :: a -> Parser a -> Parser a
succeeding fallback (ParsingFunction f) = ParsingFunction f'
   where f' err1 state = (err2, Success result state'')
             where ~(err2, result,state'') =   -- <-- vital lazy match!!!
                       case f err1 state of
                           (err2, Success x state') -> (err2, x, state')
                           (err2, Failure _)        -> (err2, fallback, state)

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


 {-
 in this case,
 makeG value position = [takes parse value ]
 -}

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
--
(-->:) :: Parser a -> (a -> b) -> (Parser (b, ParseString))
f -->: r = f --> \x s -> (return ((r x), s))

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


-- If the parser fails, return an error which points to the start of the parse
-- and overrides the default error message with a custom one.
infixl 3 <??>
(<??>) :: Parser a -> String -> Parser a
(<??>) (ParsingFunction f) message =
        ParsingFunction f_error
    where f_error err1 state@(_, start_pos) =
            case f err1 state of
                  (_, Failure (why, pos)) -> ((message, start_pos), Failure (message, start_pos))
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
                    else ((message, start_pos), Failure (message, start_pos))

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
