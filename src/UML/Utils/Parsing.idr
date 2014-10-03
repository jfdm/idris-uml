--- Parsing Utils
module UML.Utils.Parsing

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

||| An identifier parser.
|||
||| Identifiers must satisfy the form:
|||
|||    `[[:alpha:]\|[:digit:]]+`
|||
||| TODO: Allow select punctuation to be included.
||| TODO: Leading character must be `[:alpha:]`
ident : Parser String
ident = map pack (some $ satisfy isAlphaNum)

||| Implementation of a `manyTill` parser combinator.
|||
||| Inspired by parsec's `manyTill` combinator.
manyTill : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
manyTill p end = scan
  where
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}

||| A literally between parser combinator.
|||
||| Will parse anything between a pair of given character.
|||
||| @c The deliminator.
literallyBetween : (c : Char) -> Parser String
literallyBetween c = map pack $ between (char c) (char c) (some (satisfy (/= c)))

||| End of Line Parser
eol : Parser ()
eol = char '\n'

-- --------------------------------------------------------------------- [ EOF ]
