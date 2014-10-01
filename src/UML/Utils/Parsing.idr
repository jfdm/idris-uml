--- Parsing Utils
module UML.Utils.Parsing

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

manyTill : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
manyTill p end = scan
  where
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}
