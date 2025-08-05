import HaskellSpec.Lexer.Haskell.Combined

/-
Reserved Symbols
-/

#guard lex_haskell ".." == [Token.DotDot]
#guard lex_haskell ":"  == [Token.Colon]
#guard lex_haskell "::" == [Token.DoubleColon]
#guard lex_haskell "="  == [Token.Equal]
#guard lex_haskell "\\" == [Token.Backslash]
#guard lex_haskell "|"  == [Token.Solidus]
#guard lex_haskell "<-" == [Token.ArrowLeft]
#guard lex_haskell "->" == [Token.ArrowRight]
#guard lex_haskell "=>" == [Token.DoubleArrowRight]
#guard lex_haskell "@"  == [Token.Ampersand]
#guard lex_haskell "~"  == [Token.Tilde]

/-
Reserved Keywords
-/

#guard lex_haskell "case" == [Token.Case]
#guard lex_haskell "class" == [Token.Class]
#guard lex_haskell "data" == [Token.Data]
#guard lex_haskell "default" == [Token.Default]
#guard lex_haskell "deriving" == [Token.Deriving]
#guard lex_haskell "do" == [Token.Do]
#guard lex_haskell "else" == [Token.Else]
#guard lex_haskell "foreign" == [Token.Foreign]
#guard lex_haskell "if" == [Token.If]
#guard lex_haskell "import" == [Token.Import]
#guard lex_haskell "in" == [Token.In]
#guard lex_haskell "infix" == [Token.Infix]
#guard lex_haskell "infixl" == [Token.Infixl]
#guard lex_haskell "infixr" == [Token.Infixr]
#guard lex_haskell "instance" == [Token.Instance]
#guard lex_haskell "let" == [Token.Let]
#guard lex_haskell "module" == [Token.Module]
#guard lex_haskell "newtype" == [Token.Newtype]
#guard lex_haskell "of" == [Token.Of]
#guard lex_haskell "then" == [Token.Then]
#guard lex_haskell "type" == [Token.TypeT]
#guard lex_haskell "where" == [Token.Where]

/-
Semireserved Keywords
-/

#guard lex_haskell "_" == [Token.Underscore]
#guard lex_haskell "as" == [Token.As]
#guard lex_haskell "qualified" == [Token.Qualified]

/-
String Literals
-/

#guard lex_haskell "\"foo\"" == [Token.LitString "\"foo\""]

/-
Char Literals
-/

#guard lex_haskell "'a'" == [Token.LitChar 'a']

/-
Integer Literals
-/

#guard lex_haskell "1234" == [Token.LitInteger 1234]
#guard lex_haskell "01234" == [Token.LitInteger 1234]
#guard lex_haskell "0o11" == [Token.LitInteger 9]
#guard lex_haskell "0O11" == [Token.LitInteger 9]
#guard lex_haskell "0xff" == [Token.LitInteger 255]
#guard lex_haskell "0XFF" == [Token.LitInteger 255]

/-
Float Literals
-/


#guard lex_haskell "12.34" == [Token.LitFloat 1234 100]
#guard lex_haskell "12.340" == [Token.LitFloat 1234 100]
#guard lex_haskell "0.1" == [Token.LitFloat 1 10]
#guard lex_haskell "0.0" == [Token.LitFloat 0 1]
#guard lex_haskell "2.0" == [Token.LitFloat 2 1]
#guard lex_haskell "12.34e5" == [Token.LitFloat 0 0] -- Incorrect parse!
#guard lex_haskell "12.34E5" == [Token.LitFloat 0 0] -- Incorrect parse!
#guard lex_haskell "12.34e+5" == [Token.LitFloat 0 0] -- Incorrect parse!
#guard lex_haskell "12.34e-5" == [Token.LitFloat 0 0] -- Incorrect parse!
#guard lex_haskell "12.34E+5" == [Token.LitFloat 0 0] -- Incorrect parse!
#guard lex_haskell "12.34E-5" == [Token.LitFloat 0 0] -- Incorrect parse!

/-
Identifiers
-/

#guard lex_haskell "Foo" == [Token.QConId [] "Foo"]
#guard lex_haskell "Foo.Bar" == [Token.QConId ["Foo"] "Bar"]
#guard lex_haskell "Foo.Bar.Baz" == [Token.QConId ["Foo", "Bar"] "Baz"]

#guard lex_haskell "foo" == [Token.QVarId [] "foo"]
#guard lex_haskell "Foo.bar" == [Token.QVarId ["Foo"] "bar"]
#guard lex_haskell "Foo.Bar.baz" == [Token.QVarId ["Foo", "Bar"] "baz"]

/-
Comments
-/

#guard lex_haskell "--\n" == [Token.Whitespace "--\n"]
#guard lex_haskell "-- foo bar\n" == [Token.Whitespace "-- foo bar\n"]
#guard lex_haskell "{- block comment -}" == [Token.Whitespace "{- block comment -}"]

/-
Stranger Things
-/

#guard lex_haskell "0o79" == [Token.LitInteger 7, Token.LitInteger 9]
#guard lex_haskell "datatype" == [Token.QVarId [] "datatype"]
