# Lexical Structure of Haskell 2010

## Regular Expressions used in Haskell Report

- Optional: `[pattern]`
- Zero or more: `{pattern}`
- Grouping: `(pattern)`
- Choice: `pat | pat`
- Difference: `pat_{pat}`

## Tokens

- qvarid
  + (modid`.`)* varid
    * varid := (small(small|large|digit|`'`)*) / reservedid
- qconid
  + (modid`.`)* conid
    * conid := (large(small|large|digit|`'`)*)
- qvarsym
  + (modid`.`)* varsym
    * varsym := ((symbol/`:`)symbol*) / (reservedop | dashes)
- qconsym
  + (modid`.`)* consym
    * consym := (`:` symbol*) / reservedop
- literal
  + integer
    * decimal
    * `0o` octal
    * `0O` octal
    * `0x` hexadecimal
    * `0X` hexadecimal
  + float
    * decimal `.` decimal
    * decimal `.` decimal `e` decimal
    * decimal `.` decimal `e+` decimal
    * decimal `.` decimal `e-` decimal
    * decimal `.` decimal `E` decimal
    * decimal `.` decimal `E+` decimal
    * decimal `.` decimal `E-` decimal
    * decimal `e` decimal
    * decimal `e+` decimal
    * decimal `e-` decimal
    * decimal `E` decimal
    * decimal `E+` decimal
    * decimal `E-` decimal
  + char
  + string
- special
  + `(`
  + `)`
  + `,`
  + `;`
  + `[`
  + `]`
  + `\``
  + `{`
  + `{`
- reservedop
  + `..`
  + `:`
  + `::`
  + `=`
  + `\`
  + `|`
  + `<-`
  + `->`
  + `@`
  + `~`
  + `=>`
- reservedid
  + `case`
  + `class`
  + `data`
  + `default`
  + `deriving`
  + `do`
  + `else`
  + `foreign`
  + `if`
  + `import`
  + `in`
  + `infix`
  + `infixl`
  + `infixr`
  + `instance`
  + `let`
  + `module`
  + `newtype`
  + `of`
  + `then`
  + `type`
  + `where`
  + `_`
- whitespace
  + (whitestuff)+
    * whitestuff := whitechar | comment | ncomment
    * whitechar := newline | vertab | space | tab | uniWhite
    * comment := `--` ... | `---` ...
    * ncomment := `{-` ... `-}`

## Spans
All tokens are on the same line, except `ncomment`, which may span multiple lines.