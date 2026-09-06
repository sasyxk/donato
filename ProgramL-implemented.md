# Donato: implemented grammar

Derived from the tokenizer, parser, AST and code generator in this repository.
`ProgramL.md` remains the original design draft. `|` means alternatives, `?`
optional, `*` zero or more, `+` one or more; quoted text is literal syntax.

```ebnf
Program          = TopLevelDecl+
TopLevelDecl     = FuncDecl | StructDecl | ClassDecl

FuncDecl         = "function" ReturnType Identifier "(" ParamList? ")" Block
ReturnType       = "void" | "ref"? Type
ParamList        = Param ("," Param)*
Param            = "ref"? Type Identifier

StructDecl       = "struct" TypeName "{" FieldDecl+ "}"
FieldDecl        = FieldType Identifier ";"
FieldType        = PrimitiveType "*"* | TypeName "*"+

ClassDecl        = "class" TypeName "{"
                       PrivateFields?
                       "public" ":" ConstructorDecl FuncDecl*
                   "}"
PrivateFields    = ("private" ":")? FieldDecl+
ConstructorDecl  = TypeName "(" ParamList? ")" Block

Block            = "{" Stmt+ "}"
Stmt             = VarDeclStmt
                 | AutoDeclStmt
                 | RefDeclStmt
                 | AssignStmt
                 | IfStmt
                 | WhileStmt
                 | CallStmt
                 | ReturnStmt
                 | DeleteStmt
                 | PrintStmt

VarDeclStmt      = Type Identifier "=" Expr ";"
AutoDeclStmt     = "auto" Identifier "=" Expr ";"
RefDeclStmt      = "ref" Type Identifier "=" Expr ";"
AssignStmt       = AssignTarget "=" Expr ";"
AssignTarget     = Identifier | MemberAccess
IfStmt           = "if" "(" Condition ")" Block ("else" Block)?
WhileStmt        = "while" "(" Condition ")" Block
CallStmt         = (FuncCall | MethodCall) ";"
ReturnStmt       = "return" Expr? ";"
DeleteStmt       = "delete" Expr ";"
PrintStmt        = "print" "(" Expr ")" ";"

Type             = (PrimitiveType | TypeName) "*"*
PrimitiveType    = "bool" | "double" | "int"
                 | "int8" | "int16" | "int32" | "int64"

Condition        = Expr (CompareOp Expr)?
CompareOp        = "==" | "!=" | "<" | "<=" | ">" | ">="

Expr             = Term (("+" | "-") Term)*
Term             = Factor (("*" | "/") Factor)*
Factor           = "-" Factor
                 | "*" Factor
                 | "(" Expr ")"
                 | InlineIfExpr
                 | InlineLetExpr
                 | Number
                 | Boolean
                 | FuncCall
                 | MethodCall
                 | MemberAccess
                 | Identifier
                 | "this"
                 | NewExpr
                 | AddressExpr
                 | NullPtrExpr

InlineIfExpr     = "if" (Condition | "(" Condition ")")
                       "then" Expr "else" Expr
InlineLetExpr    = "let" Binding ("," Binding)* "in" Expr
Binding          = Identifier "=" Expr

FuncCall         = Identifier "(" ArgList? ")"
MethodCall       = MemberAccess "(" ArgList? ")"
ArgList          = Expr ("," Expr)*
MemberAccess     = (Identifier | "this") ("." Identifier)+
NewExpr          = "new" TypeName "(" ArgList? ")"
AddressExpr      = "&" Expr
NullPtrExpr      = "nullptr" "<" Type ">"

Identifier       = Lowercase (Letter | Digit)*
TypeName         = Uppercase (Letter | Digit)*
Letter           = Lowercase | Uppercase
Lowercase        = "a" | ... | "z"
Uppercase        = "A" | ... | "Z"
Digit            = "0" | ... | "9"
Boolean          = "true" | "false"
Number           = "-"? (Digit+ ("." Digit*)? | "." Digit+) Exponent?
Exponent         = ("e" | "E") ("+" | "-")? Digit+

ReservedWord     = "if" | "then" | "else" | "let" | "in" | "while"
                 | "this" | "return" | "struct" | "class"
                 | "private" | "public" | "function" | "void" | "ref"
                 | "delete" | "print" | "new" | "nullptr"
                 | "true" | "false" | "double" | "bool" | "int"
                 | "int8" | "int16" | "int32" | "int64" | "auto"
```

Constraints of the current implementation:

- Commas separate parameters and arguments; trailing commas are rejected, even
  with whitespace or comments before `)`. Empty lists `()` remain allowed where
  the grammar has `ParamList?` or `ArgList?`.
- Identifiers exclude reserved words and underscores. Whitespace, `//` line
  comments and non-nested `/* ... */` comments are ignored between tokens.
  A block comment must end with `*/`, which may occur at the end of the file.
  Reaching EOF without that delimiter is a lexical error reporting the opening
  `/*` location, regardless of the final character or newline. A `//` comment
  may end at EOF without a newline.
- A minus immediately followed by a digit or `.` is part of a number token.
  Write subtraction as `x - 1`; `x-1` and `x -1` do not parse as subtraction.
- `int` means `int64`. Integer literals are `int64`; literals with a decimal
  point or exponent are `double`.
- Integer conditions in `if`, `while` and inline `if` are false for zero and
  true for any nonzero value, for every supported integer width.
- `double` arithmetic and negation produce `double`; comparisons produce `bool`.
  Double comparisons are ordered: any comparison involving NaN, including `!=`,
  is false. Positive and negative zero compare equal.
- Use `function int main() { ... return 0; }` as the executable entry point.
  Declare functions and types before use; self-recursive functions are supported.
  Declarations of functions, structs and classes belong at the top level.
  Variables and other statements at the top level are rejected during parsing,
  before code generation; global variables are not supported.
- Struct and class names share one namespace across the program. A type name
  can be declared only once, regardless of category, declaration order, layout
  or use. Duplicate names are rejected during parsing, before code generation.
  Names are case-sensitive.
- Blocks contain at least one statement. The parser rejects statements after a
  `return` or an `if`/`else` whose two branches cannot fall through, at any nesting
  depth. Comments after these statements are allowed.
- Every function, method and constructor must return on all structurally checked
  paths. A final `if`/`else` whose branches both return satisfies this rule without
  an additional `return`. Use `return;` for `void` functions and constructors.
  An `if` without `else` and a `while` can fall through; loop conditions are not
  evaluated to prove termination or nontermination, even for `while (true)`.
- A class has one constructor, with the class's exact name, followed by public
  methods. Fields precede `public:` and are private, even without `private:`.
  Struct fields are public. Aggregate fields require pointers to other aggregates.
  Unclosed constructor or method parameter lists and bodies are rejected at EOF
  with a syntax diagnostic identifying the missing closing delimiter and context.
- `this` is available inside classes. Member access starts with a name or `this`;
  arbitrary postfix chains such as `obj.get().field` are not implemented.
- `new StructName(...)` initializes fields in declaration order;
  `new ClassName(...)` calls the constructor. Both yield pointers.
  `nullptr<T>` has type `T*`; `delete` takes a pointer.
- A `ref` binding or argument must refer to suitable existing storage. Unary `*`
  reads through a pointer; assignment through `*p = ...` is not a statement form.
  `&` consumes an entire `Expr` in this parser and requires an addressable result.
- Functions and methods may return `ref Type` to existing storage. Binding the
  result with `ref` creates an alias, so writes affect the original storage;
  an ordinary value declaration copies the result. Reference returns also
  support pointer and aggregate types.
- Comparisons occur only in `Condition`, not as general expressions such as
  `bool b = x < y;`. Logical operators and chained comparisons are not implemented.
- Calls used as standalone statements require a `void` return type. Use the
  result of a value-returning function in an expression or assignment.
- `print` accepts `int8`, `int16`, `int32`, `int64`, the `int` alias and `bool`,
  and prints `value: <number>`. Smaller signed integers are sign-extended to
  `int64` for the runtime call; booleans print `0` for false and `1` for true.
  Promotion affects only the print argument, preserving the original variable's
  type and value, including through references. The argument expression is
  evaluated once. Other types, including `double`, pointers and aggregates, are
  rejected during code generation before the print runtime call is emitted.
- Nested `let` bindings temporarily shadow outer bindings and restore them on exit.

Implementation: [tokenizer](src/parser/tokenizer.cpp),
[parser](src/parser/parser.cpp), [AST](src/ast/), [types](src/type/),
[values](src/value/). Runnable programs: [examples](examples/).
