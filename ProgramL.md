Program      = Stmt* 

Stmt         = VarDeclStmt 
             | AssignStmt
             | IfStmt
             | ForStmt
             | WhileStmt
             | FuncDeclStmt
             | ClassDeclStmt 
             | ReturnStmt 

ConstDeclStmt  = "const" Identifier "=" Expr
VarDeclStmt  = "var" Identifier "=" Expr  
AssignStmt   = "upd" Identifier "=" Expr

IfStmt       = "if" "(" Expr ")" Block ( "else" Block )? 
ForStmt      = "for" Identifier "in" Expr Block 
WhileStmt    = "while" "(" Expr ")" Block 

FuncDeclStmt = "function" Identifier "(" ParamList? ")" Block 
ClassDeclStmt= "class" Identifier "{" ( VarDeclStmt | FuncDeclStmt )* "}"

ReturnStmt   = "return" Expr?

Expr         = MatchExpr | InlineIfExpr | InlineLetExpr | LogicExpr 
MatchExpr    = "match" Expr "{" Case+ "}" 
Case         = "case" Pattern "=>" Expr  
Pattern      = "_" | Number | String | Identifier | ListPattern | TuplePattern
TuplePattern = "(" (Pattern ("," Pattern)*)? ")"
ListPattern  = "[" ( Pattern ( "," Pattern )* )? "]" 

InlineIfExpr = "cond" "(" Expr "," Expr "," Expr ")" 
InlineLetExpr= "let" Assignments "in" Expr  

LogicExpr    = EqualityExpr ( ("and" | "or") EqualityExpr )* 
EqualityExpr = RelationalExpr ( ("==" | "!=") RelationalExpr )* 
RelationalExpr = AdditiveExpr ( ("<" | "<=" | ">" | ">=") AdditiveExpr )* 
AdditiveExpr = MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )* 
MultiplicativeExpr = UnaryExpr ( ("*" | "/") UnaryExpr )*

UnaryExpr    = ("-" | "!")? PrimaryExpr 
PrimaryExpr  = Identifier | Number | String | "(" Expr ")" | ListExpr | FuncCall | ThisExpr  

ThisExpr     = "this" "." Identifier   
FuncCall     = Identifier "(" (Expr ("," Expr)*)? ")" 
ListExpr     = "[" (Expr ("," Expr)*)? "]" 

Assignments  = Identifier "=" Expr ("," Identifier "=" Expr)* 

Block        = "{" Stmt* "}" 

Identifier   = Letter (Letter | Digit | "_")* 
Number       = Digit+ ("." Digit+)? 
String       = "\"" (EscapedChar | [^"])* "\""   
EscapedChar  = "\\" ("\"" | "\\" | "n" | "t" | "r") 

Letter       = "a" | ... | "z" | "A" | ... | "Z" 
Digit        = "0" | ... | "9"



*** Function declaration with pattern matching ***
function operation(x, y, op) {
    match (x, y, op) {
        case (_, _, "+") => x + y
        case (_, _, "-") => x - y
        case (_, _, "*") => x * y
        case (_, _, "/") => x / y
        case _           => "Invalid operation"
    }
}

*** Variable declaration and update ***
var result = operation(10, 5, "*")
upd result = result * 2  

*** String with special characters ***
var message = "Result:\n\t\"Calculation completed\"\\OK"

*** Class with method ***
class Calculator {
    var history
    
    function add(op) {
        match (op) {
            case "print" => print(this.history)
            case _       => this.history = op
        }
        return 
    }
}

*** Using the class ***
var calc = Calculator()
calc.add("print")

*** If Statement ***
if (x > 0) {
    print("High")
} else {
    print("Low")
}

*** For Statement ***
for item in [1, 2, 3] {
    print(item)
}

*** While Statement ***
while (x < 10) {
    x = x + 1
}

*** let and if inline ***
var a1 = let x = 5, y = 10 in x + y
var a2 = cond(x > 0, 23, 12)