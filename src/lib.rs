mod token;
mod lexer;
mod ast;
mod parser;

pub use token::Token;
pub use lexer::Lexer;
pub use ast::Program;
pub use parser::Parser;


/* PrimaryExpression ::= "this"
    | ObjectLiteral
    | ( "(" Expression ")" )
    | Identifier
    | ArrayLiteral
    | Literal

Literal ::= ( <DECIMAL_LITERAL>
    | <HEX_INTEGER_LITERAL>
    | <STRING_LITERAL>
    | <BOOLEAN_LITERAL>
    | <NULL_LITERAL>
    | <REGULAR_EXPRESSION_LITERAL> )

Identifier ::= <IDENTIFIER_NAME>

ArrayLiteral ::= "[" ( ( Elision )? "]"
    | ElementList Elision "]"
    | ( ElementList )? "]" )

ElementList ::= ( Elision )? AssignmentExpression
    ( Elision AssignmentExpression )*

Elision ::= ( "," )+

ObjectLiteral ::= "{" ( PropertyNameAndValueList )? "}"

PropertyNameAndValueList ::= PropertyNameAndValue ( "," PropertyNameAndValue
    | "," )*

PropertyNameAndValue ::= PropertyName ":" AssignmentExpression

PropertyName ::= Identifier
    | <STRING_LITERAL>
    | <DECIMAL_LITERAL>
*/
