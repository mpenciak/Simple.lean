//! Here we define the core language AST

struct ImportStatement(String);

enum NamespaceModifiers {
    Open,
    Close,
    Namespace,
}

enum Definitions {
    FunctionDef,
    TypeAlias,
    StructDef,
    ImplBlock,
}

enum CoreTypes {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Bool,
    Str,
    Char,
    Unit,
}

enum TypeExpr {
    CoreType(CoreTypes),
    FunctionType,
    List(Box<TypeExpr>),
    Tuple(Vec<TypeExpr>),
    HashMap(Box<TypeExpr>, Box<TypeExpr>),
    Alias(String, Box<TypeExpr>),
    Structure(String),
}

enum Literal {
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    Bool(bool),
    Unit,
}

enum Expr {
    Literal(Literal),
}
