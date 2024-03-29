use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Token<'text> {
    Keyword(&'text str),
    Symbol(&'text str),
    Ident(&'text str),
    String(&'text str),
    Char(&'text str),
    Integer(&'text str),
    Float(&'text str),
    Bool(bool),
    Null,
}

lazy_static! {
    static ref RE_KEYWORD: Regex = Regex::new(r#"^(void|u8|u16|u32|u64|i8|i16|i32|i64|f32|f64|str|val|mval|ref|mref|const|loop|break|continue|fn|return|if|else|struct|extern|static)\b"#).unwrap();
    static ref RE_IDENT: Regex = Regex::new(r#"^[A-Za-z_][A-Za-z0-9_]*"#).unwrap();
    static ref RE_STRING: Regex = Regex::new(r#"^"[^"\n]+""#).unwrap();
    static ref RE_CHAR: Regex = Regex::new(r#"^'.'"#).unwrap();
    static ref RE_INTEGER: Regex = Regex::new(r"^-?[0-9]+").unwrap();
    static ref RE_DECIMAL: Regex = Regex::new(r"^([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)").unwrap();
    static ref RE_BOOL: Regex = Regex::new(r"^(true|false)\b").unwrap();
}

#[derive(Debug)]
pub enum LexError {
    InvalidToken { pos: usize },
}

pub fn lex(text: &str) -> Result<Vec<Token>, LexError> {
    if text.is_empty() {
        return Ok(vec![]);
    }

    let mut tokens = vec![];
    let mut pos = 0;

    loop {
        while let Some(" ") | Some("\n") = text.get(pos..pos + 1) {
            pos += 1;
        }

        if pos >= text.len() {
            break;
        }

        let (token, next_pos) = lex_token(text, pos)?;
        tokens.push(token);
        pos = next_pos;
    }

    Ok(tokens)
}

fn lex_token(text: &str, pos: usize) -> Result<(Token, usize), LexError> {
    lex_keyword(text, pos)
        .or(lex_bool(text, pos))
        .or(lex_null(text, pos))
        .or(lex_ident(text, pos))
        .or(lex_string(text, pos))
        .or(lex_char(text, pos))
        .or(lex_float(text, pos))
        .or(lex_integer(text, pos))
        .or(lex_symbol(text, pos))
        .ok_or(LexError::InvalidToken { pos })
}

fn lex_keyword(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &RE_KEYWORD)?;
    Some((Token::Keyword(token), pos))
}

fn lex_ident(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &RE_IDENT)?;
    Some((Token::Ident(token), pos))
}

fn lex_string(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &RE_STRING)?;
    let token = token
        .strip_prefix("\"")
        .unwrap()
        .strip_suffix("\"")
        .unwrap();
    Some((Token::String(token), pos))
}

fn lex_char(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &RE_CHAR)?;
    let token = token.strip_prefix("'").unwrap().strip_suffix("'").unwrap();
    Some((Token::Char(token), pos))
}

fn lex_integer(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &RE_INTEGER)?;
    Some((Token::Integer(token), pos))
}

fn lex_float(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &RE_DECIMAL)?;
    Some((Token::Float(token), pos))
}

fn lex_bool(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &RE_BOOL)?;
    Some((Token::Bool(token.parse().ok()?), pos))
}

fn lex_null(text: &str, pos: usize) -> Option<(Token, usize)> {
    if let Some(substr) = text.get(pos..) {
        if substr.starts_with("NULL") {
            return Some((Token::Null, pos + "NULL".len()));
        }
    }

    None
}

fn lex_with_pattern<'text>(
    text: &'text str,
    pos: usize,
    pat: &Regex,
) -> Option<(&'text str, usize)> {
    if let Some(slice) = text.get(pos..text.len()) {
        if let Some(m) = pat.find(slice) {
            assert_eq!(
                m.start(),
                0,
                "put caret ^ to match the text from the `pos` (text is sliced to start from pos)"
            );
            return Some((m.as_str(), pos + m.end()));
        }
    }

    None
}

const SYMBOLS: &[&'static str] = &[
    "{", "}", "[", "]", "(", ")", ".", ",", ":", ";", "->", "+=", "+", "-=", "-", "*=", "*", "/=",
    "/", "%=", "%", "^=", "^", "==", "!=", "=", "&&", "&=", "&", "||", "|=", "|", "!", "?", "~",
    "<<=", "<<", ">>=", ">>", "<=", ">=", "<", ">",
];

fn lex_symbol(text: &str, pos: usize) -> Option<(Token<'static>, usize)> {
    if let Some(substr) = text.get(pos..) {
        for &symbol in SYMBOLS {
            if substr.starts_with(symbol) {
                return Some((Token::Symbol(symbol), pos + symbol.len()));
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_all() {
        let src = r#"

        void u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 str 
        val mval ref mref const 
        loop break continue 
        fn return 
        if else 
        struct 
        extern static

        constant
        trueman

        idEnt_123"🦀"'c'
        123 -123   123- 123   123-123   123 - 123   123. .123 123.123 
        true false NULL{}[]()
        .,:;+->-*/%^!===*=/=%=+=-=&=^=|==&&&|||!?~<<<<=<=<>>=>>>=>

        "#;

        use Token::*;

        match lex(src) {
            Ok(tokens) => assert_eq!(
                vec![
                    Keyword("void"),
                    Keyword("u8"),
                    Keyword("u16"),
                    Keyword("u32"),
                    Keyword("u64"),
                    Keyword("i8"),
                    Keyword("i16"),
                    Keyword("i32"),
                    Keyword("i64"),
                    Keyword("f32"),
                    Keyword("f64"),
                    Keyword("str"),
                    Keyword("val"),
                    Keyword("mval"),
                    Keyword("ref"),
                    Keyword("mref"),
                    Keyword("const"),
                    Keyword("loop"),
                    Keyword("break"),
                    Keyword("continue"),
                    Keyword("fn"),
                    Keyword("return"),
                    Keyword("if"),
                    Keyword("else"),
                    Keyword("struct"),
                    Keyword("extern"),
                    Keyword("static"),
                    Ident("constant"),
                    Ident("trueman"),
                    Ident("idEnt_123"),
                    String("🦀"),
                    Char("c"),
                    Integer("123"),
                    Integer("-123"),
                    Integer("123"),
                    Symbol("-"),
                    Integer("123"),
                    Integer("123"),
                    Integer("-123"),
                    Integer("123"),
                    Symbol("-"),
                    Integer("123"),
                    Float("123."),
                    Float(".123"),
                    Float("123.123"),
                    Bool(true),
                    Bool(false),
                    Null,
                    Symbol("{"),
                    Symbol("}"),
                    Symbol("["),
                    Symbol("]"),
                    Symbol("("),
                    Symbol(")"),
                    Symbol("."),
                    Symbol(","),
                    Symbol(":"),
                    Symbol(";"),
                    Symbol("+"),
                    Symbol("->"),
                    Symbol("-"),
                    Symbol("*"),
                    Symbol("/"),
                    Symbol("%"),
                    Symbol("^"),
                    Symbol("!="),
                    Symbol("=="),
                    Symbol("*="),
                    Symbol("/="),
                    Symbol("%="),
                    Symbol("+="),
                    Symbol("-="),
                    Symbol("&="),
                    Symbol("^="),
                    Symbol("|="),
                    Symbol("="),
                    Symbol("&&"),
                    Symbol("&"),
                    Symbol("||"),
                    Symbol("|"),
                    Symbol("!"),
                    Symbol("?"),
                    Symbol("~"),
                    Symbol("<<"),
                    Symbol("<<="),
                    Symbol("<="),
                    Symbol("<"),
                    Symbol(">>="),
                    Symbol(">>"),
                    Symbol(">="),
                    Symbol(">")
                ],
                tokens
            ),

            Err(LexError::InvalidToken { pos }) => assert!(false, "{}", &src[pos..]),
        }
    }
}
