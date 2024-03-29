use crate::lex::lex;

mod lex;

fn main() {
    let src = r"

    var a: u8 = 0;

    fn add(a: i32, b: i32): i32 {
        return a + b;
    }
    
    ";

    let tokens = lex(src);

    println!("{:?}", tokens);
}
