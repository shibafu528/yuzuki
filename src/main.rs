use std::io;

fn main() {
    println!(":: \"Yuzuki\" interpreter ::");

    loop {
        let mut input = String::new();

        if let Err(e) = io::stdin().read_line(&mut input) {
            eprintln!("Input Error: {}", e);
            continue;
        }

        println!("Input: {}", input);
        parse(&input);
    }
}

enum Token {
    LeftParenthesis,
    RightParenthesis,
    Literal(String),
    String(String),
    Quote,
    Whitespace
}

macro_rules! extract_literal_and_clear {
    ($buf: ident, $tokens: ident) => {
        if !$buf.is_empty() {
            $tokens.push(Token::Literal($buf));
            $buf = String::new();
        }
    }
}

fn lex(code: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut in_comment = false;
    let mut in_text = false;
    let mut in_escaped_text = false;
    let mut text_buffer = String::new();

    for chr in code.chars() {
        if in_comment {
            if chr == '\n' {
                in_comment = false;
            }
        } else if in_text {
            match chr {
                '"' => {
                    if in_escaped_text {
                        text_buffer.push(chr);
                    } else {
                        in_text = false;
                        tokens.push(Token::String(text_buffer));
                        text_buffer = String::new()
                    }
                },
                '\\' => in_escaped_text = true,
                _ => text_buffer.push(chr)
            }
        } else {
            match chr {
                ';' => in_comment = true,
                '(' => {
                    extract_literal_and_clear!(text_buffer, tokens);
                    tokens.push(Token::LeftParenthesis)
                },
                ')' => {
                    extract_literal_and_clear!(text_buffer, tokens);
                    tokens.push(Token::RightParenthesis)
                },
                '\'' => tokens.push(Token::Quote),
                '"' => {
                    in_text = true;
                    text_buffer = String::new();
                },
                ' ' | '\n' => {
                    extract_literal_and_clear!(text_buffer, tokens);
                    tokens.push(Token::Whitespace);
                },
                _ => text_buffer.push(chr)
            }
        }
    }

    if in_text {
        eprintln!("Lex Err: text literal isn't terminated. \"{}\"", text_buffer)
    } else if !text_buffer.is_empty() {
        tokens.push(Token::Literal(text_buffer));
    }

    tokens
}

fn parse(code: &str) {
    let tokens = lex(code);

    println!("Lexer Result:");

    for token in &tokens {
        match token {
            Token::LeftParenthesis => println!("("),
            Token::RightParenthesis => println!(")"),
            Token::Quote => println!("'"),
            Token::Literal(s) => println!("{}", s),
            Token::String(s) => println!("\"{}\"", s),
        }
    }

    println!("----");

    // ----


}