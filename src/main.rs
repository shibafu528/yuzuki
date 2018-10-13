extern crate rustyline;

use std::fmt;
use std::collections::VecDeque;
use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    println!(":: \"Yuzuki\" interpreter ::");

    let mut rl = Editor::<()>::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());

                println!("Input: {}", line);

                let expr = match parse(&line) {
                    Ok(expr) => expr,
                    Err(e) => {
                        eprintln!("{}", e);
                        continue;
                    }
                };

                println!("{:?}", expr);
            },
            Err(ReadlineError::Interrupted) => {
                println!("C-c");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("C-d");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
}

#[derive(Debug)]
enum Token {
    LeftParenthesis,
    RightParenthesis,
    Literal(String),
    String(String),
    Quote,
    Whitespace
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::LeftParenthesis => write!(f, "("),
            Token::RightParenthesis => write!(f, ")"),
            Token::Literal(ref s) => write!(f, "{}", s),
            Token::String(ref s) => write!(f, "\"{}\"", s),
            Token::Quote => write!(f, "'"),
            Token::Whitespace => write!(f, " "),
        }
    }
}

macro_rules! extract_literal_and_clear {
    ($buf: ident, $tokens: ident) => {
        if !$buf.is_empty() {
            $tokens.push_back(Token::Literal($buf));
            $buf = String::new();
        }
    }
}

#[derive(Debug)]
enum SyntaxError {
    EOF,
    UnterminatedString(String),
    UnterminatedList,
    UnexpectedToken(Token),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SyntaxError::EOF => write!(f, "Syntax Error: Unexpected EOF"),
            SyntaxError::UnterminatedString(ref s) => write!(f, "Syntax Error: Unterminated string: \"{}\"", s),
            SyntaxError::UnterminatedList => write!(f, "Syntax Error: Unterminated list"),
            SyntaxError::UnexpectedToken(ref t) => write!(f, "Syntax Error: Unexpected token: {}", t),
        }
    }
}

fn tokenize(code: &str) -> Result<VecDeque<Token>, SyntaxError> {
    let mut tokens: VecDeque<Token> = VecDeque::new();
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
                        tokens.push_back(Token::String(text_buffer));
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
                    tokens.push_back(Token::LeftParenthesis)
                },
                ')' => {
                    extract_literal_and_clear!(text_buffer, tokens);
                    tokens.push_back(Token::RightParenthesis)
                },
                '\'' => tokens.push_back(Token::Quote),
                '"' => {
                    in_text = true;
                    text_buffer = String::new();
                },
                ' ' | '\n' => {
                    extract_literal_and_clear!(text_buffer, tokens);
                    tokens.push_back(Token::Whitespace);
                },
                _ => text_buffer.push(chr)
            }
        }
    }

    if in_text {
        return Err(SyntaxError::UnterminatedString(text_buffer));
    } else if !text_buffer.is_empty() {
        tokens.push_back(Token::Literal(text_buffer));
    }

    Ok(tokens)
}

#[derive(Debug)]
enum Atom {
    Symbol(String),
    Integer(i64),
    Float(f64),
    String(String),
    Nil,
}

impl Atom {
    fn is_nil(&self) -> bool {
        match *self {
            Atom::Nil => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct Cons {
    car: Box<Expression>,
    cdr: Box<Expression>,
}

impl Cons {
    fn is_nil(&self) -> bool {
        let is_car_nil = match *(self.car) {
            Expression::Atom(ref atom) => atom.is_nil(),
            _ => false,
        };
        let is_cdr_nil = match *(self.cdr) {
            Expression::Atom(ref atom) => atom.is_nil(),
            _ => false,
        };
        is_car_nil && is_cdr_nil
    }
}

#[derive(Debug)]
enum Expression {
    Cons(Cons),
    Atom(Atom),
}

const NIL_EXPRESSION: Expression = Expression::Atom(Atom::Nil);

fn parse(code: &str) -> Result<Expression, SyntaxError> {
    let mut tokens = tokenize(code)?;

    println!("Tokenizer Result:");

    for token in &tokens {
        println!("{}", token);
    }

    println!("----");

    // ----

    parse_recursive(&mut tokens)
}

fn parse_recursive(tokens: &mut VecDeque<Token>) -> Result<Expression, SyntaxError> {
    while let Some(token) = tokens.pop_front() {
        return match token {
            Token::LeftParenthesis => Ok(Expression::Cons(parse_list(tokens)?)),
            Token::RightParenthesis => Err(SyntaxError::UnexpectedToken(token)),
            Token::Quote => Ok(Expression::Cons(Cons {
                car: Box::new(Expression::Atom(Atom::Symbol("quote".to_owned()))),
                cdr: Box::new(parse_recursive(tokens)?)
            })),
            Token::Literal(s) => Ok(Expression::Atom(parse_literal(&s)?)),
            Token::String(s) => Ok(Expression::Atom(Atom::String(s))),
            _ => continue,
        }
    }

    Err(SyntaxError::EOF)
}

fn skip_whitespace_token(tokens: &mut VecDeque<Token>) {
    while !tokens.is_empty() {
        match tokens.front().unwrap() {
            Token::Whitespace => (),
            _ => break,
        }

        // drop
        tokens.pop_front();
    }
}

fn parse_list(tokens: &mut VecDeque<Token>) -> Result<Cons, SyntaxError> {
    // (? ?...
    //   `-- Check
    skip_whitespace_token(tokens);
    if let Some(car_token) = tokens.front() {
        match car_token {
            Token::RightParenthesis => {
                // => ()
                return Ok(Cons {car: Box::new(NIL_EXPRESSION), cdr: Box::new(NIL_EXPRESSION)})
            },
            _ => (),
        }
    } else {
        return Err(SyntaxError::UnterminatedList)
    }

    // => (car_token ?...
    //      `-- Parse
    let car = parse_recursive(tokens)?;
    // println!("(Debug) Parse car: {:?}", car);

    // => (car ?...
    //          `-- Check
    skip_whitespace_token(tokens);
    let mut is_dotted_pair = false;
    if let Some(cdr_token) = tokens.front() {
        match cdr_token {
            Token::Literal(s) => {
                if s == "." {
                    // => (car . ?...
                    // 1つ読み捨ててさらに次のトークンを読んでcdrにする必要がある。
                    // ただし、ここではmutableな借用ができないので、一旦スコープを抜ける。
                    is_dotted_pair = true;
                }
            },
            _ => (),
        }
    } else {
        return Err(SyntaxError::UnterminatedList)
    }

    let cdr = if is_dotted_pair {
        // => (car . ?...
        //          `-- Skip
        tokens.pop_front();

        // => (car . ?...
        //            `-- Parse
        let cdr = parse_recursive(tokens)?;

        // => (car . cdr ?
        //                `-- Check
        skip_whitespace_token(tokens);
        match tokens.pop_front() {
            Some(token) => {
                match token {
                    Token::RightParenthesis => (),
                    _ => return Err(SyntaxError::UnterminatedList),
                }
            },
            None => return Err(SyntaxError::UnterminatedList),
        }

        cdr
    } else {
        // => (car ?...
        //          `-- Parse
        let cdr = parse_list(tokens)?;

        if cdr.is_nil() {
            // リスト終端のトークンが残っているので読み捨てる
            tokens.pop_front();

            NIL_EXPRESSION
        } else {
            Expression::Cons(cdr)
        }
    };
    // println!("(Debug) Parse cdr: {:?}", cdr);

    // => (car . cdr)
    Ok(Cons {car: Box::new(car), cdr: Box::new(cdr)})
}

fn parse_literal(literal: &str) -> Result<Atom, SyntaxError> {
    if literal == "nil" {
        return Ok(Atom::Nil)
    }

    let mut has_point = false;
    for chr in literal.chars() {
        match chr {
            '.' => {
                if has_point {
                    // double point is invalid
                    return Ok(Atom::Symbol(literal.to_owned()))
                }
                has_point = true
            },
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '+' => (),
            _ => return Ok(Atom::Symbol(literal.to_owned()))
        }
    }

    if has_point {
        Ok(Atom::Float(literal.parse::<f64>().unwrap()))
    } else {
        Ok(Atom::Integer(literal.parse::<i64>().unwrap()))
    }
}