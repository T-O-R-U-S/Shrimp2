use std::fs::File;
use std::io::Read;

use std::collections::HashMap;

use thiserror::Error;

use anyhow::{anyhow, Result as anyErr};

macro_rules! bail {
	($e: expr) => {
			return Err(anyhow!($e))
	};
}

#[derive(Error, Debug)]
pub enum Error {
	#[error("The ShrimpLang parser itself has encountered an error at line {1}! Please open an issue on GitHub!\n\nExact error:\n{0}")]
	ParserError(anyhow::Error, usize),
	#[error("Unknown token `{0}` at {1}. (Is {0} correct? Open an issue on GitHub!)")]
	UnknownToken(char, usize),
	#[error("Expected a {expected:?}, found {found:?}")]
	ExpectedToken { expected: Token, found: Token },
	#[error("Delimiter never closed.")]
	UnclosedDelimiter,
	#[error("Something was declared outside of a function!")]
	OutOfFunction,
	#[error("There is no main function!")]
	NoMain,
	#[error("Unexpected end of line")]
	UnexpectedEOL
}

impl Error {
	pub fn handle_default(&self) {
		println!("{}", self);
		std::process::exit(1)
	}
	pub fn handle(&self) {
		match self {
			_ => self.handle_default(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
	LineEnd,
	FunctionDecl,
	Quote,
	Number(i32),
	Bool(bool),
	Ident(String),
	String(String),
	Group(Vec<Token>),
	Codeblock(Vec<Token>),
	VarAccessor(String)
}

pub struct Line {
	pub line: String,
	pub line_num: usize,
	pub pos: isize,
	pub current_char: Option<char>,
}

impl Line {
	pub fn advance(&mut self) {
		self.pos += 1;
		self.current_char = if self.pos as usize >= self.line.len() {
			None
		} else {
			Some(self.line.chars().collect::<Vec<char>>()[self.pos as usize])
		}
	}
	pub fn new(line: String, line_num: usize) -> Self {
		let mut this = Line {
			line,
			line_num,
			pos: -1,
			current_char: None,
		};
		this.advance();
		this
	}
}

pub struct Lexer {
	pub line: String,
	pub line_num: usize,
	pub pos: isize,
	pub current_char: Option<char>,
	pub tokens: Vec<Token>,
}

pub fn make_tokens(mut line: Line) -> anyErr<Vec<Token>> {
	let mut tokens = Vec::new();

	while line.current_char.is_some() {
		match line.current_char.unwrap() {

			'$' => {
				let mut word = String::new();

				line.advance();

				while let 'Z'..='z' = line.current_char.unwrap() {
					word.push(line.current_char.unwrap());
					line.advance();
					if line.current_char.is_none() {
						break
					}
				}

				tokens.push(Token::VarAccessor(word))
			}

			'"' | '\'' => {
				let current_quot = line.current_char.unwrap();

				let mut word = String::new();

				line.advance();

				while line.current_char.unwrap() != current_quot
					|| line.line.chars().collect::<Vec<char>>()[line.pos as usize - 1] == '\\'
				{
					word.push(line.current_char.unwrap());
					line.advance();
					if line.current_char.is_none() {
						break
					}
				}

				tokens.push(Token::String(word))
			}
			'Z'..='z' => {
				let mut word = String::new();
				while let Some('A'..='z') = line.current_char {
					word.push(line.current_char.unwrap());
					line.advance()
				}
				tokens.push(Token::Ident(word));
			}
			'0'..='9' => {
				let mut num = String::new();

				while let Some('0'..='9') = line.current_char {
					num.push(line.current_char.unwrap());
					line.advance()
				}

				tokens.push(Token::Number(num.parse().unwrap()))
			}
			'?' => {
				line.advance();
				match line.current_char.unwrap() {
					'T' => tokens.push(Token::Bool(true)),
					'F' => tokens.push(Token::Bool(false)),
					_ => {
						bail!(Error::UnknownToken(
							line.current_char.unwrap(),
							line.line_num
						))
					}
				}
			}
			'(' => {
				line.advance();

				let mut word = String::new();

				let mut brack_count = 1;

				while brack_count > 0 {
					match line.current_char {
						Some(chr) => match chr {
							')' => {
								if brack_count > 1 {
									word.push(')')
								}
								brack_count -= 1;
							}
							'(' => {
								brack_count += 1;
								word.push('(')
							}
							_ => word.push(chr),
						},
						None => bail!(Error::UnclosedDelimiter),
					}
					line.advance()
				}

				let n_tokens = make_tokens(Line::new(word, 0));

				tokens.push(Token::Group(n_tokens.unwrap()))
			}

			'{' => {
				line.advance();

				let mut word = String::new();

				let mut brack_count = 1;

				while brack_count > 0 {
					match line.current_char {
						Some(chr) => match chr {
							'}' => {
								if brack_count > 1 {
									word.push(')')
								}
								brack_count -= 1;
							}
							'{' => {
								brack_count += 1;
								word.push('(')
							}
							_ => word.push(chr),
						},
						None => bail!(Error::UnclosedDelimiter),
					}
					line.advance()
				}

				let n_tokens = make_tokens(Line::new(word, 0));

				tokens.push(Token::Codeblock(n_tokens.unwrap()))
			}

			'@' => tokens.push(Token::FunctionDecl),

			';' => tokens.push(Token::LineEnd),
			c if c.is_whitespace() => {}
			_ => {
				bail!(Error::UnknownToken(
					line.current_char.unwrap(),
					line.line_num
				))
			}
		}
		line.advance();
	}
	Ok(tokens)
}

impl Lexer {
	pub fn new(line: String, line_num: usize) -> Self {
		let mut this = Lexer {
			line: line.split("").collect::<String>(),
			line_num,
			pos: -1,
			current_char: None,
			tokens: Vec::new(),
		};
		this.advance();
		this
	}
	pub fn advance(&mut self) {
		self.pos += 1;
		self.current_char = if self.pos as usize >= self.line.len() {
			None
		} else {
			Some(self.line.chars().collect::<Vec<char>>()[self.pos as usize])
		}
	}
	pub fn make_tokens(self) -> anyErr<Vec<Token>> {
		make_tokens(Line::new(self.line, self.line_num))
	}
}

fn main() -> anyErr<()> {
	let mut file = File::open("./main.imp")?;
	let mut code = String::new();

	file.read_to_string(&mut code)?;

	let lexer = Lexer::new(code, 0);

	let tokens = lexer.make_tokens().unwrap();

	run(funcs(tokens).unwrap())
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
	arguments: Vec<Token>,
	instructions: Token
}

pub fn funcs(tokens: Vec<Token>) -> anyErr<HashMap<String, Function>> {
	let mut functions = HashMap::new();

	let mut tokens = tokens.into_iter().peekable();

	while tokens.peek().is_some() {
		match tokens.next() {
			Some(token) => match token {
				Token::FunctionDecl => {
					if let Some(Token::Ident(ident)) = tokens.next() {
						functions.insert(
							ident,
							Function {
								arguments: if let Token::Group(inner) = tokens.next().unwrap() {
									inner
								} else {
									bail!(Error::ExpectedToken {
										expected: Token::Group(vec![Token::Ident("Your function here".to_string())]),
										found: tokens.next().unwrap()
									})
								},
								instructions: tokens.next().unwrap()
							},
						);
					} else {
						bail!(Error::ExpectedToken {
								expected: Token::Ident("Identifier".to_string()),
								found: token
							})
						
					}
				}
				_ => Error::OutOfFunction.handle(),
			},
			None => std::process::exit(0),
		}
	}

	match functions.get(&"main".to_string()) {
		Some(func) => {
			Ok(functions)
		}
		None => bail!(Error::NoMain)
	}	
}

pub fn run(functions: HashMap<String, Function>) -> anyErr<()> {

	// Naming this variable "main" will overwrite the main fn
	let shrimp_main = functions.get(&"main".to_string()).unwrap();

	println!("{:?}", shrimp_main);

	Ok(())
}