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
	#[error("The ShrimpLang parser itself has encountered an error! Please open an issue on GitHub with your code!")]
	ParserError,
	#[error("Unknown token `{0}`. (Is {0} correct? Open an issue on GitHub!)")]
	UnknownToken(char),
	#[error("Unexpected token. Found {0}")]
	UnexpectedToken(Token),
	#[error("Expected a {expected:?}, found {found:?}")]
	ExpectedToken { expected: Token, found: Token },
	#[error("Delimiter never closed.")]
	UnclosedDelimiter,
	#[error("Something was declared outside of a function!")]
	OutOfFunction,
	#[error("There is no main function!")]
	NoMain,
	#[error("Unexpected end of line")]
	UnexpectedEOL,
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self.clone() {
			Token::VarAccessor(name) => write!(f, "{}", name),
			Token::Array(arr) => {
				let mut to_write = String::new();

				for val in arr {
					to_write += &val.to_string();
				}

				write!(f, "{}", to_write)
			},
			Token::Ident(val) => write!(f, "{}", val),
			Token::String(val) => write!(f, "{}", val),
			Token::Number(num) => write!(f, "{}", num),
			Token::Bool(val) => write!(f, "{}", val),
			Token::LineEnd | Token::FunctionDecl | Token::Native(_) | Token::Codeblock(_) | Token::Group(_) => write!(f, "{:?}", self)
		}
	}
}

struct DisplayHandle<T>(T);

impl std::fmt::Display for DisplayHandle<Vec<Token>> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let mut to_write = String::from("[");

		for i in &self.clone().0 {
			to_write += &format!("{}, ", i.to_string());
		}
		to_write += "]";
		write!(f, "{}", to_write)
	}
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
	LineEnd,
	FunctionDecl,
	Number(i32),
	Bool(bool),
	Ident(String),
	String(String),
	Group(Vec<Token>),
	Codeblock(Vec<Token>),
	Array(Vec<Token>),
	VarAccessor(String),
	Native(fn(Vec<Token>, std::iter::Peekable<std::vec::IntoIter<Token>>) -> anyErr<std::iter::Peekable<std::vec::IntoIter<Token>>>),
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
	pub fn retreat(&mut self) {
		self.pos -= 1;
		self.current_char = if self.pos as usize <= 0 {
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

	while let Some(curr_char) = line.current_char {
		match curr_char {
			'$' => {
				let mut word = String::new();

				line.advance();

				while let Some('a'..='z' | '_') = line.current_char {
					word.push(line.current_char.unwrap());
					line.advance();
				}

				line.retreat();

				tokens.push(Token::VarAccessor(word))
			}

			'"' | '\'' => {
				let current_quot = line.current_char.unwrap();

				let mut word = String::new();

				line.advance();

				while Some(current_quot) != line.current_char {
					if line.current_char.is_none() {
						bail!(Error::UnexpectedEOL)
					}
					if line.current_char == Some('\\') {
						line.advance();

						if line.current_char.is_none() {
							bail!(Error::UnexpectedEOL)
						}
						
						if line.current_char == Some(current_quot) {
							word.push(line.current_char.unwrap());
						} else {
							word.push('\\');
							word.push(line.current_char.unwrap())
						}
						line.advance();
						continue
					}

					word.push(line.current_char.unwrap());
					line.advance();
				}

				/*
				while line.current_char.unwrap() != current_quot
					|| line.line.chars().collect::<Vec<char>>()[line.pos as usize - 1] == '\\'
				{
					word.push(line.current_char.unwrap());
					line.advance();
					if line.current_char.is_none() {
						bail!(Error::UnexpectedEOL)
					}
				}
				*/
				tokens.push(Token::String(word))
			}
			'a'..='z' | '_' => {
				let mut word = String::new();
				while let Some('a'..='z' | '_') = line.current_char {
					word.push(line.current_char.unwrap());
					line.advance()
				}
				tokens.push(Token::Ident(word));
			}
			'0'..='9' => {
				let mut num = String::new();

				while let Some('0'..='9') = line.current_char {
					num.push(line.current_char.unwrap());
					line.advance();
				}
				line.retreat();

				tokens.push(Token::Number(num.parse()?))
			}
			'?' => {
				line.advance();
				match line.current_char {
					Some('T') => tokens.push(Token::Bool(true)),
					Some('F') => tokens.push(Token::Bool(false)),
					None => bail!(Error::UnexpectedEOL),
					_ => {
						bail!(Error::UnknownToken(line.current_char.unwrap(),))
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

				let n_tokens = make_tokens(Line::new(word, line.line_num));

				tokens.push(Token::Group(n_tokens?))
			}
			'[' => {
				line.advance();

				let mut word = String::new();

				let mut brack_count = 1;

				while brack_count > 0 {
					match line.current_char {
						Some(chr) => match chr {
							'[' => {
								if brack_count > 1 {
									word.push(']')
								}
								brack_count -= 1;
							}
							']' => {
								brack_count += 1;
								word.push('[')
							}
							_ => word.push(chr),
						},
						None => bail!(Error::UnclosedDelimiter),
					}
					line.advance()
				}

				let n_tokens = make_tokens(Line::new(word, line.line_num));

				tokens.push(Token::Array(n_tokens?))
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
									word.push('}')
								}
								brack_count -= 1;
							}
							'{' => {
								brack_count += 1;
								word.push('{')
							}
							_ => word.push(chr),
						},
						None => bail!(Error::UnclosedDelimiter),
					}
					line.advance()
				}

				let n_tokens = make_tokens(Line::new(word, line.line_num));

				tokens.push(Token::Codeblock(n_tokens?))
			}

			'@' => tokens.push(Token::FunctionDecl),

			';' => tokens.push(Token::LineEnd),
			c if c.is_whitespace() => {}
			'\\' => {}
			_ => {
				bail!(Error::UnknownToken(curr_char))
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
	pub fn retreat(&mut self) {
		self.pos -= 1;
		self.current_char = if self.pos as usize <= 0 {
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

	let tokens = lexer.make_tokens()?;

	run(funcs(tokens)?)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Function {
	arguments: Vec<Token>,
	instructions: Token,
}

macro_rules! insert_many {
	{funct!$hashmap: expr,$($key:expr => $val:expr),*} => {
		$(
			$hashmap.insert($key.to_string(), Function { arguments: vec![], instructions: Token::Native($val) });
		)*
	};
	{$hashmap: expr,$($key:expr => $val:expr),*} => {
		$(
			$hashmap.insert($key.to_string(), $val)
		)*
	};
}

pub fn funcs(tokens: Vec<Token>) -> anyErr<HashMap<String, Function>> {
	let mut functions = HashMap::new();

	insert_many! {
		funct!functions,
		"print" => |_, mut line| {
			let mut to_print = String::new();

			while let Some(arg) = line.next() {
				match arg {
					Token::VarAccessor(var) => to_print += "thus",
					Token::Array(arr) => to_print += &format!("{}", DisplayHandle(arr)),
					Token::String(val) => to_print += &val,
					Token::Bool(val) => to_print += &val.to_string(),
					Token::Number(num) => to_print += &num.to_string(),
					Token::FunctionDecl | Token::Codeblock(_) | Token::Ident(_) | Token::Group(_) => bail!(Error::ExpectedToken {
						expected: Token::String("Anything that can be displayed!".to_string()),
						found: arg
					}),
					Token::Native(_) => bail!(Error::ParserError),
					Token::LineEnd => break 
				}
				to_print += " "
			}

			let mut print_chars = to_print.chars();
			print_chars.next_back();

			to_print = print_chars.collect();

			println!("{}", to_print);
			return Ok(line);
		},
		"declare" => |_, mut line| {
			if let Some(Token::VarAccessor(name)) = line.next() {
				
			}
			return Ok(line)
		}
	};

	let mut tokens = tokens.into_iter().peekable();

	while tokens.peek().is_some() {
		match tokens.next() {
			Some(token) => match token {
				Token::FunctionDecl => {
					if let Some(Token::Ident(ident)) = tokens.next() {
						functions.insert(
							ident,
							Function {
								arguments: if let Some(Token::Group(inner)) = tokens.next() {
									inner
								} else {
									bail!(Error::ExpectedToken {
										expected: Token::Group(vec![Token::Ident("Your function here".to_string())]),
										found: tokens.next().unwrap()
									})
								},
								instructions: tokens.next().unwrap(),
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
		Some(_) => Ok(functions),
		None => bail!(Error::NoMain),
	}
}

pub fn run(functions: HashMap<String, Function>) -> anyErr<()> {
	// Naming this variable "main" will overwrite the main fn
	let shrimp_main = functions.get(&"main".to_string()).unwrap().clone();

	println!("{:?}", shrimp_main);

	match shrimp_main.instructions {
		Token::Codeblock(val) => {
			let mut instructions = val.into_iter().peekable();
			while let Some(val) = instructions.next() {
				match val {
					Token::Ident(name) => {
						match functions.get(&name) {
							Some(func) => {
								match func.instructions.clone() {
									Token::Native(fun) => match fun(func.arguments.clone(), instructions.clone()) {
										Ok(iterator) => {
											instructions = iterator
										},
										Err(err) => bail!(err)
									}
									any => bail!(Error::UnexpectedToken(any))
								}
							},
							None => bail!(Error::UnexpectedToken(Token::Ident(name)))
						}
					}
					x => bail!(Error::UnexpectedToken(x))
				}
			}
		}
		any => bail!(Error::UnexpectedToken(any))
	}

	Ok(())
}
