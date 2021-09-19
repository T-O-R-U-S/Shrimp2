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
	#[error("Unknown variable accessed")]
	UnknownVar,
	#[error("Incorrect amount of arguments provided")]
	MalformedArgs
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
	Native(fn(HashMap<String, Token>, std::iter::Peekable<std::vec::IntoIter<Token>>) -> anyErr<(HashMap<String, Token>, std::iter::Peekable<std::vec::IntoIter<Token>>)>),
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

				while let Some('a'..='z' | '0'..='9' | '_') = line.current_char {
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
						
						if line.current_char == Some(current_quot) || line.current_char == Some('\\') {
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
				while let Some('a'..='z' | '0'..='9' | '_') = line.current_char {
					word.push(line.current_char.unwrap());
					line.advance()
				}
				line.retreat();
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
				line.retreat();

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
							']' => {
								if brack_count > 1 {
									word.push(']')
								}
								brack_count -= 1;
							}
							'[' => {
								brack_count += 1;
								word.push('[')
							}
							_ => word.push(chr),
						},
						None => bail!(Error::UnclosedDelimiter),
					}
					line.advance()
				}
				line.retreat();

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
				line.retreat();

				let n_tokens = make_tokens(Line::new(word, line.line_num));

				tokens.push(Token::Codeblock(n_tokens?))
			}

			'@' => tokens.push(Token::FunctionDecl),

			'/' => {
				line.advance();
				while line.current_char != Some('/') {
					if line.current_char.is_none() {
						bail!(Error::UnexpectedEOL)
					}
					line.advance();
				}
				line.advance()
			}

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
pub struct FunctionArgs {
	arg_name: Vec<Token>,
	args: Vec<Token>
}

impl FunctionArgs {
	pub fn validate(self) -> anyErr<Self> {
		if self.arg_name.len() > self.args.len() || self.arg_name.len() < self.args.len() {
			bail!(Error::MalformedArgs)
		} else {
			Ok(self)
		}
	}
	pub fn new() -> Self {
		Self {
			arg_name: vec![],
			args: vec![]
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Function {
	arguments: FunctionArgs,
	instructions: Token,
}



macro_rules! insert_many {
	{funct!$hashmap: expr,$($key:expr => $val:expr),*} => {
		$(
			$hashmap.insert($key.to_string(), Function { arguments: FunctionArgs::new(), instructions: Token::Native($val) });
		)*
	};
	{$hashmap: expr,$($key:expr => $val:expr),*} => {
		$(
			$hashmap.insert($key.to_string(), $val)
		)*
	};
}

macro_rules! try_or_bail {
	($option: expr;$error:expr) => {
		match $option {
			Some(var) => var,
			None => bail!($error)
		}
	};
	($result: expr) => {
		match $result {
			Ok(var) => var,
			Err(val) => bail!(val)
		}
	};
	(m!$token:expr;$match_with:ident) => {
		match $token {
			$match_with(val) => val,Z
			a => bail!(Error::UnexpectedToken(a))
		}
	};
	(mo!$token:expr;$match_with:ident) => {
		match $token {
			Some($match_with(val)) => val,
			None => bail!(Error::UnexpectedEOL)
		}
	}
}

pub fn funcs(tokens: Vec<Token>) -> anyErr<HashMap<String, Function>> {
	let mut functions = HashMap::new();

	insert_many! {
		funct!functions,
		"print" => |vars, mut line| {
			let mut to_print = String::new();

			while let Some(arg) = line.next() {
				match arg {
					Token::VarAccessor(var) => to_print += &format!("{}", match vars.get(&var).clone() {
						Some(any) => any,
						None => bail!(Error::UnknownVar)
					}),
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
			return Ok((vars, line));
		},
		"declare" => |mut vars, mut line| {
			if let Some(Token::VarAccessor(name)) = line.next() {
				let var_token = match line.next() {
					Some(Token::VarAccessor(any)) => vars.get(&any).unwrap().clone(),
					Some(thing) => thing,
					None => bail!(Error::UnexpectedEOL)
				};
				vars.insert(name, var_token);
			}
			
			match line.next() {
				Some(Token::LineEnd) => Ok((vars, line)),
				Some(any) => bail!(Error::UnexpectedToken(any)),
				None => bail!(Error::UnexpectedEOL)
			}
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
								arguments:
								match tokens.next() {
									Some(Token::Group(inner)) => FunctionArgs { arg_name: inner, args: vec![] },
									Some(token) => bail!(Error::UnexpectedToken(token)),
									None => bail!(Error::UnexpectedEOL)
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

	try_or_bail!(functions.get(&"main".to_string()); Error::NoMain);

	Ok(functions)
}

pub fn execute(func: Function, mut variables: HashMap<String,Token>, mut functions: HashMap<String, Function>) -> anyErr<(HashMap<String, Token>, HashMap<String, Function>, Option<Token>)> {
	let mut output = None;
	match func.instructions {
		Token::Codeblock(val) => {
			let mut instructions = val.into_iter().peekable();
			while let Some(val) = instructions.next() {
				match val {
					Token::Ident(name) => {
						match functions.get(&name) {
							Some(func) => {
								match func.instructions.clone() {
									Token::Native(fun) => match fun(variables.clone(), instructions.clone()) {
										Ok((vars, iterator)) => {
											instructions = iterator;
											variables = vars;
										},
										Err(err) => bail!(err)
									}
									Token::Codeblock(_) => {
										let mut temp_vars = vec![];

										let mut args = func.arguments.arg_name.clone().iter();

										for i in func.arguments.arg_name.clone() {
											temp_vars.push(i.to_string().clone());
											variables.insert(i.to_string(), match instructions.next() {
												Some(Token::LineEnd) => bail!(Error::UnexpectedEOL),
												Some(thing) => thing,
												None => bail!(Error::UnexpectedEOL)
											});
										}

										instructions.next();
										if temp_vars.len() != func.arguments.arg_name.len() {
											bail!(Error::MalformedArgs)
										}
										instructions.next();
										let out = try_or_bail!(execute(func.clone(), variables.clone(), functions.clone()));
										// TODO: Once destructuring gets stabilized, use it here.
										variables = out.0;
										functions = out.1;
										output = out.2;
										for i in temp_vars {
											variables.remove(&i);
										}
									},
									any => bail!(Error::UnexpectedToken(any))
								}
							},
							None => bail!(Error::UnexpectedEOL)
						}
					}
					x => bail!(Error::UnexpectedToken(x))
				}
			}
		}
		any => bail!(Error::UnexpectedToken(any))
	}

	Ok((variables, functions, output))
}

pub fn run(functions: HashMap<String, Function>) -> anyErr<()> {
	let mut variables = HashMap::<String, Token>::new();

	// Naming this variable "main" will overwrite the main fn
	let shrimp_main = functions.get(&"main".to_string()).unwrap().clone();

	println!("{:?}", shrimp_main);

	match execute(shrimp_main, variables, functions) {
		Ok(_) => Ok(()),
		Err(any) => bail!(any)
	}

	/*match shrimp_main.instructions {
		Token::Codeblock(val) => {
			let mut instructions = val.into_iter().peekable();
			while let Some(val) = instructions.next() {
				match val {
					Token::Ident(name) => {
						match functions.get(&name) {
							Some(func) => {
								match func.instructions.clone() {
									Token::Native(fun) => match fun(variables.clone(), instructions.clone()) {
										Ok((vars, iterator)) => {
											instructions = iterator;
											variables = vars;
										},
										Err(err) => bail!(err)
									}
									Token::Codeblock(code) => {
										
									}
									any => bail!(Error::UnexpectedToken(any))
								}
							},
							None => bail!(Error::UnexpectedEOL)
						}
					}
					x => bail!(Error::UnexpectedToken(x))
				}
			}
		}
		any => bail!(Error::UnexpectedToken(any))
	}

	Ok(())*/
}
