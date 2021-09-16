#[cfg(test)]
mod tests {
	use std::ffi::OsString;
	use std::time::SystemTime;

	use crate::lex::Lexer;
	use crate::parse::Parser;
	use crate::session::SingleSession;
	use crate::source::{Origin, Source};
	use crate::token::TokenKind;

	#[test]
	fn local_file_lex() {
		let path = std::env::var_os("CAROB_DEMO_PATH")
			.unwrap_or_else(|| OsString::from("assets/example.bean"));

		let content = std::fs::read_to_string(path).unwrap();

		let mut lexer = Lexer::new(&content);

		let start = SystemTime::now();

		loop {
			let token = lexer.next_token();
			//println!("[{}] {:?}", token.span(), token.kind());

			if token.kind() == &TokenKind::Eof {
				break;
			}
		}

		println!("{:?}", start.elapsed().unwrap());
	}

	#[test]
	fn local_file_parse() {
		let path = std::env::var_os("CAROB_DEMO_PATH")
			.unwrap_or_else(|| OsString::from("assets/example.bean"));

		let content = std::fs::read_to_string(&path).unwrap();

		let mut lexer = Lexer::new(&content);

		let start = SystemTime::now();

		let tokens = std::iter::from_fn(|| {
			let token = lexer.next_token();

			if token.kind() == &TokenKind::Eof {
				None
			} else {
				Some(token)
			}
		})
		.collect::<Vec<_>>();

		let lex_elapsed = start.elapsed().unwrap();

		let source = Source::new(Origin::new(path.into(), None), content);
		let mut parser = Parser::new(SingleSession::new(source), &tokens);

		let start = SystemTime::now();

		while let Some(_directive) = parser.next_directive() {
			//println!("{:#?}", directive);
		}

		let parse_elapsed = start.elapsed().unwrap();

		let session = parser.into_session();

		for diagnostic in session.diagnostics {
			println!("{:#?}", diagnostic);
		}

		println!("Lex: {:?}", lex_elapsed);
		println!("Parse: {:?}", parse_elapsed);
	}
}
