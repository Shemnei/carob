#[cfg(test)]
mod tests {
	use std::time::SystemTime;

	use crate::lex::Lexer;
	use crate::token::TokenKind;

	#[test]
	fn local_file() {
		let path = std::env::var_os("CAROB_DEMO_PATH").unwrap();

		let content = std::fs::read_to_string(path).unwrap();

		let mut lexer = Lexer::new(content.as_bytes());

		let start = SystemTime::now();

		loop {
			let token = lexer.next_token();
			//println!("[{}] {}", token.span(), token.kind().name());

			if token.kind() == &TokenKind::Eof {
				break;
			}
		}

		println!("{:?}", start.elapsed().unwrap());
	}
}
