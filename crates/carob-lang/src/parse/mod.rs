mod cursor;

use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;

use self::cursor::Cursor;
use crate::date::UncheckedDate;
use crate::diagnostic::Diagnostic;
use crate::directive::{Directive, DirectiveKind};
use crate::keyword::Keyword;
use crate::metadata::Tag;
use crate::session::{SingleDiagnostic, SingleLabel, SingleSession};
use crate::span::{ByteSpan, Span as _};
use crate::token::{LiteralKind, Token, TokenKind};

type Result<T, E = SingleDiagnostic> = std::result::Result<T, E>;

pub struct Parser<'a> {
	session: SingleSession,
	cursor: Cursor<'a>,
}

impl<'a> Parser<'a> {
	pub const fn new(session: SingleSession, tokens: &'a [Token]) -> Self {
		Self { session, cursor: Cursor::new(tokens) }
	}

	pub fn next_directive(&mut self) -> Option<Directive> {
		loop {
			match self.parse_top_level_directive()? {
				Ok(directive) => return Some(directive),
				Err(diagnostic) => {
					self.emit_diagnostic(diagnostic);
					self.recover_state();
				}
			}
		}
	}

	// Allowed: Destructors can not be const
	#[allow(clippy::missing_const_for_fn)]
	pub fn into_session(self) -> SingleSession {
		self.session
	}

	fn emit_diagnostic(&mut self, diagnostic: SingleDiagnostic) {
		if diagnostic.severity.is_hard_error() {
			self.session.set_failed();
		}

		self.session.add_diagnositic(diagnostic);
	}

	// Skips to the first token after a eol
	fn skip_to_sol(&mut self) {
		if self.cursor.pos() > 0 {
			// Skip to next eol token
			self.cursor.consume_until(|Token { kind, .. }| kind == &TokenKind::Eol);
			// Skip eol tokens
			self.cursor.consume_while(|Token { kind, .. }| kind == &TokenKind::Eol);
		}
	}

	fn recover_state(&mut self) {
		// Fix state by first going to the start new line
		self.skip_to_sol();

		/*
		// Check if next token could be the start of a directive
		while let Some(Token { span, kind }) = self.cursor.first() {
			// TODO: also allow negative years
			if matches!(*kind, TokenKind::Literal { kind: LiteralKind::Integer })
				// ISO-8601 prescibes that years must be atleast 4 digits
				&& span.len() >= 4
			{
				// Probably found the start of a date
				// TODO: Try parse it as date and only then break?
				break;
			}

			if matches!(kind, TokenKind::Ident) {
				if let Some(keyword) = Keyword::from_str_opt(span.index(&self.session.source)) {
					if matches!(
						keyword,
						Keyword::PushTag
							| Keyword::PopTag | Keyword::Option
							| Keyword::Plugin | Keyword::Include
					) {
						break;
					}
				}
			}

			// No valid start of directive; Goto next start of line
			self.skip_to_sol();
		}
		*/
	}

	fn try_parse<P, E>(&self, token: &Token, expected: &str) -> Result<P>
	where
		P: FromStr<Err = E>,
		E: fmt::Display,
	{
		let s = token.span.index(&self.session.source);

		<P>::from_str(s).map_err(|err| {
			SingleDiagnostic::error()
				.with_message(format!(
					"failed to parse token `{}` as `{}`",
					token.kind.kind_str(),
					expected
				))
				.with_label(SingleLabel::primary((), *token.span()))
				.with_note(err.to_string())
		})
	}

	fn opt(&mut self, kind: TokenKind) -> Option<Token> {
		let first = self.cursor.first_or_eof();

		if first.kind == kind {
			// TODO: move into cursor somehow
			// If token is owned don't consume it as it is not actually in the tokens input
			let advance = matches!(first, Cow::Borrowed(_));

			let token = first.into_owned();

			if advance {
				unsafe {
					self.cursor.advance_unchecked(1);
				}
			}

			Some(token)
		} else {
			None
		}
	}

	fn opt_any(&mut self, kinds: &[TokenKind]) -> Option<Token> {
		let first = self.cursor.first_or_eof();

		let matches = kinds.iter().any(|kind| kind == &first.kind);

		if matches {
			// TODO: move into cursor somehow
			// If token is owned don't consume it as it is not actually in the tokens input
			let advance = matches!(first, Cow::Borrowed(_));

			let token = first.into_owned();

			if advance {
				unsafe {
					self.cursor.advance_unchecked(1);
				}
			}

			Some(token)
		} else {
			None
		}
	}

	fn opt_string(&mut self) -> Option<(ByteSpan, String)> {
		let Token { span, .. } = self.opt_any(&[
			TokenKind::literal(LiteralKind::String { terminated: true }),
			TokenKind::literal(LiteralKind::String { terminated: false }),
		])?;

		// TODO: decide what to do with unclosed strings

		Some((span, span.index(&self.session.source).to_owned()))
	}

	fn expect(&mut self, kind: TokenKind) -> Result<Token> {
		let token = self.opt(kind);

		if let Some(token) = token {
			Ok(token)
		} else {
			let first = self.cursor.first_or_eof();

			let diagnostic = SingleDiagnostic::error()
				.with_message(format!(
					"expected token `{}` but found `{}`",
					kind.kind_str(),
					first.kind.kind_str()
				))
				.with_label(SingleLabel::primary((), *first.span()));

			Err(diagnostic)
		}
	}

	fn expect_any(&mut self, kinds: &[TokenKind]) -> Result<Token> {
		let token = self.opt_any(kinds);

		if let Some(token) = token {
			Ok(token)
		} else {
			let first = self.cursor.first_or_eof();

			// TODO: optimize allocations
			let kinds_str = kinds.iter().map(|kind| kind.kind_str()).collect::<Vec<_>>().join(", ");

			let diagnostic = SingleDiagnostic::error()
				.with_message(format!(
					"expected one of the following tokens `[{}]` but found `{}`",
					&kinds_str,
					first.kind.kind_str()
				))
				.with_label(SingleLabel::primary((), *first.span()));

			Err(diagnostic)
		}
	}

	fn expect_string(&mut self) -> Result<(ByteSpan, String)> {
		let Token { span, .. } = self.expect_any(&[
			TokenKind::literal(LiteralKind::String { terminated: true }),
			TokenKind::literal(LiteralKind::String { terminated: false }),
		])?;

		// TODO: decide what to do with unclosed strings

		Ok((span, span.index(&self.session.source).to_owned()))
	}

	fn expect_date(&mut self) -> Result<(ByteSpan, UncheckedDate)> {
		// TODO: check if whitespaces between tokens

		fn date_diagnostic(
			primary_span: ByteSpan,
			secondary_span: ByteSpan,
			action: &'static str,
			note: &'static str,
		) -> SingleDiagnostic {
			SingleDiagnostic::error()
				.with_message("found invalid date")
				.with_labels(vec![
					SingleLabel::primary((), primary_span).with_message(action),
					SingleLabel::secondary((), secondary_span)
						.with_message("while trying to parse this date"),
				])
				.with_note(note)
		}

		fn extend_date_diagnostic(
			diagnostic: SingleDiagnostic,
			action: &'static str,
			date_span: Option<ByteSpan>,
		) -> SingleDiagnostic {
			// TODO: better
			let primary_span = diagnostic.labels.last().unwrap().span;

			diagnostic.with_labels(vec![
				SingleLabel::secondary((), primary_span).with_message(action),
				SingleLabel::secondary((), date_span.unwrap_or(primary_span))
					.with_message("while trying to parse this date"),
			])
		}

		let sign_token = self.opt_any(&[TokenKind::Hyphen, TokenKind::Plus]);

		let year_token =
			self.expect(TokenKind::literal(LiteralKind::Integer)).map_err(|diagnostic| {
				extend_date_diagnostic(
					diagnostic,
					"while trying to parse this as year",
					sign_token.map(|token| *token.span()),
				)
			})?;

		let mut date_span = if let Some(sign_token) = sign_token {
			sign_token.span().union(year_token.span())
		} else {
			*year_token.span()
		};

		let year = if year_token.span.len() >= 4 {
			let year: i16 = self.try_parse(&year_token, "i16").map_err(|diagnostic| {
				extend_date_diagnostic(
					diagnostic,
					"while trying to parse this as year",
					Some(date_span),
				)
			})?;

			if matches!(sign_token, Some(Token { kind: TokenKind::Hyphen, .. })) {
				-year
			} else {
				year
			}
		} else {
			return Err(date_diagnostic(
				*year_token.span(),
				date_span,
				"while trying to parse this as year",
				"A valid year must be 4 digits long and can optionally prefixed with a sign \
				 (`+`/`-`)",
			));
		};

		let separator_token =
			self.expect_any(&[TokenKind::Hyphen, TokenKind::Slash]).map_err(|diagnostic| {
				extend_date_diagnostic(
					diagnostic,
					"while trying to parse this as date separator",
					Some(date_span),
				)
				.with_note("The date separator can either be `-` or `/`")
				.with_note("The first found separator dictates the second one")
			})?;

		date_span = date_span.union(separator_token.span());

		let month_token =
			self.expect(TokenKind::literal(LiteralKind::Integer)).map_err(|diagnostic| {
				extend_date_diagnostic(
					diagnostic,
					"while trying to parse this as month",
					Some(date_span),
				)
			})?;

		date_span = date_span.union(month_token.span());

		let month = if month_token.span.len() == 2 {
			self.try_parse(&month_token, "u8").map_err(|diagnostic| {
				extend_date_diagnostic(
					diagnostic,
					"while trying to parse this as month",
					Some(date_span),
				)
			})?
		} else {
			return Err(date_diagnostic(
				*month_token.span(),
				date_span,
				"while trying to parse this as month",
				"A valid month must be 2 digits long",
			));
		};

		let separator_token = self.expect(separator_token.kind).map_err(|diagnostic| {
			extend_date_diagnostic(
				diagnostic,
				"while trying to parse this as date separator",
				Some(date_span),
			)
			.with_note("The date separator can either be `-` or `/`")
			.with_note("The first found separator dictates the second one")
		})?;

		date_span = date_span.union(separator_token.span());

		let day_token =
			self.expect(TokenKind::literal(LiteralKind::Integer)).map_err(|diagnostic| {
				extend_date_diagnostic(
					diagnostic,
					"while trying to parse this as day",
					Some(date_span),
				)
			})?;

		date_span = date_span.union(day_token.span());

		let day = if day_token.span.len() == 2 {
			self.try_parse(&day_token, "u8").map_err(|diagnostic| {
				extend_date_diagnostic(
					diagnostic,
					"while trying to parse this as day",
					Some(date_span),
				)
			})?
		} else {
			return Err(date_diagnostic(
				*day_token.span(),
				date_span,
				"while trying to parse this as day",
				"A valid day must be 2 digits long",
			));
		};

		Ok((date_span, UncheckedDate::from_ymd(year, month, day)))
	}

	fn parse_top_level_directive(&mut self) -> Option<Result<Directive>> {
		// TLDS:
		//		- Date
		//		- push/poptag
		//		- option
		//		- plugin
		//		- include

		// TODO: move while condition into cursor
		// TODO: skip eol tokens
		while !matches!(self.cursor.first(), None | Some(Token { kind: TokenKind::Eof, .. })) {
			let cursor_cp = self.cursor;

			// TODO: decide when to add diagnostic (e.g. if date was malformed?)

			if let Ok(date) = self.expect_date() {
				return Some(self.parse_date_directive(date));
			} else {
				self.cursor = cursor_cp;
			}

			let Token { span, kind } = self.cursor.consume()?;

			if matches!(kind, TokenKind::Ident) {
				if let Some(keyword) = Keyword::from_str_opt(span.index(&self.session.source)) {
					if matches!(
						keyword,
						Keyword::PushTag
							| Keyword::PopTag | Keyword::Option
							| Keyword::Plugin | Keyword::Include
					) {
						return Some(self.parse_keyword_directive((span, keyword)));
					}
				}
			}

			// No valid start of directive; Goto next start of line
			self.skip_to_sol();
		}

		None
	}

	// TODO: include span of date to reference it in diagnostics
	fn parse_date_directive(
		&mut self,
		(_date_span, _date): (ByteSpan, UncheckedDate),
	) -> Result<Directive> {
		use crate::pos::BytePos;

		let kind = DirectiveKind::PushTag { tag: Tag(String::from("a")) };
		Ok(Directive::new(ByteSpan::new(BytePos(0), BytePos(0)), kind))
	}

	fn parse_keyword_directive(
		&mut self,
		(keyword_span, keyword): (ByteSpan, Keyword),
	) -> Result<Directive> {
		match keyword {
			Keyword::PushTag => self.parse_pushtag(keyword_span, keyword),
			Keyword::PopTag => self.parse_poptag(keyword_span, keyword),
			Keyword::Option => self.parse_option(keyword_span, keyword),
			Keyword::Plugin => self.parse_plugin(keyword_span, keyword),
			Keyword::Include => self.parse_include(keyword_span, keyword),
			// TODO: better error / no error?
			_ => Err(Diagnostic::error()),
		}
	}

	fn parse_pushtag(&mut self, keyword_span: ByteSpan, keyword: Keyword) -> Result<Directive> {
		debug_assert_eq!(keyword, Keyword::PushTag);

		let mut directive_span = keyword_span;

		let (tag_span, tag) = self.expect_string().map_err(|diagnostic| {
			diagnostic.with_label(
				SingleLabel::secondary((), directive_span)
					.with_message("while trying to parse this pushtag directive"),
			)
		})?;

		directive_span = directive_span.union(&tag_span);

		let tag = Tag(tag);

		Ok(Directive::new(directive_span, DirectiveKind::PushTag { tag }))
	}

	fn parse_poptag(&mut self, keyword_span: ByteSpan, keyword: Keyword) -> Result<Directive> {
		debug_assert_eq!(keyword, Keyword::PopTag);

		let mut directive_span = keyword_span;

		let (tag_span, tag) = self.expect_string().map_err(|diagnostic| {
			diagnostic.with_label(
				SingleLabel::secondary((), directive_span)
					.with_message("while trying to parse this poptag directive"),
			)
		})?;

		directive_span = directive_span.union(&tag_span);

		let tag = Tag(tag);

		Ok(Directive::new(directive_span, DirectiveKind::PopTag { tag }))
	}

	fn parse_option(&mut self, keyword_span: ByteSpan, keyword: Keyword) -> Result<Directive> {
		debug_assert_eq!(keyword, Keyword::Option);

		let mut directive_span = keyword_span;

		let (name_span, name) = self.expect_string().map_err(|diagnostic| {
			diagnostic
				.expand_on_last_label(|label| {
					label.with_message("while trying to parse this as option name")
				})
				.with_label(
					SingleLabel::secondary((), directive_span)
						.with_message("while trying to parse this option directive"),
				)
		})?;

		directive_span = directive_span.union(&name_span);

		let (value_span, value) = self.expect_string().map_err(|diagnostic| {
			diagnostic
				.expand_on_last_label(|label| {
					label.with_message("while trying to parse this as option value")
				})
				.with_label(
					SingleLabel::secondary((), directive_span)
						.with_message("while trying to parse this option directive"),
				)
		})?;

		directive_span = directive_span.union(&value_span);

		Ok(Directive::new(directive_span, DirectiveKind::Option { name, value }))
	}

	fn parse_plugin(&mut self, keyword_span: ByteSpan, keyword: Keyword) -> Result<Directive> {
		debug_assert_eq!(keyword, Keyword::Plugin);
		let mut directive_span = keyword_span;

		let (name_span, name) = self.expect_string().map_err(|diagnostic| {
			diagnostic
				.expand_on_last_label(|label| {
					label.with_message("while trying to parse this as plugin name")
				})
				.with_label(
					SingleLabel::secondary((), directive_span)
						.with_message("while trying to parse this plugin directive"),
				)
		})?;

		directive_span = directive_span.union(&name_span);

		let configuration = if let Some((configuration_span, configuration)) = self.opt_string() {
			directive_span = directive_span.union(&configuration_span);
			Some(configuration)
		} else {
			None
		};

		Ok(Directive::new(directive_span, DirectiveKind::Plugin { name, configuration }))
	}

	fn parse_include(&mut self, keyword_span: ByteSpan, keyword: Keyword) -> Result<Directive> {
		debug_assert_eq!(keyword, Keyword::Include);

		let mut directive_span = keyword_span;

		let (path_span, path) = self.expect_string().map_err(|diagnostic| {
			diagnostic.with_label(
				SingleLabel::secondary((), directive_span)
					.with_message("while trying to parse this include directive"),
			)
		})?;

		directive_span = directive_span.union(&path_span);

		Ok(Directive::new(directive_span, DirectiveKind::Include { path }))
	}
}

#[cfg(test)]
mod tests {
	use std::path::PathBuf;

	use super::*;
	use crate::lex::Lexer;
	use crate::source::{Origin, Source};

	#[test]
	fn parse_date() {
		let content = "2021-01-01";
		let mut lexer = Lexer::new(&content);

		let mut tokens = Vec::new();

		loop {
			let token = lexer.next_token();
			println!("[{}/`{}`] {:?}", token.span(), token.span().index(content), token.kind());

			if token.kind() == &TokenKind::Eof {
				tokens.push(token);
				break;
			}

			tokens.push(token);
		}

		let mut parser = Parser::new(
			SingleSession::new(Source::new(Origin::new(PathBuf::new(), None), content.to_owned())),
			&tokens,
		);

		let diag = parser.expect_date();
		println!("{:?}", diag);
	}
}
