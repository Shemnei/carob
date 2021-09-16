use std::borrow::Cow;
use std::cmp::Ordering;

use crate::span::ByteSpan;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
	Bug,
	Error,
	Warning,
	Note,
	Help,
}

impl Severity {
	pub const fn is_hard_error(&self) -> bool {
		matches!(self, Self::Bug | Self::Error)
	}

	const fn to_cmp_value(self) -> u8 {
		match self {
			Severity::Bug => 5,
			Severity::Error => 4,
			Severity::Warning => 3,
			Severity::Note => 2,
			Severity::Help => 1,
		}
	}
}

impl PartialOrd<Self> for Severity {
	fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
		u8::partial_cmp(&self.to_cmp_value(), &rhs.to_cmp_value())
	}
}

impl Ord for Severity {
	fn cmp(&self, rhs: &Self) -> Ordering {
		self.partial_cmp(rhs).expect("Failed to cmp with Ord")
	}
}

impl std::fmt::Display for Severity {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let s = match self {
			Severity::Bug => "bug",
			Severity::Error => "error",
			Severity::Warning => "warning",
			Severity::Note => "note",
			Severity::Help => "help",
		};

		f.write_str(s)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Code {}

impl Code {
	pub fn message(&self) -> Option<&'static str> {
		todo!()
	}

	pub fn url(&self) -> Option<&'static str> {
		todo!()
	}
}

impl std::fmt::Display for Code {
	fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		todo!()
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LabelKind {
	Primary,
	Secondary,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label<ID> {
	pub(crate) kind: LabelKind,
	pub(crate) source_id: ID,
	pub(crate) span: ByteSpan,
	pub(crate) message: Option<Cow<'static, str>>,
}

impl<ID> Label<ID> {
	pub const fn new(
		kind: LabelKind,
		source_id: ID,
		span: ByteSpan,
		message: Option<Cow<'static, str>>,
	) -> Self {
		Self { kind, source_id, span, message }
	}

	pub fn primary<S: Into<ByteSpan>>(source_id: ID, span: S) -> Self {
		Self::new(LabelKind::Primary, source_id, span.into(), None)
	}

	pub fn secondary<S: Into<ByteSpan>>(source_id: ID, span: S) -> Self {
		Self::new(LabelKind::Secondary, source_id, span.into(), None)
	}

	pub fn with_message<M: Into<Cow<'static, str>>>(mut self, message: M) -> Self {
		self.message = Some(message.into());
		self
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic<ID> {
	pub(crate) severity: Severity,
	pub(crate) code: Option<Code>,
	pub(crate) message: Option<Cow<'static, str>>,
	pub(crate) labels: Vec<Label<ID>>,
	pub(crate) notes: Vec<Cow<'static, str>>,
	pub(crate) children: Vec<Self>,
}

impl<ID> Diagnostic<ID> {
	pub fn new(
		severity: Severity,
		code: Option<Code>,
		message: Option<Cow<'static, str>>,
		labels: Vec<Label<ID>>,
		notes: Vec<Cow<'static, str>>,
		children: Vec<Self>,
	) -> Self {
		Self { severity, code, message, labels, notes, children }
	}

	pub fn bug() -> Self {
		Self::new(Severity::Bug, None, None, Vec::new(), Vec::new(), Vec::new())
	}

	pub fn error() -> Self {
		Self::new(Severity::Error, None, None, Vec::new(), Vec::new(), Vec::new())
	}

	pub fn warning() -> Self {
		Self::new(Severity::Warning, None, None, Vec::new(), Vec::new(), Vec::new())
	}

	pub fn note() -> Self {
		Self::new(Severity::Note, None, None, Vec::new(), Vec::new(), Vec::new())
	}

	pub fn help() -> Self {
		Self::new(Severity::Help, None, None, Vec::new(), Vec::new(), Vec::new())
	}

	pub const fn with_code(mut self, code: Code) -> Self {
		self.code = Some(code);
		self
	}

	pub fn with_message<M: Into<Cow<'static, str>>>(mut self, message: M) -> Self {
		self.message = Some(message.into());
		self
	}

	pub fn with_label(mut self, label: Label<ID>) -> Self {
		self.labels.push(label);
		self
	}

	pub fn with_labels(mut self, labels: Vec<Label<ID>>) -> Self {
		self.labels.extend(labels.into_iter());
		self
	}

	pub fn with_note<N: Into<Cow<'static, str>>>(mut self, note: N) -> Self {
		self.notes.push(note.into());
		self
	}

	pub fn with_notes<N: Into<Cow<'static, str>>>(mut self, notes: Vec<N>) -> Self {
		self.notes.extend(notes.into_iter().map(|n| n.into()));
		self
	}

	pub fn with_child(mut self, child: Self) -> Self {
		self.children.push(child);
		self
	}

	pub fn with_children(mut self, children: Vec<Self>) -> Self {
		self.children.extend(children.into_iter());
		self
	}

	pub fn expand_on_last_label<F>(mut self, mut f: F) -> Self
	where
		ID: Clone,
		F: FnMut(Label<ID>) -> Label<ID>,
	{
		let last_label = self.labels.last().expect("No label present on diagnostic");

		let cloned = Label::secondary(last_label.source_id.clone(), last_label.span);

		self.labels.push(f(cloned));

		self
	}
}

mod fmt {}
