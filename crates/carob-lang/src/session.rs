use crate::diagnostic::{Diagnostic, Label};
use crate::source::Source;

pub type SingleLabel = Label<()>;
pub type SingleDiagnostic = Diagnostic<()>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SingleSession {
	pub(crate) source: Source,
	pub(crate) diagnostics: Vec<SingleDiagnostic>,
	pub(crate) failed: bool,
}

impl SingleSession {
	pub const fn new(source: Source) -> Self {
		Self { source, diagnostics: Vec::new(), failed: false }
	}

	pub fn add_diagnositic(&mut self, diagnostic: SingleDiagnostic) {
		self.diagnostics.push(diagnostic)
	}

	pub fn set_failed(&mut self) {
		self.failed = true;
	}
}
