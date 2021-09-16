use std::collections::HashMap;
use std::ops::Deref;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Origin {
	path: PathBuf,
	// File from which it was included
	parent: Option<PathBuf>,
}

impl Origin {
	pub const fn new(path: PathBuf, parent: Option<PathBuf>) -> Self {
		Self { path, parent }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Source {
	origin: Origin,
	content: String,
}

impl Source {
	pub const fn new(origin: Origin, content: String) -> Self {
		Self { origin, content }
	}
}

impl AsRef<str> for Source {
	fn as_ref(&self) -> &str {
		&self.content
	}
}

impl Deref for Source {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		&self.content
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(usize);

pub struct SourceMap {
	ids: HashMap<PathBuf, SourceId>,
	sources: Vec<Source>,
}

impl SourceMap {
	pub fn contains(&self, source: &Source) -> bool {
		self.ids.contains_key(&source.origin.path)
	}

	pub fn get(&self, id: SourceId) -> Option<&Source> {
		self.sources.get(id.0)
	}

	pub fn insert_or_get(&mut self, source: Source) -> SourceId {
		if let Some(id) = self.ids.get(&source.origin.path) {
			*id
		} else {
			let id = SourceId(self.sources.len());
			self.ids.insert(source.origin.path.clone(), id);
			self.sources.push(source);
			id
		}
	}
}
