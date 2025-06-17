use std::collections::HashMap;
use std::sync::Mutex;

/// String interning for efficient identifier storage
/// 
/// This module provides string interning functionality to reduce memory usage
/// and improve performance when dealing with many repeated identifiers throughout
/// the compilation process.

lazy_static::lazy_static! {
    static ref STRING_INTERNER: Mutex<StringInterner> = Mutex::new(StringInterner::new());
}

/// Interned string handle
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternedString {
    id: u32,
}

impl InternedString {
    /// Get the string value for this interned string
    pub fn as_str(&self) -> String {
        STRING_INTERNER.lock().unwrap().get(*self)
    }
    
    /// Get the string slice for this interned string
    pub fn as_str_ref(&self) -> &'static str {
        // This is safe because interned strings are never deallocated
        // during the lifetime of the program
        unsafe {
            std::mem::transmute(
                STRING_INTERNER.lock().unwrap().get_ref(*self)
            )
        }
    }
    
    /// Get the internal ID of this interned string
    pub fn id(&self) -> u32 {
        self.id
    }
}

impl std::fmt::Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Internal string interner implementation
struct StringInterner {
    strings: Vec<String>,
    indices: HashMap<String, u32>,
}

impl StringInterner {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
            indices: HashMap::new(),
        }
    }
    
    fn intern(&mut self, s: &str) -> InternedString {
        if let Some(&id) = self.indices.get(s) {
            InternedString { id }
        } else {
            let id = self.strings.len() as u32;
            self.strings.push(s.to_owned());
            self.indices.insert(s.to_owned(), id);
            InternedString { id }
        }
    }
    
    fn get(&self, interned: InternedString) -> String {
        self.strings[interned.id as usize].clone()
    }
    
    fn get_ref(&self, interned: InternedString) -> &str {
        &self.strings[interned.id as usize]
    }
}

/// Intern a string and return an interned string handle
pub fn intern(s: &str) -> InternedString {
    STRING_INTERNER.lock().unwrap().intern(s)
}

/// Common interned strings for frequently used identifiers
pub struct CommonStrings {
    pub main: InternedString,
    pub self_: InternedString,
    pub super_: InternedString,
    pub crate_: InternedString,
    pub std: InternedString,
    pub core: InternedString,
    pub alloc: InternedString,
    pub new: InternedString,
    pub drop: InternedString,
    pub clone: InternedString,
    pub copy: InternedString,
    pub debug: InternedString,
    pub display: InternedString,
    pub eq: InternedString,
    pub ord: InternedString,
    pub hash: InternedString,
    pub default: InternedString,
    pub into: InternedString,
    pub from: InternedString,
    pub try_from: InternedString,
    pub try_into: InternedString,
    pub result: InternedString,
    pub option: InternedString,
    pub some: InternedString,
    pub none: InternedString,
    pub ok: InternedString,
    pub err: InternedString,
}

lazy_static::lazy_static! {
    pub static ref COMMON_STRINGS: CommonStrings = CommonStrings {
        main: intern("main"),
        self_: intern("self"),
        super_: intern("super"),
        crate_: intern("crate"),
        std: intern("std"),
        core: intern("core"),
        alloc: intern("alloc"),
        new: intern("new"),
        drop: intern("drop"),
        clone: intern("clone"),
        copy: intern("copy"),
        debug: intern("debug"),
        display: intern("display"),
        eq: intern("eq"),
        ord: intern("ord"),
        hash: intern("hash"),
        default: intern("default"),
        into: intern("into"),
        from: intern("from"),
        try_from: intern("try_from"),
        try_into: intern("try_into"),
        result: intern("result"),
        option: intern("option"),
        some: intern("some"),
        none: intern("none"),
        ok: intern("ok"),
        err: intern("err"),
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_intern_same_string() {
        let s1 = intern("hello");
        let s2 = intern("hello");
        assert_eq!(s1, s2);
        assert_eq!(s1.id(), s2.id());
    }
    
    #[test]
    fn test_intern_different_strings() {
        let s1 = intern("hello");
        let s2 = intern("world");
        assert_ne!(s1, s2);
        assert_ne!(s1.id(), s2.id());
    }
    
    #[test]
    fn test_string_retrieval() {
        let original = "test_string";
        let interned = intern(original);
        assert_eq!(interned.as_str(), original);
    }
    
    #[test]
    fn test_common_strings() {
        assert_eq!(COMMON_STRINGS.main.as_str(), "main");
        assert_eq!(COMMON_STRINGS.self_.as_str(), "self");
        assert_eq!(COMMON_STRINGS.result.as_str(), "result");
    }
}