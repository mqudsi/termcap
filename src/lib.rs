use std::io::{Error, ErrorKind, Result as IoResult};
use std::path::Path;

pub trait CapabilityResult {}

impl CapabilityResult for bool {}
impl CapabilityResult for i32 {}
impl CapabilityResult for String {}

pub trait Capability {
    type Result: CapabilityResult;

    fn code(&self) -> Code;
    fn name(&self) -> String;
    fn description(&self) -> String;
}

/// The core interface for querying the system's termcap database for capabilities of a terminal.
pub struct Termcap {
    database: Database,
}

impl Termcap {
    /// Loads the system termcap database.
    pub fn load() -> IoResult<Termcap> {
        /// Possible paths to the termcap file
        const TERMCAP_PATHS: &[&str] = &["/usr/share/misc/termcap", "/etc/termcap"];

        let path = TERMCAP_PATHS
            .iter()
            .find(|path| Path::new(path).is_file())
            .ok_or(Error::from(ErrorKind::NotFound))?;

        Ok(Self {
            database: Database::load(Path::new(path))?,
        })
    }

    /// Loads the specified termcap file.
    pub fn load_from_path(path: &Path) -> IoResult<Termcap> {
        Ok(Self {
            database: Database::load(path)?,
        })
    }

    /// Gets the matching termcap(5) entry for the current value of `$TERM`.
    ///
    /// Returns `None` if `$TERM` is not set or if there is no entry in the termcap database for the
    /// current `$TERM` value. See [get()](Self::get()) to look up by terminal name without relying
    /// on the presence of any particular environment value.
    pub fn get_env<'a>(&'a self) -> Option<Capabilities<'a>> {
        // Terminal names can only be ASCII, so we can jump to NotFound if $TERM is non-ASCII.
        let term = std::env::var("TERM").ok()?;

        Some(Capabilities {
            database: &self.database,
            entries: self.database.lookup(&term)?,
        })
    }

    /// Gets the matching `termcap(5)` entry for the terminal with a name or code matching `term`.
    ///
    /// Returns `None` if there is no match for this terminal name/code in the termcap(5) database
    /// or if the termcap database could not be found.
    pub fn get<'a>(&'a self, term: &str) -> Option<Capabilities<'a>> {
        Some(Capabilities {
            database: &self.database,
            entries: self.database.lookup(term)?,
        })
    }
}

/// Represents the capabilities of a particular terminal, as loaded from the termcap database.
///
/// Can be used to iterate over all capabilities or to query for a specific capability.
pub struct Capabilities<'a> {
    /// The loaded termcap database.
    database: &'a Database,
    /// The entries returned by [`Database::lookup()`].
    entries: &'a [Entry],
}

impl Capabilities<'_> {
    /// Queries for the presence and value of a named capability `capability`.
    pub fn query<C: Capability>(&self, capability: C) -> Option<C::Result> {
        for cap in self.entries {
            if cap.code == capability.code() {
                // Check for explicitly disavowed capabilities
                if matches!(cap.value, Field::Bool(false)) {
                    return None;
                }
            }
        }

        todo!()
    }

    #[allow(unused)]
    /// Returns an iterator of all capabilities matching the looked up terminal.
    fn iter<'a>(&'a self) -> CapabilitiesIterator<'a> {
        CapabilitiesIterator {
            database: self.database,
            entries: self.entries.iter(),
        }
    }
}

/// Iterates over the capabilities of a terminal, as recorded in the termcap database.
struct CapabilitiesIterator<'a> {
    database: &'a Database,
    entries: std::slice::Iter<'a, Entry>,
}

impl<'a> Iterator for CapabilitiesIterator<'a> {
    type Item = &'a Entry;

    fn next(&mut self) -> Option<Self::Item> {
        let entry = self.entries.next()?;
        if entry.code == Code::new([b't', b'c']) {
            // Inherits from another entry
            let parent_term = match &entry.value {
                Field::String(term) => term,
                _ => unreachable!(),
            };
            let entries = self.database.lookup(&parent_term)?;
            self.entries = entries.into_iter();
            self.next()
        } else {
            Some(entry)
        }
    }
}

/// The identifier(s) associated with a termcap entry, divided into primary and secondary.
///
/// e.g. for the entry `vt420|vt420-24|dec-vt420|DEC vt420 7 bit controls 80x24 autowrap`, the
/// `key` here would be `vt420`, while each of `vt420-24`, `dec-vt420`, and the last (being the
/// long descriptor) `DEC vt420 7 bit controls 80x24 autowrap` would each be aliases.
///
/// We look up entries via binary search on the `key` because that's the most common entry, then
/// fall back to traversal for full-text search among the `aliases`.
struct TermId {
    /// The primary (first/left-most) name associated with an entry in the termcap file.
    key: String,
    aliases: Vec<String>,
}

impl std::fmt::Display for TermId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} ({})", &self.key, self.aliases.join(", ")))
    }
}

/// A field matching a capability. Each field is either a boolean, numeric, or string value.
enum Field {
    Bool(bool),
    Number(i32),
    String(String),
}

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Field::Bool(value) => f.write_str(&value.to_string()),
            Field::Number(value) => f.write_str(&value.to_string()),
            Field::String(value) => f.write_str(&value),
        }
    }
}

/// A capability entry from the termcap database, consisting of a 2-character `code` and a [`Field`]
/// `field`.
struct Entry {
    /// The two-character code associated with the capability.
    pub code: Code,
    /// The value of the capability, as a [`Field`] enum.
    pub value: Field,
}

impl std::fmt::Display for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = String::from_utf8_lossy(&self.code.0);
        f.write_fmt(format_args!("{}: {}", code, self.value))
    }
}

/// The two-character code associated with the capability.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Code([u8; 2]);

impl Code {
    fn new(code: [u8; 2]) -> Self {
        Code(code)
    }
}

impl std::fmt::Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = String::from_utf8_lossy(&self.0);
        f.write_str(&code)
    }
}

#[derive(Default)]
pub struct Database {
    /// The entries, as a vector sorted on [`TermId::key`] with its associated vector of [`Entry`]s,
    /// sorted on [`Entry::code`].
    entries: Vec<(TermId, Vec<Entry>)>,
}

impl Database {
    fn lookup(&self, term: &str) -> Option<&[Entry]> {
        // Perform a quick binary search first round on the most common name
        if let Ok(idx) = self
            .entries
            .binary_search_by_key(&term, |entry| entry.0.key.as_str())
        {
            Some(self.entries[idx].1.as_slice())
        }
        // Fall back to full traversal to search case-insensitive common name or alternate name
        // matches.
        else if let Some(idx) = self.entries.iter().position(|entry| {
            entry
                .0
                .aliases
                .iter()
                .any(|alias| alias.eq_ignore_ascii_case(term))
        }) {
            Some(self.entries[idx].1.as_slice())
        } else {
            None
        }
    }

    pub fn load(path: &Path) -> IoResult<Database> {
        if !path.exists() {
            return Err(Error::from(ErrorKind::NotFound));
        }

        Database::parse(Path::new(path))
    }

    pub fn parse(path: &Path) -> IoResult<Database> {
        use std::fs::File;
        use std::io::prelude::*;
        use std::io::BufReader;

        let mut database = Database::default();
        let mut term_id;
        let mut capabilities: Vec<Entry> = Vec::new();
        let mut line = String::new();
        let mut file = BufReader::new(File::open(path)?);

        macro_rules! insert_entry {
            () => {
                if !capabilities.is_empty() {
                    capabilities.sort_by_key(|cap| cap.code);
                    eprintln!(
                        "Found {} capabilities associated with terminal {}",
                        capabilities.len(),
                        term_id.as_ref().unwrap()
                    );
                    database
                        .entries
                        .push((term_id.take().unwrap(), capabilities.split_off(0)));
                }
            };
        }

        'outer: loop {
            line.clear();
            if file.read_line(&mut line)? == 0 {
                break 'outer;
            }
            if line.starts_with('#') {
                // Skip over this comment line.
                continue;
            }
            // In case of continuation, keep reading.
            while line.ends_with("\\\n") {
                line.pop();
                line.pop();
                // After a continuation, a line starts with a \t that should be ignored and then a :
                // that we already ended on.
                let mut _tab = [0u8; 2];
                let _ = file.read_exact(&mut _tab)?;
                file.read_line(&mut line)?;
            }

            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            // dbg!(line);
            let (ids, caps) = line.split_once(':').unwrap();
            // dbg!(ids);

            // Parse line as a terminal identifier.
            // Format should be `name1|name2|..|long name maybe w/ spaces :\`
            let mut names = ids.split('|').map(str::trim_end).map(str::to_string);
            term_id = Some(TermId {
                key: names.next().unwrap(), // first value is always available
                aliases: names.collect(),
            });
            // dbg!(&term_id.as_ref().unwrap().key);

            // Capabilities are one or more per line, starting with :, separated by ':', and
            // continuing on the next line with \ ending when the line ends with : without a
            // new line escape \.

            // Capabilities are separated by : but we can't just call line.split(':') because a
            // : can be part of a capability's string payload. We use a custom iterator to
            // return capabilties that splits on : but not escaped colons (denoted as \:).
            let mut remainder = caps.trim_matches(':').chars().peekable();
            let mut cap = String::new();
            let caps_fn = || -> Option<_> {
                cap.clear();
                loop {
                    match remainder.next() {
                        None | Some(':') if cap.trim().is_empty() => return None,
                        None | Some(':') => return Some(cap.clone()),
                        Some('^') => {
                            // Handle control code
                            let next = remainder.next().unwrap();
                            cap.push(char::from(next as u8 ^ 0x40));
                        }
                        Some('\\') => {
                            // Handle escape
                            match remainder.next() {
                                Some('\\') => cap.push('\\'),
                                Some('E') => cap.push('\x1B'), // escape
                                Some('e') => cap.push('\x1B'), // escape typo (bitgraph on BSD)
                                Some('n') => cap.push('\n'),
                                Some('t') => cap.push('\t'),
                                Some('r') => cap.push('\r'),
                                Some('b') => cap.push('\x08'), // backspace
                                Some('^') => cap.push('^'),
                                Some('f') => cap.push('\x0C'), // form feed
                                Some(':') => cap.push(':'),
                                Some(d0 @ '0'..='7') => {
                                    // Octal number. These are supposed to be three octal digits
                                    // long, but it seems it's "up to" three digits in practice.
                                    let mut oct = [d0 as u8, b'\0', b'\0'];
                                    let oct = if matches!(remainder.peek(), Some('0'..='7')) {
                                        oct[1] = remainder.next().unwrap() as u8;
                                        if matches!(remainder.peek(), Some('0'..='7')) {
                                            oct[2] = remainder.next().unwrap() as u8;
                                            &oct[..3]
                                        } else {
                                            &oct[..2]
                                        }
                                    } else {
                                        &oct[..1]
                                    };
                                    let oct = std::str::from_utf8(oct).unwrap();
                                    let byte = u8::from_str_radix(oct, 8).unwrap();
                                    cap.push(char::from(byte));
                                }
                                c @ _ => {
                                    // unreachable!("\\{:?} on {}", c, caps),
                                    // Typos abound in termcap files.
                                    // e.g. there's \LM1 \LM2 in 4112 under BSD, which should be
                                    // \ELM1\ELM2 judging by context.
                                    eprintln!("\\{:?} on {}", c, caps);
                                }
                            }
                        }
                        Some(c) => cap.push(c),
                    }
                }
            };

            let mut caps = std::iter::from_fn(caps_fn);
            while let Some(cap) = caps.next() {
                if cap.is_empty()
                        // The (boolean?) capability is "commented out" and should be skipped
                        || cap.starts_with('.')
                {
                    continue;
                }

                if cap.as_bytes().len() < 2 {
                    // adm20 entry in FreeBSD termcap has a flag with the literal code ^X which
                    // we unescape to a single byte above. The original entry is obviously
                    // invalid, no such flag exists.
                    eprintln!("Unsupported termcap entry: {} on {}", cap, line);
                    continue;
                }
                let code = Code::new(cap.as_bytes()[0..2].try_into().unwrap());
                let value =
                        // Boolean fields are denoted only by the presence of their code, e.g. :xn:
                        if cap.len() == 2 {
                            // Boolean capability, always true by value of its mere presence
                            Field::Bool(true)
                        }
                        // A terminal can inherit capabilities from another terminal's termcap
                        // entry, but if there are some capabilities it explicitly disavows (e.g.
                        // implements foo except for bar and baz), it has those caps listed as :xx@:
                        // before the inheritance entry :tc=foo:
                        else if cap.as_bytes().get(2) == Some(&b'@') {
                            // Record this capability as explicitly not available.
                            Field::Bool(false)
                        }
                        // Numeric entries are denoted as :xx#value:
                        else if cap.as_bytes().get(2) == Some(&b'#') {
                            // Numeric capability
                            Field::Number(cap[3..].parse().expect(&format!(
                                "Failed to parse {} value {} as number",
                                code, cap
                            )))
                        }
                        // String entries are denoted as :xx=yyy:
                        else if cap.as_bytes().get(2) == Some(&b'=') {
                            assert!(
                                cap.contains('='),
                                "Expected a string value parsing cap {}, got {}",
                                code,
                                cap
                            );
                            Field::String(cap[3..].to_string())
                        } else {
                            // Some termcap files have unsupported legacy names, like the AT&T
                            // Teletype 5425 (5425) capabilities list, which has k1, k2, .., k9,
                            // then goes on to unsupported value k10. Some later termcap databases
                            // change these to compatible value, mapping k10 to k, and
                            // K11..K16 to F1..F6
                            eprintln!("Unsupported capability {}: {}", code, cap);
                            continue;
                        };

                capabilities.push(Entry { code, value });
            }
            insert_entry!();
        }

        database
            .entries
            .sort_by(|lhs, rhs| lhs.0.key.cmp(&rhs.0.key));
        Ok(database)
    }
}

#[test]
fn db_load() -> std::io::Result<()> {
    let db = Termcap::load()?.database;
    let entries = db
        .lookup(&std::env::var("TERM").unwrap())
        .ok_or(std::io::Error::from(std::io::ErrorKind::NotFound))?;

    for entry in entries {
        eprintln!("{}", entry);
    }

    Ok(())
}

#[test]
fn caps_dump() -> std::io::Result<()> {
    let termcap = Termcap::load()?;
    let caps = termcap.get_env().ok_or(Error::from(ErrorKind::NotFound))?;

    for cap in caps.iter() {
        eprintln!("{}", cap);
    }

    Ok(())
}
