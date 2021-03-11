use std::path::{PathBuf, Path};
use std::fs::{File, read_dir};
use std::iter;
use std::io::{Read, stdin};
use std::ffi::OsStr;
use std::convert::TryFrom;
use crate::consumers::utils::read_buffer;
use crate::{Result, Message, Messages, FILE_EXTENSION};


/// Source finds and reads IR messages from a directory.
/// It supports reading messages one-by-one from large files or from many files.
/// It supports reading from stdin using dash (-) as a special filename.
/// It supports reading from given buffers.
///
/// # Example
/// ```
/// use sieve_ir_zki::{Source, FilesSink, Sink, Message};
/// use sieve_ir_zki::producers::examples::*;
/// use std::path::PathBuf;
///
/// // Create an example workspace including multiple constraints files.
/// let dir = PathBuf::from("local/test_source");
/// let mut sink = FilesSink::new(&dir).unwrap();
/// sink.clean_workspace().unwrap();
/// sink.push_instance(&example_instance());
/// sink.push_witness(&example_witness());
/// sink.push_relation(&example_relation());
/// sink.push_witness(&example_witness());
/// sink.push_relation(&example_relation());
///
/// // Iterate over the files and observe the messages.
/// let mut got = vec![];
///
/// let source = Source::from_directory(&dir).unwrap();
/// for msg in source.iter_messages() {
///     match msg.unwrap() {
///         Message::Instance(h) => got.push("INSTANCE"),
///         Message::Witness(w) => got.push("WITNESS"),
///         Message::Relation(cs) => got.push("RELATION"),
///     }
/// }
///
/// assert_eq!(got, vec!["INSTANCE", "WITNESS", "RELATION", "WITNESS", "RELATION"]);
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Source {
    /// Set to true to print the paths of files as they are read.
    pub print_filenames: bool,

    buffer_source: BufferSource,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum BufferSource {
    Stdin,
    Files(Vec<PathBuf>),
    Memory(Vec<Vec<u8>>),
}

impl Source {
    pub fn from_directory(path: &Path) -> Result<Self> {
        Self::from_dirs_and_files(&[path.to_path_buf()])
    }

    pub fn from_dirs_and_files(paths: &[PathBuf]) -> Result<Self> {
        let all_files = list_workspace_files(paths)?;
        Ok(Self::from_filenames(all_files))
    }

    pub fn from_filenames(mut paths: Vec<PathBuf>) -> Self {
        let source = if paths == vec![PathBuf::from("-")] {
            BufferSource::Stdin
        } else {
            paths.sort();
            /* Alternative, sort by message type.
            paths.sort_by_key(|path| {
                let name = path.file_name().unwrap().to_str().unwrap();
                match () {
                    _ if name.contains("instance") => 0,
                    _ if name.contains("witness") => 1,
                    _ if name.contains("relation") => 3,
                    _ => 4,
                }
            });*/
            BufferSource::Files(paths)
        };
        Source { buffer_source: source, print_filenames: false }
    }

    pub fn from_buffers(buffers: Vec<Vec<u8>>) -> Self {
        Source { buffer_source: BufferSource::Memory(buffers), print_filenames: false }
    }

    pub fn iter_buffers<'w>(&'w self) -> Box<dyn Iterator<Item=Vec<u8>> + 'w> {
        match &self.buffer_source {
            BufferSource::Stdin => Box::new(
                iterate_stream(stdin())),
            BufferSource::Files(paths) => Box::new(
                iterate_files(&paths[..], self.print_filenames)),
            BufferSource::Memory(buffers) => Box::new(
                iterate_buffers(&buffers[..])),
        }
    }

    pub fn iter_messages<'w>(&'w self) -> impl Iterator<Item=Result<Message>> + 'w {
        self.iter_buffers().map(|buffer| Message::try_from(&buffer[..]))
    }

    pub fn read_all_messages(&self) -> Result<Messages> {
        let mut messages = Messages::default();
        for msg in self.iter_messages() {
            messages.push_message(&msg?);
        }
        Ok(messages)
    }
}

pub fn iterate_files<'w>(paths: &'w [PathBuf], print: bool) -> impl Iterator<Item=Vec<u8>> + 'w {
    paths.iter().flat_map(move |path| {
        if print { eprintln!("Reading {}", path.display()); }
        iterate_file(path)
    })
}

pub fn iterate_file(path: &Path) -> Box<dyn Iterator<Item=Vec<u8>>> {
    match File::open(path) {
        Err(err) => {
            eprintln!("Warning: failed to open file {}: {}", path.display(), err);
            Box::new(iter::empty())
        }
        Ok(file) => Box::new(
            iterate_stream(file)),
    }
}

pub fn iterate_buffers<'w>(buffers: &'w [Vec<u8>]) -> impl Iterator<Item=Vec<u8>> + 'w {
    buffers.iter().flat_map(|buffer| {
        iterate_stream(&buffer[..])
    })
}

pub fn iterate_stream<'s>(mut stream: impl Read + 's) -> impl Iterator<Item=Vec<u8>> + 's {
    iter::from_fn(move ||
        match read_buffer(&mut stream) {
            Err(err) => {
                eprintln!("Warning: failed to read: {}", err);
                None
            }
            Ok(buffer) => {
                if buffer.len() == 0 {
                    None
                } else {
                    Some(buffer)
                }
            }
        }
    )
}

pub fn has_sieve_extension(path: &Path) -> bool {
    path.extension() == Some(OsStr::new(FILE_EXTENSION))
}

pub fn list_workspace_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>> {
    let mut all_paths = vec![];

    for path in paths {
        if has_sieve_extension(path) {
            all_paths.push(path.clone());
        } else if path == Path::new("-") {
            if paths.len() > 1 { return Err("Cannot combine files and stdin".into()); }
            all_paths.push(path.clone());
        } else {
            for file in read_dir(path)? {
                match file {
                    Ok(file) => {
                        if has_sieve_extension(&file.path()) {
                            all_paths.push(file.path());
                        }
                    }
                    Err(err) => {
                        eprintln!("Warning: {}", err);
                        continue;
                    }
                }
            }
        }
    }
    Ok(all_paths)
}
