use crate::consumers::source::has_sieve_extension;
use crate::{Instance, Relation, Result, Witness, FILE_EXTENSION};
use std::fs::{create_dir_all, read_dir, remove_file, File};
use std::io::Write;
use std::path::{Path, PathBuf};

pub trait Sink {
    type Write: Write;

    fn get_instance_writer(&mut self) -> &mut Self::Write;
    fn get_witness_writer(&mut self) -> &mut Self::Write;
    fn get_relation_writer(&mut self) -> &mut Self::Write;

    fn push_instance(&mut self, instance: &Instance) -> Result<()> {
        instance.write_into(self.get_instance_writer())
    }

    fn push_witness(&mut self, witness: &Witness) -> Result<()> {
        witness.write_into(self.get_witness_writer())
    }

    fn push_relation(&mut self, relation: &Relation) -> Result<()> {
        relation.write_into(self.get_relation_writer())
    }
}

#[derive(Default)]
pub struct MemorySink {
    instance_buffer: Vec<u8>,
    witness_buffer: Vec<u8>,
    relation_buffer: Vec<u8>,
}

impl Sink for MemorySink {
    type Write = Vec<u8>;

    fn get_instance_writer(&mut self) -> &mut Self::Write {
        &mut self.instance_buffer
    }
    fn get_witness_writer(&mut self) -> &mut Self::Write {
        &mut self.witness_buffer
    }
    fn get_relation_writer(&mut self) -> &mut Self::Write {
        &mut self.relation_buffer
    }
}

/// Store messages into files using conventional filenames inside of a workspace.
pub struct FilesSink {
    pub workspace: PathBuf,
    /// Set to true to print the paths of files as they are created.
    pub print_filenames: bool,

    file_counter: u32,
    current_file: Option<File>,
}

impl FilesSink {
    pub fn new(workspace: impl AsRef<Path>) -> Result<FilesSink> {
        create_dir_all(workspace.as_ref())?;
        Ok(FilesSink {
            workspace: workspace.as_ref().to_path_buf(),
            print_filenames: false,
            file_counter: 0,
            current_file: None,
        })
    }
}

impl FilesSink {
    pub fn clean_workspace(&mut self) -> Result<()> {
        self.file_counter = 0;
        clean_workspace(&self.workspace)
    }

    fn next_file(&mut self, typ: &str) -> Result<File> {
        let path = self.workspace.join(format!(
            "{:03}_{}.{}",
            self.file_counter, typ, FILE_EXTENSION
        ));
        if self.print_filenames {
            eprintln!("Writing {}", path.display());
        }
        let file = File::create(path)?;
        self.file_counter += 1;
        Ok(file)
    }
}

impl Sink for FilesSink {
    type Write = File;

    fn get_instance_writer(&mut self) -> &mut File {
        self.current_file = Some(self.next_file("instance").unwrap());
        self.current_file.as_mut().unwrap()
    }

    fn get_witness_writer(&mut self) -> &mut File {
        self.current_file = Some(self.next_file("witness").unwrap());
        self.current_file.as_mut().unwrap()
    }

    fn get_relation_writer(&mut self) -> &mut File {
        self.current_file = Some(self.next_file("relation").unwrap());
        self.current_file.as_mut().unwrap()
    }
}

pub fn clean_workspace(workspace: impl AsRef<Path>) -> Result<()> {
    let workspace = workspace.as_ref();

    let files = read_dir(workspace)?;

    for f in files
        .filter_map(std::result::Result::ok)
        .filter(|d| has_sieve_extension(&d.path()))
    {
        remove_file(f.path())?;
    }

    Ok(())
}

#[test]
fn test_sink() {
    use crate::producers::examples::*;
    use std::fs::remove_dir_all;

    let workspace = PathBuf::from("local/test_sink");
    let _ = remove_dir_all(&workspace);
    let mut sink = FilesSink::new(&workspace).unwrap();

    // workspace is empty, check it!
    assert_eq!(read_dir(&workspace).unwrap().count(), 0);

    // Create files and ensure there is exactly the right number of files.
    sink.push_instance(&example_instance()).unwrap();
    assert_eq!(read_dir(&workspace).unwrap().count(), 1);

    sink.push_witness(&example_witness()).unwrap();
    assert_eq!(read_dir(&workspace).unwrap().count(), 2);

    sink.push_relation(&example_relation()).unwrap();
    assert_eq!(read_dir(&workspace).unwrap().count(), 3);

    sink.push_instance(&example_instance()).unwrap();
    assert_eq!(read_dir(&workspace).unwrap().count(), 4);

    sink.push_witness(&example_witness()).unwrap();
    assert_eq!(read_dir(&workspace).unwrap().count(), 5);

    sink.push_relation(&example_relation()).unwrap();
    assert_eq!(read_dir(&workspace).unwrap().count(), 6);

    // clean workspace, and check there is no more file in it.
    clean_workspace(&workspace).unwrap();
    assert_eq!(read_dir(&workspace).unwrap().count(), 0);
}
