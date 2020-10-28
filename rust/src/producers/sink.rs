use std::path::{Path, PathBuf};
use std::fs::{remove_file, File, create_dir_all, read_dir};
use crate::{Result, Instance, Witness, Relation, FILE_EXTENSION};
use crate::consumers::workspace::has_sieve_extension;

pub trait Sink {
    fn push_instance(&mut self, instance: &Instance) -> Result<()>;
    fn push_witness(&mut self, witness: &Witness) -> Result<()>;
    fn push_relation(&mut self, relation: &Relation) -> Result<()>;
}


/// Store messages into files using conventional filenames inside of a workspace.
pub struct WorkspaceSink {
    pub workspace: PathBuf,
    /// Set to true to print the paths of files as they are created.
    pub print_filenames: bool,

    file_counter: u32,
}

impl WorkspaceSink {
    pub fn new(workspace: impl AsRef<Path>) -> Result<WorkspaceSink> {
        create_dir_all(workspace.as_ref())?;
        Ok(WorkspaceSink {
            workspace: workspace.as_ref().to_path_buf(),
            print_filenames: false,
            file_counter: 0,
        })
    }
}

impl WorkspaceSink {
    pub fn clean_workspace(&mut self) -> Result<()> {
        self.file_counter = 0;
        clean_workspace(&self.workspace)
    }

    fn next_file(&mut self, typ: &str) -> Result<File> {
        let path = self.workspace.join(format!(
            "{:03}_{}.{}",
            self.file_counter, typ, FILE_EXTENSION));
        if self.print_filenames {
            eprintln!("Writing {}", path.display());
        }
        let file = File::create(path)?;
        self.file_counter += 1;
        Ok(file)
    }
}

impl Sink for WorkspaceSink {
    fn push_instance(&mut self, instance: &Instance) -> Result<()> {
        let mut file = self.next_file("instance")?;
        instance.write_into(&mut file)
    }

    fn push_witness(&mut self, witness: &Witness) -> Result<()> {
        let mut file = self.next_file("witness")?;
        witness.write_into(&mut file)
    }

    fn push_relation(&mut self, relation: &Relation) -> Result<()> {
        let mut file = self.next_file("relation")?;
        relation.write_into(&mut file)
    }
}

pub fn clean_workspace(workspace: impl AsRef<Path>) -> Result<()> {
    let workspace = workspace.as_ref();

    let files = read_dir(workspace)?;

    for f in files.filter_map(std::result::Result::ok)
        .filter(|d| has_sieve_extension(&d.path())) {
        remove_file(f.path())?;
    }

    Ok(())
}


#[test]
fn test_workspace_sink() {
    use std::fs::remove_dir_all;
    use crate::producers::examples::*;

    let workspace = PathBuf::from("local/test_workspace_sink");
    let _ = remove_dir_all(&workspace);
    let mut sink = WorkspaceSink::new(&workspace).unwrap();

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