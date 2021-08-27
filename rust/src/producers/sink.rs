use crate::{consumers::source::has_sieve_extension, Source};
use crate::{Instance, Relation, Result, Witness, FILE_EXTENSION};
use std::fs::{create_dir_all, read_dir, remove_file, File};
use std::io::Write;
use std::path::{Path, PathBuf};

pub trait Sink {
    type Write: Write;

    fn get_instance_writer(&mut self) -> &mut Self::Write;
    fn get_witness_writer(&mut self) -> &mut Self::Write;
    fn get_relation_writer(&mut self) -> &mut Self::Write;

    fn push_instance_message(&mut self, instance: &Instance) -> Result<()> {
        instance.write_into(self.get_instance_writer())
    }

    fn push_witness_message(&mut self, witness: &Witness) -> Result<()> {
        witness.write_into(self.get_witness_writer())
    }

    fn push_relation_message(&mut self, relation: &Relation) -> Result<()> {
        relation.write_into(self.get_relation_writer())
    }
}

#[derive(Default)]
pub struct MemorySink {
    pub instance_buffer: Vec<u8>,
    pub witness_buffer: Vec<u8>,
    pub relation_buffer: Vec<u8>,
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

impl Into<Source> for MemorySink {
    fn into(self) -> Source {
        Source::from_buffers(vec![
            self.instance_buffer,
            self.witness_buffer,
            self.relation_buffer,
        ])
    }
}

/// Store messages into files using conventional filenames inside of a workspace.
pub struct FilesSink {
    pub workspace: PathBuf,

    instance_file: File,
    witness_file: File,
    relation_file: File,
}

impl FilesSink {
    pub fn new_clean(workspace: &impl AsRef<Path>) -> Result<FilesSink> {
        create_dir_all(workspace)?;
        clean_workspace(workspace)?;
        Self::new_no_cleanup(workspace)
    }

    pub fn new_no_cleanup(workspace: &impl AsRef<Path>) -> Result<FilesSink> {
        Ok(FilesSink {
            workspace: workspace.as_ref().to_path_buf(),

            instance_file: File::create(Self::instance_path(workspace))?,
            witness_file: File::create(Self::witness_path(workspace))?,
            relation_file: File::create(Self::relation_path(workspace))?,
        })
    }

    pub fn instance_path(workspace: &impl AsRef<Path>) -> PathBuf {
        workspace
            .as_ref()
            .join(format!("000_instance.{}", FILE_EXTENSION))
    }

    pub fn witness_path(workspace: &impl AsRef<Path>) -> PathBuf {
        workspace
            .as_ref()
            .join(format!("001_witness.{}", FILE_EXTENSION))
    }

    pub fn relation_path(workspace: &impl AsRef<Path>) -> PathBuf {
        workspace
            .as_ref()
            .join(format!("002_relation.{}", FILE_EXTENSION))
    }

    pub fn print_filenames(&self) {
        eprintln!("Writing {}", Self::instance_path(&self.workspace).display());
        eprintln!("Writing {}", Self::witness_path(&self.workspace).display());
        eprintln!("Writing {}", Self::relation_path(&self.workspace).display());
    }
}

impl Sink for FilesSink {
    type Write = File;

    fn get_instance_writer(&mut self) -> &mut File {
        &mut self.instance_file
    }

    fn get_witness_writer(&mut self) -> &mut File {
        &mut self.witness_file
    }

    fn get_relation_writer(&mut self) -> &mut File {
        &mut self.relation_file
    }
}

impl Into<Source> for FilesSink {
    fn into(self) -> Source {
        Source::from_directory(&self.workspace).unwrap()
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
    use crate::consumers::stats::Stats;
    use crate::producers::examples::*;

    let workspace = PathBuf::from("local/test_sink");

    // Helper to look at file names and sizes.
    let get_file_sizes = || {
        let mut file_sizes = read_dir(&workspace)
            .unwrap()
            .map(|res| {
                let e = res.unwrap();
                (e.path(), e.metadata().unwrap().len())
            })
            .collect::<Vec<_>>();

        file_sizes.sort();

        let filenames = file_sizes
            .iter()
            .map(|(name, _)| name.clone())
            .collect::<Vec<_>>();

        let sizes = file_sizes.iter().map(|(_, size)| *size).collect::<Vec<_>>();

        (filenames, sizes)
    };

    let mut sink = FilesSink::new_clean(&workspace).unwrap();

    let expected_filenames = &[
        ("local/test_sink/000_instance.sieve".into()),
        ("local/test_sink/001_witness.sieve".into()),
        ("local/test_sink/002_relation.sieve".into()),
    ] as &[PathBuf];

    let (filenames, sizes) = get_file_sizes();
    assert_eq!(filenames.as_slice(), expected_filenames);
    assert_eq!(sizes, vec![0, 0, 0]);

    sink.push_instance_message(&example_instance()).unwrap();

    sink.push_witness_message(&example_witness()).unwrap();

    sink.push_relation_message(&example_relation()).unwrap();

    let (filenames, sizes1) = get_file_sizes();
    assert_eq!(filenames.as_slice(), expected_filenames);
    assert!(sizes[0] < sizes1[0]);
    assert!(sizes[1] < sizes1[1]);
    assert!(sizes[2] < sizes1[2]);

    sink.push_instance_message(&example_instance()).unwrap();

    sink.push_witness_message(&example_witness()).unwrap();

    sink.push_relation_message(&example_relation()).unwrap();

    let (filenames, sizes2) = get_file_sizes();
    assert_eq!(filenames.as_slice(), expected_filenames);
    assert!(sizes1[0] < sizes2[0]);
    assert!(sizes1[1] < sizes2[1]);
    assert!(sizes1[2] < sizes2[2]);

    let source: Source = sink.into();
    let mut stats = Stats::default();
    for msg in source.iter_messages() {
        stats.ingest_message(&msg.unwrap());
    }
    assert_eq!(stats.gate_stats.instance_messages, 2);
    assert_eq!(stats.gate_stats.witness_messages, 2);
    assert_eq!(stats.gate_stats.relation_messages, 2);

    // clean workspace, and check there is no more file in it.
    clean_workspace(&workspace).unwrap();
    assert!(get_file_sizes().0.is_empty());
}
