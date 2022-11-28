use crate::{consumers::source::has_sieve_extension, Source};
use crate::{PrivateInputs, PublicInputs, Relation, Result, FILE_EXTENSION};
use std::fs::{create_dir_all, read_dir, remove_file, File};
use std::io::Write;
use std::path::{Path, PathBuf};

pub trait Sink {
    type Write: Write;

    fn get_public_inputs_writer(&mut self) -> &mut Self::Write;
    fn get_private_inputs_writer(&mut self) -> &mut Self::Write;
    fn get_relation_writer(&mut self) -> &mut Self::Write;

    fn push_public_inputs_message(&mut self, public_inputs: &PublicInputs) -> Result<()> {
        public_inputs.write_into(self.get_public_inputs_writer())
    }

    fn push_private_inputs_message(&mut self, private_inputs: &PrivateInputs) -> Result<()> {
        private_inputs.write_into(self.get_private_inputs_writer())
    }

    fn push_relation_message(&mut self, relation: &Relation) -> Result<()> {
        relation.write_into(self.get_relation_writer())
    }
}

#[derive(Default)]
pub struct MemorySink {
    pub public_inputs_buffer: Vec<u8>,
    pub private_inputs_buffer: Vec<u8>,
    pub relation_buffer: Vec<u8>,
}

impl Sink for MemorySink {
    type Write = Vec<u8>;

    fn get_public_inputs_writer(&mut self) -> &mut Self::Write {
        &mut self.public_inputs_buffer
    }
    fn get_private_inputs_writer(&mut self) -> &mut Self::Write {
        &mut self.private_inputs_buffer
    }
    fn get_relation_writer(&mut self) -> &mut Self::Write {
        &mut self.relation_buffer
    }
}

impl From<MemorySink> for Source {
    fn from(mem: MemorySink) -> Source {
        Source::from_buffers(vec![
            mem.public_inputs_buffer,
            mem.private_inputs_buffer,
            mem.relation_buffer,
        ])
    }
}

/// Store messages into files using conventional filenames inside of a workspace.
pub struct FilesSink {
    pub workspace: PathBuf,

    public_inputs_file: File,
    private_inputs_file: File,
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

            public_inputs_file: File::create(Self::public_inputs_path(workspace))?,
            private_inputs_file: File::create(Self::private_inputs_path(workspace))?,
            relation_file: File::create(Self::relation_path(workspace))?,
        })
    }

    pub fn public_inputs_path(workspace: &impl AsRef<Path>) -> PathBuf {
        workspace
            .as_ref()
            .join(format!("000_public_inputs.{}", FILE_EXTENSION))
    }

    pub fn private_inputs_path(workspace: &impl AsRef<Path>) -> PathBuf {
        workspace
            .as_ref()
            .join(format!("001_private_inputs.{}", FILE_EXTENSION))
    }

    pub fn relation_path(workspace: &impl AsRef<Path>) -> PathBuf {
        workspace
            .as_ref()
            .join(format!("002_relation.{}", FILE_EXTENSION))
    }

    pub fn print_filenames(&self) {
        eprintln!(
            "Writing {}",
            Self::public_inputs_path(&self.workspace).display()
        );
        eprintln!(
            "Writing {}",
            Self::private_inputs_path(&self.workspace).display()
        );
        eprintln!("Writing {}", Self::relation_path(&self.workspace).display());
    }
}

impl Sink for FilesSink {
    type Write = File;

    fn get_public_inputs_writer(&mut self) -> &mut File {
        &mut self.public_inputs_file
    }

    fn get_private_inputs_writer(&mut self) -> &mut File {
        &mut self.private_inputs_file
    }

    fn get_relation_writer(&mut self) -> &mut File {
        &mut self.relation_file
    }
}

impl From<FilesSink> for Source {
    fn from(files_sink: FilesSink) -> Source {
        Source::from_directory(&files_sink.workspace).unwrap()
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
    use crate::producers::simple_examples::*;

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
        ("local/test_sink/000_public_inputs.sieve".into()),
        ("local/test_sink/001_private_inputs.sieve".into()),
        ("local/test_sink/002_relation.sieve".into()),
    ] as &[PathBuf];

    let (filenames, sizes) = get_file_sizes();
    assert_eq!(filenames.as_slice(), expected_filenames);
    assert_eq!(sizes, vec![0, 0, 0]);

    sink.push_public_inputs_message(&simple_example_public_inputs())
        .unwrap();

    sink.push_private_inputs_message(&simple_example_private_inputs())
        .unwrap();

    sink.push_relation_message(&simple_example_relation())
        .unwrap();

    let (filenames, sizes1) = get_file_sizes();
    assert_eq!(filenames.as_slice(), expected_filenames);
    assert!(sizes[0] < sizes1[0]);
    assert!(sizes[1] < sizes1[1]);
    assert!(sizes[2] < sizes1[2]);

    sink.push_public_inputs_message(&simple_example_public_inputs())
        .unwrap();

    sink.push_private_inputs_message(&simple_example_private_inputs())
        .unwrap();

    sink.push_relation_message(&simple_example_relation())
        .unwrap();

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
    assert_eq!(stats.gate_stats.public_inputs_messages, 2);
    assert_eq!(stats.gate_stats.private_inputs_messages, 2);
    assert_eq!(stats.gate_stats.relation_messages, 2);

    // clean workspace, and check there is no more file in it.
    clean_workspace(&workspace).unwrap();
    assert!(get_file_sizes().0.is_empty());
}
