//! Helpers to read messages.

use std::fs::File;
use std::io::Read;
use std::path::Path;
use flatbuffers::SIZE_UOFFSET;

use crate::sieve_ir_generated::sieve_ir as g;
use crate::Result;
use super::utils::{read_buffer, read_size_prefix};

/// Collect buffers waiting to be read.
#[derive(Clone, Default)]
pub struct Reader {
    pub messages: Vec<Vec<u8>>,
}

impl Reader {
    pub fn push_message(&mut self, buf: Vec<u8>) -> Result<()> {
        self.messages.push(buf);
        Ok(())
    }

    pub fn read_from(&mut self, reader: &mut impl Read) -> Result<()> {
        loop {
            let buffer = read_buffer(reader)?;
            if buffer.len() == 0 {
                return Ok(());
            }
            self.push_message(buffer)?;
        }
    }

    pub fn read_file(&mut self, path: impl AsRef<Path>) -> Result<()> {
        let mut file = File::open(&path)?;
        let mut buf = Vec::new();
        file.read_to_end(&mut buf)?;
        self.push_message(buf)
    }
}

// Implement `for message in reader`
impl<'a> IntoIterator for &'a Reader {
    type Item = g::Root<'a>;
    type IntoIter = MessageIterator<'a>;

    fn into_iter(self) -> MessageIterator<'a> {
        MessageIterator {
            bufs: &self.messages,
            offset: 0,
        }
    }
}

pub struct MessageIterator<'a> {
    bufs: &'a [Vec<u8>],
    offset: usize,
}

impl<'a> Iterator for MessageIterator<'a> {
    type Item = g::Root<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.bufs.len() == 0 { return None; }

            let buf = &self.bufs[0][self.offset..];

            let size = {
                let size = read_size_prefix(buf);
                if size <= buf.len() {
                    size
                } else {
                    buf.len()
                }
            };

            if size <= SIZE_UOFFSET {
                // Move to the next buffer.
                self.bufs = &self.bufs[1..];
                self.offset = 0;
                continue;
            }

            // Move to the next message in the current buffer.
            self.offset += size;

            // Parse the current message.
            let root = g::get_size_prefixed_root_as_root(&buf[..size]);
            return Some(root);
        }
    }
}
