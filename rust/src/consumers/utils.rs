use std::io::Read;
use flatbuffers::{read_scalar_at, SIZE_UOFFSET, UOffsetT};
use crate::Result;

// Read a flatbuffers size prefix (4 bytes, little-endian). Size including the prefix.
pub fn read_size_prefix(buf: &[u8]) -> usize {
    if buf.len() < SIZE_UOFFSET { return 0; }
    let size = read_scalar_at::<UOffsetT>(buf, 0) as usize;
    SIZE_UOFFSET + size
}

pub fn split_messages(mut buf: &[u8]) -> Vec<&[u8]> {
    let mut bufs = vec![];
    loop {
        let size = read_size_prefix(buf);
        if size <= SIZE_UOFFSET { break; }
        bufs.push(&buf[..size]);
        buf = &buf[size..];
    }
    bufs
}

pub fn read_buffer(stream: &mut impl Read) -> Result<Vec<u8>> {
    let mut buffer = vec![0u8; 4];
    if stream.read_exact(&mut buffer).is_err() {
        return Ok(Vec::new()); // End of stream at the correct place.
    }
    let size = read_size_prefix(&buffer);
    eprintln!("Read size: {:?} --> size={}", buffer, size);
    if size <= SIZE_UOFFSET {
        return Ok(Vec::new()); // Explicit size 0 as end marker.
    }
    buffer.resize(size, 0);
    stream.read_exact(&mut buffer[4..])?;
    eprintln!("Read buffer: {:?}", buffer);
    Ok(buffer)
}