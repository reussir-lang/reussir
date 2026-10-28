//! Client-owned live payloads backed by compcache's persistent executable bytes.

use super::{
    Error, Result,
    executable::{CompilationClient, Executable},
};
use compcache::{InMemoryCache, Key, MemoryConfig, MemoryEntry, QuickCache};
use std::sync::Arc;

#[derive(Clone)]
pub(super) enum Payload {
    Executable(Arc<Executable>),
    Refined(Arc<reussir_stablehlo::Refined>),
}

pub(super) struct Memory {
    pub owner: Arc<CompilationClient>,
    pub entries: Arc<QuickCache<Payload>>,
}

impl Memory {
    pub fn new(owner: Arc<CompilationClient>, config: &MemoryConfig) -> compcache::Result<Self> {
        Ok(Self {
            owner,
            entries: Arc::new(QuickCache::new(config)?),
        })
    }
}

impl InMemoryCache for Memory {
    type Payload = Payload;

    fn from_config(_: &MemoryConfig) -> compcache::Result<Self> {
        Err(compcache::Error::Config {
            field: "pjrt.client",
            reason: "supply a PJRT client with in_memory",
        })
    }

    fn materialize(&self, bytes: Arc<[u8]>) -> compcache::Result<Payload> {
        if bytes.len() < 32 || blake3::hash(&bytes[32..]).as_bytes() != &bytes[..32] {
            return Err(compcache::Error::producer(Error::CorruptArtifact));
        }
        let executable = self
            .owner
            .deserialize(&bytes[32..])
            .map_err(compcache::Error::producer)?;
        executable.set_weight(bytes.len());
        tracing::debug!("PJRT executable loaded from persistent compcache");
        Ok(Payload::Executable(executable))
    }

    fn weight(&self, payload: &Payload) -> u64 {
        match payload {
            Payload::Executable(executable) => executable.weight(),
            Payload::Refined(program) => {
                (program.bytecode.len()
                    + program
                        .result_types
                        .iter()
                        .map(|ty| ty.dimensions.len() * 8 + 32)
                        .sum::<usize>()
                    + 128) as u64
            }
        }
    }
    fn get(&self, key: &Key) -> Option<Arc<MemoryEntry<Payload>>> {
        self.entries.get(key)
    }
    fn insert(&self, key: Key, entry: Arc<MemoryEntry<Payload>>) -> Vec<Arc<MemoryEntry<Payload>>> {
        self.entries.insert(key, entry)
    }
    fn remove(&self, key: &Key) -> Option<Arc<MemoryEntry<Payload>>> {
        self.entries.remove(key)
    }
    fn clear(&self) -> Vec<Arc<MemoryEntry<Payload>>> {
        self.entries.clear()
    }
}

pub(super) fn executable(payload: Payload) -> Result<Arc<Executable>> {
    match payload {
        Payload::Executable(executable) => Ok(executable),
        Payload::Refined(_) => Err(Error::UnexpectedPayload),
    }
}
