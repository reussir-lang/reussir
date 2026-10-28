//! Client-owned live payloads backed by compcache's persistent executable bytes.

use super::{
    Error, Result,
    executable::{CompilationClient, Executable, LaunchPlan},
};
use compcache::{InMemoryCache, Key, MemoryConfig, MemoryEntry, QuickCache};
use std::sync::Arc;

type Payload = Arc<Executable>;

/// A checksummed record containing length-delimited JSON execution metadata
/// followed by the plugin's opaque executable bytes. No source is persisted.
pub(super) fn encode(plan: &LaunchPlan, executable: &[u8]) -> Result<Vec<u8>> {
    let metadata = serde_json::to_vec(plan).map_err(Arc::new)?;
    let mut record = vec![0; 32];
    record.extend_from_slice(&(metadata.len() as u64).to_le_bytes());
    record.extend_from_slice(&metadata);
    record.extend_from_slice(executable);
    let digest = blake3::hash(&record[32..]);
    record[..32].copy_from_slice(digest.as_bytes());
    Ok(record)
}

fn decode(bytes: &[u8]) -> Result<(LaunchPlan, &[u8])> {
    if bytes.len() < 40 || blake3::hash(&bytes[32..]).as_bytes() != &bytes[..32] {
        return Err(Error::CorruptArtifact);
    }
    let length = u64::from_le_bytes(bytes[32..40].try_into().unwrap());
    let end = usize::try_from(length)
        .ok()
        .and_then(|n| n.checked_add(40))
        .filter(|&n| n <= bytes.len())
        .ok_or(Error::CorruptArtifact)?;
    let plan = serde_json::from_slice(&bytes[40..end]).map_err(Arc::new)?;
    Ok((plan, &bytes[end..]))
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
        let (plan, executable) = decode(&bytes).map_err(compcache::Error::producer)?;
        let executable = self
            .owner
            .deserialize(executable, plan)
            .map_err(compcache::Error::producer)?;
        executable.set_weight(bytes.len());
        tracing::debug!("PJRT executable loaded from persistent compcache");
        Ok(executable)
    }

    fn weight(&self, payload: &Payload) -> u64 {
        payload.weight()
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
