//! Optional compiler features that gate parts of the language.
//!
//! A build of the compiler may leave out an optional feature (see
//! `cmake/ReussirFeatures.cmake`). The frontend then rejects programs that use
//! what the feature provides, at the use site, instead of failing later in
//! lowering. The `core` library still declares everything unconditionally;
//! its ground items that need a missing feature are left unlowered (see
//! [`crate::semi::feature_use`]).

/// An optional feature the frontend checks uses against.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Feature {
    /// StableHLO kernels: `#[kernel]` functions, `Token`, `Future`, the tensor
    /// intrinsics, and kernel launches.
    OpenXla,
    /// Arbitrary-precision integers: `Nat` and `Int`.
    Rcgmp,
}

impl Feature {
    pub const ALL: [Feature; 2] = [Feature::OpenXla, Feature::Rcgmp];

    /// The feature's name in the compiler's feature registry.
    pub fn name(self) -> &'static str {
        match self {
            Feature::OpenXla => "openxla",
            Feature::Rcgmp => "rcgmp",
        }
    }

    /// The feature registered under `name`.
    pub fn from_name(name: &str) -> Option<Feature> {
        Feature::ALL.into_iter().find(|f| f.name() == name)
    }

    fn bit(self) -> u32 {
        1 << self as u32
    }
}

/// A set of features: those a program may use, or those an item needs.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct FeatureSet(u32);

impl FeatureSet {
    pub const EMPTY: FeatureSet = FeatureSet(0);

    /// Every feature, for sessions that do not lower code (tests, tools) and
    /// for compiling `core` itself.
    pub fn all() -> Self {
        FeatureSet(Feature::ALL.iter().fold(0, |bits, f| bits | f.bit()))
    }

    /// The features for which `enabled` holds.
    pub fn from_fn(mut enabled: impl FnMut(Feature) -> bool) -> Self {
        FeatureSet(
            Feature::ALL
                .iter()
                .filter(|&&f| enabled(f))
                .fold(0, |bits, f| bits | f.bit()),
        )
    }

    pub fn contains(self, feature: Feature) -> bool {
        self.0 & feature.bit() != 0
    }

    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub fn insert(&mut self, feature: Feature) {
        self.0 |= feature.bit();
    }

    pub fn union(self, other: FeatureSet) -> FeatureSet {
        FeatureSet(self.0 | other.0)
    }

    /// The features of `self`, in registry order.
    pub fn iter(self) -> impl Iterator<Item = Feature> {
        Feature::ALL.into_iter().filter(move |&f| self.contains(f))
    }

    /// The features of `needed` outside `self`.
    pub fn missing(self, needed: FeatureSet) -> FeatureSet {
        FeatureSet(needed.0 & !self.0)
    }
}

impl Default for FeatureSet {
    fn default() -> Self {
        FeatureSet::all()
    }
}
