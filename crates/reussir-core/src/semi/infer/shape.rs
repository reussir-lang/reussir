//! Inference-only shapes. Metavariables never occupy serialized extent slots.
use super::{InferCtxt, Level, Mismatch, key};
use crate::semi::ty::{HoleId, Ty, TyKind};
use rustc_hash::FxHashMap;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DimId(pub(crate) usize);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ShapeId(pub(crate) usize);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Dim {
    Known(u64),
    Var(DimId),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TensorView<'tcx> {
    pub elem: Ty<'tcx>,
    pub shape: ShapeId,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ShapeError {
    Dimension { left: u64, right: u64 },
    Rank { left: usize, right: usize },
    MinimumRank { minimum: usize, actual: usize },
}
impl fmt::Display for ShapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dimension { left, right } => {
                write!(f, "incompatible dimensions {left} and {right}")
            }
            Self::Rank { left, right } => write!(f, "incompatible tensor ranks {left} and {right}"),
            Self::MinimumRank { minimum, actual } => {
                write!(f, "rank {actual} is smaller than required rank {minimum}")
            }
        }
    }
}
impl std::error::Error for ShapeError {}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Family<'tcx> {
    Tensor,
    Array,
    DevArray(Ty<'tcx>),
    Spec,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Shaped<'tcx> {
    pub family: Family<'tcx>,
    pub view: TensorView<'tcx>,
}
#[derive(Clone)]
struct DimNode {
    parent: usize,
    size: usize,
    value: Option<u64>,
    level: Level,
}
#[derive(Clone)]
struct ShapeNode {
    parent: usize,
    size: usize,
    dims: Option<Vec<Dim>>,
    minimum: usize,
    level: Level,
}
enum Undo<'tcx> {
    Dim(usize, DimNode),
    Shape(usize, ShapeNode),
    View(HoleId, Option<Shaped<'tcx>>),
    Cache(Ty<'tcx>),
}
pub(super) struct Snapshot {
    dims: usize,
    shapes: usize,
    undo: usize,
    progress: u64,
}
pub(super) struct ShapeStore<'tcx> {
    dims: Vec<DimNode>,
    shapes: Vec<ShapeNode>,
    views: FxHashMap<HoleId, Shaped<'tcx>>,
    cache: FxHashMap<Ty<'tcx>, Shaped<'tcx>>,
    undo: Vec<Undo<'tcx>>,
    snapshots: usize,
    pub(super) progress: u64,
}
impl<'tcx> Default for ShapeStore<'tcx> {
    fn default() -> Self {
        Self {
            dims: Vec::new(),
            shapes: Vec::new(),
            views: FxHashMap::default(),
            cache: FxHashMap::default(),
            undo: Vec::new(),
            snapshots: 0,
            progress: 0,
        }
    }
}
impl<'tcx> ShapeStore<'tcx> {
    pub(super) fn snapshot(&mut self) -> Snapshot {
        self.snapshots += 1;
        Snapshot {
            dims: self.dims.len(),
            shapes: self.shapes.len(),
            undo: self.undo.len(),
            progress: self.progress,
        }
    }
    pub(super) fn commit(&mut self, _: Snapshot) {
        self.snapshots -= 1;
        if self.snapshots == 0 {
            self.undo.clear();
        }
    }
    pub(super) fn rollback(&mut self, snapshot: Snapshot) {
        while self.undo.len() > snapshot.undo {
            match self.undo.pop().unwrap() {
                Undo::Cache(ty) => {
                    self.cache.remove(&ty);
                }
                Undo::Dim(i, old) => self.dims[i] = old,
                Undo::Shape(i, old) => self.shapes[i] = old,
                Undo::View(i, old) => match old {
                    Some(v) => {
                        self.views.insert(i, v);
                    }
                    None => {
                        self.views.remove(&i);
                    }
                },
            }
        }
        self.dims.truncate(snapshot.dims);
        self.shapes.truncate(snapshot.shapes);
        self.progress = snapshot.progress;
        self.snapshots -= 1;
    }
    fn save_dim(&mut self, id: usize) {
        if self.snapshots != 0 {
            self.undo.push(Undo::Dim(id, self.dims[id].clone()));
        }
    }
    fn save_shape(&mut self, id: usize) {
        if self.snapshots != 0 {
            self.undo.push(Undo::Shape(id, self.shapes[id].clone()));
        }
    }
    pub(super) fn set_view(&mut self, id: HoleId, view: Shaped<'tcx>) {
        if self.views.get(&id) == Some(&view) {
            return;
        }
        let old = self.views.insert(id, view);
        if self.snapshots != 0 {
            self.undo.push(Undo::View(id, old));
        }
        self.progress += 1;
    }
    fn dim_root(&self, mut id: usize) -> usize {
        while self.dims[id].parent != id {
            id = self.dims[id].parent;
        }
        id
    }
    fn shape_root(&self, mut id: usize) -> usize {
        while self.shapes[id].parent != id {
            id = self.shapes[id].parent;
        }
        id
    }
    fn lower_shape_level(&mut self, id: ShapeId, level: Level) {
        let root = self.shape_root(id.0);
        if self.shapes[root].level > level {
            self.save_shape(root);
            self.shapes[root].level = level;
        }
        if let Some(dims) = self.shapes[root].dims.clone() {
            for dim in dims {
                if let Dim::Var(id) = dim {
                    let root = self.dim_root(id.0);
                    if self.dims[root].level > level {
                        self.save_dim(root);
                        self.dims[root].level = level;
                    }
                }
            }
        }
    }
}
impl<'a, 'tcx> InferCtxt<'a, 'tcx> {
    /// Monotonic within a transaction; rollback restores the previous revision.
    pub fn progress(&self) -> u64 {
        self.shapes.progress
    }
    pub fn new_dim(&mut self) -> Dim {
        let id = self.shapes.dims.len();
        self.shapes.dims.push(DimNode {
            parent: id,
            size: 1,
            value: None,
            level: self.level,
        });
        Dim::Var(DimId(id))
    }
    pub fn new_shape(&mut self) -> ShapeId {
        let id = self.shapes.shapes.len();
        self.shapes.shapes.push(ShapeNode {
            parent: id,
            size: 1,
            dims: None,
            minimum: 0,
            level: self.level,
        });
        ShapeId(id)
    }
    pub fn ranked_shape(&mut self, dims: Vec<Dim>) -> ShapeId {
        let id = self.new_shape();
        self.shapes.shapes[id.0].dims = Some(dims);
        id
    }
    pub fn resolve_dim(&self, dim: Dim) -> Dim {
        match dim {
            Dim::Known(_) => dim,
            Dim::Var(id) => {
                let root = self.shapes.dim_root(id.0);
                self.shapes.dims[root]
                    .value
                    .map(Dim::Known)
                    .unwrap_or(Dim::Var(DimId(root)))
            }
        }
    }
    pub fn shape_dims(&self, shape: ShapeId) -> Option<Vec<Dim>> {
        self.shapes.shapes[self.shapes.shape_root(shape.0)]
            .dims
            .as_ref()
            .map(|dims| dims.iter().map(|&d| self.resolve_dim(d)).collect())
    }
    pub fn shape_rank_min(&self, shape: ShapeId) -> usize {
        let node = &self.shapes.shapes[self.shapes.shape_root(shape.0)];
        node.dims.as_ref().map_or(node.minimum, Vec::len)
    }
    pub fn require_rank_at_least(
        &mut self,
        shape: ShapeId,
        minimum: usize,
    ) -> Result<(), ShapeError> {
        let root = self.shapes.shape_root(shape.0);
        let node = &self.shapes.shapes[root];
        if let Some(dims) = &node.dims {
            return if dims.len() >= minimum {
                Ok(())
            } else {
                Err(ShapeError::MinimumRank {
                    minimum,
                    actual: dims.len(),
                })
            };
        }
        if node.minimum < minimum {
            self.shapes.save_shape(root);
            self.shapes.shapes[root].minimum = minimum;
            self.shapes.progress += 1;
        }
        Ok(())
    }
    pub fn unify_dims(&mut self, left: Dim, right: Dim) -> Result<(), ShapeError> {
        match (self.resolve_dim(left), self.resolve_dim(right)) {
            (Dim::Known(a), Dim::Known(b)) if a != b => {
                Err(ShapeError::Dimension { left: a, right: b })
            }
            (Dim::Known(_), Dim::Known(_)) => Ok(()),
            (Dim::Var(a), Dim::Known(b)) | (Dim::Known(b), Dim::Var(a)) => {
                self.shapes.save_dim(a.0);
                self.shapes.dims[a.0].value = Some(b);
                self.shapes.progress += 1;
                Ok(())
            }
            (Dim::Var(a), Dim::Var(b)) if a == b => Ok(()),
            (Dim::Var(a), Dim::Var(b)) => {
                let (a, b) = if self.shapes.dims[a.0].size >= self.shapes.dims[b.0].size {
                    (a.0, b.0)
                } else {
                    (b.0, a.0)
                };
                self.shapes.save_dim(a);
                self.shapes.save_dim(b);
                self.shapes.dims[b].parent = a;
                self.shapes.dims[a].size += self.shapes.dims[b].size;
                self.shapes.dims[a].level =
                    self.shapes.dims[a].level.min(self.shapes.dims[b].level);
                self.shapes.progress += 1;
                Ok(())
            }
        }
    }
    /// Atomic even when a later dimension contradicts an earlier equality.
    pub fn unify_shapes(&mut self, left: ShapeId, right: ShapeId) -> Result<(), ShapeError> {
        let snapshot = self.shapes.snapshot();
        let result = self.unify_shapes_inner(left, right);
        if result.is_ok() {
            self.shapes.commit(snapshot);
        } else {
            self.shapes.rollback(snapshot);
        }
        result
    }
    fn unify_shapes_inner(&mut self, left: ShapeId, right: ShapeId) -> Result<(), ShapeError> {
        let a = self.shapes.shape_root(left.0);
        let b = self.shapes.shape_root(right.0);
        if a == b {
            return Ok(());
        }
        let na = self.shapes.shapes[a].clone();
        let nb = self.shapes.shapes[b].clone();
        let minimum = na.minimum.max(nb.minimum);
        if let (Some(xs), Some(ys)) = (&na.dims, &nb.dims) {
            if xs.len() != ys.len() {
                return Err(ShapeError::Rank {
                    left: xs.len(),
                    right: ys.len(),
                });
            }
            for (&x, &y) in xs.iter().zip(ys) {
                self.unify_dims(x, y)?;
            }
        }
        let dims = na.dims.or(nb.dims);
        if let Some(ds) = &dims {
            if ds.len() < minimum {
                return Err(ShapeError::MinimumRank {
                    minimum,
                    actual: ds.len(),
                });
            }
        }
        // Re-observing two fully known equal shapes adds no information.
        // In particular, a solver must not keep waking on freshly viewed
        // canonical tensor types whose dimensions are already established.
        if self
            .shape_dims(left)
            .is_some_and(|ds| ds.iter().all(|d| matches!(d, Dim::Known(_))))
            && self
                .shape_dims(right)
                .is_some_and(|ds| ds.iter().all(|d| matches!(d, Dim::Known(_))))
        {
            return Ok(());
        }
        let (a, b) = if na.size >= nb.size { (a, b) } else { (b, a) };
        self.shapes.save_shape(a);
        self.shapes.save_shape(b);
        self.shapes.shapes[b].parent = a;
        self.shapes.shapes[a].size += self.shapes.shapes[b].size;
        self.shapes.shapes[a].dims = dims;
        self.shapes.shapes[a].minimum = minimum;
        self.shapes.shapes[a].level = na.level.min(nb.level);
        self.shapes
            .lower_shape_level(ShapeId(a), na.level.min(nb.level));
        self.shapes.progress += 1;
        Ok(())
    }
}
impl<'a, 'tcx> InferCtxt<'a, 'tcx> {
    fn new_shaped_ty(&mut self, family: Family<'tcx>, elem: Ty<'tcx>, shape: ShapeId) -> Ty<'tcx> {
        let id = self.new_hole();
        self.shapes.set_view(
            id,
            Shaped {
                family,
                view: TensorView { elem, shape },
            },
        );
        // Allocating an independent variable is not progress on an obligation.
        self.shapes.progress -= 1;
        self.tcx.mk_hole(id)
    }
    pub fn new_tensor_ty(&mut self, elem: Ty<'tcx>, shape: ShapeId) -> Ty<'tcx> {
        self.new_shaped_ty(Family::Tensor, elem, shape)
    }
    pub fn new_array_ty(&mut self, elem: Ty<'tcx>, shape: ShapeId) -> Ty<'tcx> {
        self.new_shaped_ty(Family::Array, elem, shape)
    }
    pub fn new_devarray_ty(
        &mut self,
        elem: Ty<'tcx>,
        shape: ShapeId,
        placement: Ty<'tcx>,
    ) -> Ty<'tcx> {
        self.new_shaped_ty(Family::DevArray(placement), elem, shape)
    }
    pub(super) fn hole_view(&mut self, hole: HoleId) -> Option<Shaped<'tcx>> {
        let root = self.table.find(key(hole)).id;
        self.shapes.views.get(&root).copied()
    }
    pub(super) fn set_hole_view(&mut self, hole: HoleId, view: Shaped<'tcx>) {
        let root = self.table.find(key(hole)).id;
        self.shapes.set_view(root, view);
    }
    /// Canonicalization waits for rank and every extent. Rank-zero additionally
    /// waits for dtype, so it cannot be mistaken for a whole-shape former.
    pub(super) fn canonical_shaped(&mut self, shaped: Shaped<'tcx>) -> Option<Ty<'tcx>> {
        let dims: Vec<u64> = self
            .shape_dims(shaped.view.shape)?
            .into_iter()
            .map(|d| match d {
                Dim::Known(n) => Some(n),
                _ => None,
            })
            .collect::<Option<_>>()?;
        let elem = self.shallow_resolve(shaped.view.elem);
        if dims.is_empty() && matches!(elem.kind(), TyKind::Hole(_)) {
            return None;
        }
        Some(match shaped.family {
            Family::Tensor => self.tcx.mk_tensor(elem, &dims),
            Family::Array => self.tcx.mk_array(elem, &dims),
            Family::DevArray(p) => {
                let p = self.shallow_resolve(p);
                self.tcx.mk_devarray(elem, &dims, Some(p))
            }
            Family::Spec if dims.is_empty() => elem,
            Family::Spec => self.tcx.mk_array(elem, &dims),
        })
    }
    fn spec_view(&mut self, spec: Ty<'tcx>) -> TensorView<'tcx> {
        let spec = self.shallow_resolve(spec);
        if let TyKind::Hole(h) = spec.kind() {
            if let Some(v) = self.hole_view(*h) {
                return if matches!(v.family, Family::Spec | Family::Array) {
                    v.view
                } else {
                    TensorView {
                        elem: spec,
                        shape: self.ranked_shape(Vec::new()),
                    }
                };
            }
            let elem = self.new_hole_ty();
            let shape = self.new_shape();
            let view = TensorView { elem, shape };
            self.set_hole_view(
                *h,
                Shaped {
                    family: Family::Spec,
                    view,
                },
            );
            return view;
        }
        match spec.kind() {
            TyKind::Array { elem, dims } => TensorView {
                elem: *elem,
                shape: self.ranked_shape(dims.iter().copied().map(Dim::Known).collect()),
            },
            _ => TensorView {
                elem: spec,
                shape: self.ranked_shape(Vec::new()),
            },
        }
    }
    pub(super) fn shaped_view(&mut self, ty: Ty<'tcx>) -> Option<Shaped<'tcx>> {
        if let TyKind::Hole(h) = ty.kind() {
            if let Some(v) = self.hole_view(*h) {
                return Some(v);
            }
        }
        let ty = self.shallow_resolve(ty);
        if let Some(view) = self.shapes.cache.get(&ty) {
            return Some(*view);
        }
        let (family, elem, dims) = match ty.kind() {
            TyKind::Tensor { elem, dims } => (Family::Tensor, *elem, *dims),
            TyKind::Array { elem, dims } => (Family::Array, *elem, *dims),
            TyKind::DevArray {
                elem,
                dims,
                placement,
            } => (Family::DevArray(*placement), *elem, *dims),
            _ => return None,
        };
        let view = if dims.is_empty() && !matches!(family, Family::Array) {
            self.spec_view(elem)
        } else {
            TensorView {
                elem,
                shape: self.ranked_shape(dims.iter().copied().map(Dim::Known).collect()),
            }
        };
        let shaped = Shaped { family, view };
        self.shapes.cache.insert(ty, shaped);
        if self.shapes.snapshots != 0 {
            self.shapes.undo.push(Undo::Cache(ty));
        }
        Some(shaped)
    }
    pub fn tensor_view(&mut self, ty: Ty<'tcx>) -> Option<TensorView<'tcx>> {
        self.shaped_view(ty)
            .filter(|v| matches!(v.family, Family::Tensor))
            .map(|v| v.view)
    }
    pub fn array_view(&mut self, ty: Ty<'tcx>) -> Option<TensorView<'tcx>> {
        self.shaped_view(ty)
            .filter(|v| matches!(v.family, Family::Array))
            .map(|v| v.view)
    }
    pub fn devarray_view(&mut self, ty: Ty<'tcx>) -> Option<(TensorView<'tcx>, Ty<'tcx>)> {
        self.shaped_view(ty).and_then(|v| match v.family {
            Family::DevArray(p) => Some((v.view, p)),
            _ => None,
        })
    }
    pub(super) fn unify_shaped(
        &mut self,
        a: Ty<'tcx>,
        b: Ty<'tcx>,
    ) -> Option<Result<(), Mismatch<'tcx>>> {
        if matches!(a.kind(), TyKind::Bottom) || matches!(b.kind(), TyKind::Bottom) {
            return Some(Ok(()));
        }
        let av = self.shaped_view(a);
        let bv = self.shaped_view(b);
        let mismatch = Mismatch { left: a, right: b };
        match (av, bv) {
            (Some(a), Some(b)) => {
                let compatible = match (a.family, b.family) {
                    (Family::Tensor, Family::Tensor)
                    | (Family::Array, Family::Array)
                    | (Family::Spec, Family::Spec)
                    | (Family::Spec, Family::Array)
                    | (Family::Array, Family::Spec) => true,
                    (Family::DevArray(pa), Family::DevArray(pb)) => {
                        if let Err(e) = self.unify(pa, pb) {
                            return Some(Err(e));
                        }
                        true
                    }
                    _ => false,
                };
                if !compatible {
                    return Some(Err(mismatch));
                }
                Some(
                    self.unify_shapes(a.view.shape, b.view.shape)
                        .map_err(|_| mismatch)
                        .and_then(|()| self.unify(a.view.elem, b.view.elem)),
                )
            }
            (Some(view), None) | (None, Some(view)) => {
                let other = if av.is_some() { b } else { a };
                if matches!(other.kind(), TyKind::Hole(_)) {
                    return None;
                }
                if matches!(view.family, Family::Spec) {
                    let rank_zero = self.ranked_shape(Vec::new());
                    Some(
                        self.unify_shapes(view.view.shape, rank_zero)
                            .map_err(|_| mismatch)
                            .and_then(|()| self.unify(view.view.elem, other)),
                    )
                } else {
                    Some(Err(mismatch))
                }
            }
            _ => None,
        }
    }
}

impl<'a, 'tcx> InferCtxt<'a, 'tcx> {
    pub(super) fn lower_levels(&mut self, hole: HoleId, ty: Ty<'tcx>) {
        if let super::HoleValue::Unknown { level } = self.table.probe_value(key(hole)) {
            self.lower_ty_level(ty, level, &mut rustc_hash::FxHashSet::default());
        }
    }
    fn lower_ty_level(
        &mut self,
        ty: Ty<'tcx>,
        level: Level,
        seen: &mut rustc_hash::FxHashSet<HoleId>,
    ) {
        if let TyKind::Hole(h) = ty.kind() {
            let root = self.table.find(key(*h)).id;
            if !seen.insert(root) {
                return;
            }
            match self.table.probe_value(key(root)) {
                super::HoleValue::Known(t) => self.lower_ty_level(t, level, seen),
                super::HoleValue::Unknown { level: old } => {
                    if old > level {
                        self.table
                            .union_value(key(root), super::HoleValue::Unknown { level });
                    }
                    if let Some(v) = self.hole_view(root) {
                        self.shapes.lower_shape_level(v.view.shape, level);
                        self.lower_ty_level(v.view.elem, level, seen);
                        if let Family::DevArray(p) = v.family {
                            self.lower_ty_level(p, level, seen);
                        }
                    }
                }
            }
            return;
        }
        match ty.kind() {
            TyKind::Record { args, .. } | TyKind::Future(args) | TyKind::Tuple(args) => {
                for &t in args.iter() {
                    self.lower_ty_level(t, level, seen);
                }
            }
            TyKind::Closure { params, ret } => {
                for &t in params.iter() {
                    self.lower_ty_level(t, level, seen);
                }
                self.lower_ty_level(*ret, level, seen);
            }
            TyKind::Array { elem, .. }
            | TyKind::Tensor { elem, .. }
            | TyKind::Cell { elem, .. }
            | TyKind::Nullable(elem)
            | TyKind::Arc(elem) => self.lower_ty_level(*elem, level, seen),
            TyKind::DevArray {
                elem, placement, ..
            } => {
                self.lower_ty_level(*elem, level, seen);
                self.lower_ty_level(*placement, level, seen);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semi::infer::Instantiation;
    use crate::semi::ty::{DYNAMIC_EXTENT, GenericId, IntTy};
    use crate::with_tcx;

    #[test]
    fn unknown_rank_and_scalar_rank_are_distinct() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let unknown = i.new_shape();
            let scalar = i.ranked_shape(vec![]);
            assert_eq!(i.shape_dims(unknown), None);
            assert_eq!(i.shape_dims(scalar), Some(vec![]));
            i.require_rank_at_least(unknown, 2).unwrap();
            assert!(i.unify_shapes(unknown, scalar).is_err());
            assert_eq!(i.shape_dims(unknown), None);
            let matrix = i.ranked_shape(vec![Dim::Known(0), Dim::Known(4)]);
            i.unify_shapes(unknown, matrix).unwrap();
            assert_eq!(
                i.shape_dims(unknown),
                Some(vec![Dim::Known(0), Dim::Known(4)])
            );
        });
    }
    #[test]
    fn dimensions_are_shared_and_zero_is_a_value() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let a = i.new_dim();
            let b = i.new_dim();
            i.unify_dims(a, b).unwrap();
            i.unify_dims(b, Dim::Known(0)).unwrap();
            assert_eq!(i.resolve_dim(a), Dim::Known(0));
            assert!(i.unify_dims(a, Dim::Known(1)).is_err());
        });
    }
    #[test]
    fn runtime_extent_is_not_an_inference_variable() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let hole = i.new_dim();
            i.unify_dims(hole, Dim::Known(DYNAMIC_EXTENT)).unwrap();
            assert_eq!(i.resolve_dim(hole), Dim::Known(DYNAMIC_EXTENT));
            assert!(i.unify_dims(hole, Dim::Known(8)).is_err());
        });
    }
    #[test]
    fn contradictory_shape_unification_is_atomic() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let x = i.new_dim();
            let a = i.ranked_shape(vec![x, Dim::Known(2)]);
            let b = i.ranked_shape(vec![Dim::Known(3), Dim::Known(4)]);
            let revision = i.progress();
            assert!(i.unify_shapes(a, b).is_err());
            assert_eq!(i.resolve_dim(x), x);
            assert_eq!(i.progress(), revision);
        });
    }
    #[test]
    fn probe_restores_shapes_types_bounds_and_revision() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let d = i.new_dim();
            let s = i.new_shape();
            let e = i.new_hole_ty();
            let t = i.new_tensor_ty(e, s);
            let revision = i.progress();
            let result: Result<(), ()> = i.probe(|i| {
                i.require_rank_at_least(s, 2).unwrap();
                i.unify_dims(d, Dim::Known(5)).unwrap();
                i.unify(t, tcx.mk_tensor(tcx.mk_bool(), &[2, 3])).unwrap();
                Err(())
            });
            assert!(result.is_err());
            assert_eq!(i.progress(), revision);
            assert_eq!(i.resolve_dim(d), d);
            assert_eq!(i.shape_dims(s), None);
            assert_eq!(i.shape_rank_min(s), 0);
            assert_eq!(i.resolve(e), e);
            i.unify(t, tcx.mk_tensor(tcx.mk_int(IntTy::Signed(32)), &[]))
                .unwrap();
            assert_eq!(
                i.resolve(t),
                tcx.mk_tensor(tcx.mk_int(IntTy::Signed(32)), &[])
            );
        });
    }
    #[test]
    fn nested_probe_commit_remains_undoable_by_outer_probe() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let d = i.new_dim();
            let result: Result<(), ()> = i.probe(|i| {
                i.probe::<_, ()>(|i| {
                    i.unify_dims(d, Dim::Known(9)).unwrap();
                    Ok(())
                })
                .unwrap();
                Err(())
            });
            assert!(result.is_err());
            assert_eq!(i.resolve_dim(d), d);
        });
    }
    #[test]
    fn shaped_holes_keep_identity_when_unioned_with_plain_holes() {
        with_tcx(|tcx| {
            for reverse in [false, true] {
                let mut i = InferCtxt::new(tcx);
                let plain = i.new_hole_ty();
                let s = i.new_shape();
                let shaped = i.new_tensor_ty(tcx.mk_bool(), s);
                if reverse {
                    i.unify(shaped, plain).unwrap();
                } else {
                    i.unify(plain, shaped).unwrap();
                }
                i.unify(plain, tcx.mk_tensor(tcx.mk_bool(), &[7])).unwrap();
                assert_eq!(i.resolve(shaped), tcx.mk_tensor(tcx.mk_bool(), &[7]));
            }
        });
    }
    #[test]
    fn scalar_dtype_hole_does_not_turn_into_whole_shape_former() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let elem = i.new_hole_ty();
            let s = i.ranked_shape(vec![]);
            let scalar = i.new_tensor_ty(elem, s);
            assert!(matches!(i.shallow_resolve(scalar).kind(), TyKind::Hole(_)));
            assert!(i.unify(scalar, tcx.mk_tensor(tcx.mk_bool(), &[3])).is_err());
            i.unify(elem, tcx.mk_bool()).unwrap();
            assert_eq!(i.resolve(scalar), tcx.mk_tensor(tcx.mk_bool(), &[]));
        });
    }
    #[test]
    fn whole_shape_former_relates_to_partial_shape_and_materializes_argument() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let spec = i.new_hole_ty();
            let former = tcx.mk_tensor(spec, &[]);
            let dim = i.new_dim();
            let s = i.ranked_shape(vec![dim, Dim::Known(4)]);
            let actual = i.new_tensor_ty(tcx.mk_bool(), s);
            i.unify(former, actual).unwrap();
            i.unify_dims(dim, Dim::Known(3)).unwrap();
            assert_eq!(i.resolve(spec), tcx.mk_array(tcx.mk_bool(), &[3, 4]));
            assert_eq!(i.resolve(former), tcx.mk_tensor(tcx.mk_bool(), &[3, 4]));
        });
    }
    #[test]
    fn generic_instantiation_preserves_partial_shape_relations() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let generic = GenericId(0);
            let template = tcx.mk_tensor(tcx.mk_generic(generic), &[]);
            let inst = i.instantiate(&[generic]);
            let dim = i.new_dim();
            let s = i.ranked_shape(vec![dim]);
            let actual = i.new_tensor_ty(tcx.mk_bool(), s);
            i.unify_instantiated(template, &inst, actual).unwrap();
            i.unify_dims(dim, Dim::Known(8)).unwrap();
            assert_eq!(
                i.resolve(inst.get(generic).unwrap()),
                tcx.mk_array(tcx.mk_bool(), &[8])
            );
            let other = i.instantiate(&[generic]);
            i.unify_instantiated(template, &other, tcx.mk_tensor(tcx.mk_bool(), &[9]))
                .unwrap();
            assert_ne!(
                i.resolve(inst.get(generic).unwrap()),
                i.resolve(other.get(generic).unwrap())
            );
        });
    }
    #[test]
    fn shaped_occurs_check_rejects_hidden_recursive_dtype_and_placement() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let h = i.new_hole_ty();
            let s = i.new_shape();
            let t = i.new_tensor_ty(h, s);
            assert!(i.unify(h, t).is_err());
            let h = i.new_hole_ty();
            let s = i.new_shape();
            let t = i.new_devarray_ty(tcx.mk_bool(), s, h);
            assert!(i.unify(h, t).is_err());
        });
    }
    #[test]
    fn instantiated_structural_type_cannot_overwrite_shaped_hole() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let s = i.new_shape();
            let t = i.new_tensor_ty(tcx.mk_bool(), s);
            let inst = Instantiation::from_pairs([]);
            assert!(
                i.unify_instantiated(tcx.mk_int(IntTy::Signed(32)), &inst, t)
                    .is_err()
            );
            i.unify(t, tcx.mk_tensor(tcx.mk_bool(), &[1])).unwrap();
        });
    }
    #[test]
    fn repeated_concrete_relations_reach_a_fixed_point() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let a = tcx.mk_tensor(tcx.mk_bool(), &[2, 3]);
            let s = i.new_shape();
            let b = i.new_tensor_ty(tcx.mk_bool(), s);
            i.unify(a, b).unwrap();
            let revision = i.progress();
            for _ in 0..20 {
                i.unify(a, b).unwrap();
            }
            assert_eq!(i.progress(), revision);
        });
    }
    #[test]
    fn shape_and_dtype_levels_follow_older_binding() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let outer = i.new_hole_ty();
            i.enter_level();
            let elem = i.new_hole_ty();
            let d = i.new_dim();
            let s = i.ranked_shape(vec![d]);
            let inner = i.new_tensor_ty(elem, s);
            i.exit_level();
            i.unify(outer, inner).unwrap();
            assert_eq!(i.shapes.shapes[i.shapes.shape_root(s.0)].level, Level(0));
            let Dim::Var(d) = d else { unreachable!() };
            assert_eq!(i.shapes.dims[i.shapes.dim_root(d.0)].level, Level(0));
            let TyKind::Hole(h) = elem.kind() else {
                unreachable!()
            };
            assert!(matches!(
                i.table.probe_value(key(*h)),
                super::super::HoleValue::Unknown { level: Level(0) }
            ));
        });
    }
    #[test]
    fn array_tensor_and_device_array_families_remain_distinct() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let s = i.new_shape();
            let array = i.new_array_ty(tcx.mk_bool(), s);
            let t = i.new_tensor_ty(tcx.mk_bool(), s);
            assert!(i.unify(array, t).is_err());
            let p = i.new_hole_ty();
            let device = i.new_devarray_ty(tcx.mk_bool(), s, p);
            assert!(i.unify(device, t).is_err());
            let concrete = tcx.mk_devarray(tcx.mk_bool(), &[4], None);
            i.unify(device, concrete).unwrap();
            assert_eq!(i.resolve(device), concrete);
            assert_eq!(i.resolve(array), tcx.mk_array(tcx.mk_bool(), &[4]));
        });
    }
    #[test]
    fn whole_shape_former_does_not_flatten_nested_tensor_or_device_array() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let s = i.new_shape();
            let inner = i.new_tensor_ty(tcx.mk_bool(), s);
            let nested = tcx.mk_tensor(inner, &[]);
            assert!(i.unify(nested, tcx.mk_tensor(tcx.mk_bool(), &[3])).is_err());
            assert!(i.unify(nested, tcx.mk_tensor(tcx.mk_bool(), &[])).is_err());
            let s = i.new_shape();
            let inner = i.new_devarray_ty(tcx.mk_bool(), s, tcx.mk(TyKind::DefaultPlacement));
            let nested = tcx.mk_tensor(inner, &[]);
            assert!(i.unify(nested, tcx.mk_tensor(tcx.mk_bool(), &[3])).is_err());
        });
    }
    #[test]
    fn view_cache_is_bounded_and_transactional() {
        with_tcx(|tcx| {
            let mut i = InferCtxt::new(tcx);
            let t = tcx.mk_tensor(tcx.mk_bool(), &[3, 4]);
            let result: Result<(), ()> = i.probe(|i| {
                i.tensor_view(t).unwrap();
                Err(())
            });
            assert!(result.is_err());
            assert!(i.shapes.cache.is_empty());
            assert!(i.shapes.shapes.is_empty());
            let first = i.tensor_view(t).unwrap();
            let count = i.shapes.shapes.len();
            for _ in 0..100 {
                assert_eq!(i.tensor_view(t), Some(first));
            }
            assert_eq!(i.shapes.shapes.len(), count);
        });
    }
}
