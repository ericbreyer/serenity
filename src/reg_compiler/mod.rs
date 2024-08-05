mod emit_ssa;
mod resolve_labels;
use tracing::debug;
// mod optimization_pass;

pub mod iasm;
mod optimize;
mod tests;

use std::collections::HashMap;
use crate::prelude::*;

use self::iasm::IASMProg;

pub fn compile(
    parsed: ParseResult,
    _native_functions: &HashMap<SharedString, (usize, UValueType)>,
) -> Result<IASMProg, SharedString> {
    let ssa = emit_ssa::EmitRegWalker::compile(parsed.clone())?;
    debug!("SSA:");
    debug!("{:#?}", ssa);
    // let optimized = optimize::optimize(ssa);
    let optimized = ssa;
    debug!("optimized:");
    debug!("{:#?}", optimized);
    let resolved = resolve_labels::resolve_labels(optimized);
    debug!("Resolved:");
    debug!("{:#?}", resolved);
    Ok(resolved)
}
