use crate::{Result, WireId};
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use crate::sieve_ir_generated::sieve_ir as g;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::collections::HashMap;


// ***********************************
//
//     IterExprWireNumber
//
// ***********************************

/// This structure is an iterator expression
#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum IterExprWireNumber {
    // Const iterator
    IterExprConst(u64),
    // Named iterator
    IterExprName(String),
    // Addition of iterator Expression
    IterExprAdd(Box<IterExprWireNumber>, Box<IterExprWireNumber>),
    // Subtraction of iterator Expression
    IterExprSub(Box<IterExprWireNumber>, Box<IterExprWireNumber>),
    // Multiplication of iterator Expressions
    IterExprMul(Box<IterExprWireNumber>, Box<IterExprWireNumber>),
    // Division by a constant
    IterExprDivConst(Box<IterExprWireNumber>, u64),
}

use IterExprWireNumber::*;
use crate::sieve_ir_generated::sieve_ir::{IterExprWireListElementU, IterExprWireListElementArgs};

impl<'a> TryFrom<g::IterExprWireNumber<'a>> for IterExprWireNumber {
    type Error = Box<dyn Error>;

    fn try_from(iter_expr: g::IterExprWireNumber) -> Result<IterExprWireNumber> {
        Ok(match iter_expr.value_type() {
            g::IterExpr::NONE => return Err("Unknown Iterator Expression type".into()),
            g::IterExpr::IterExprConst => {
                let value = iter_expr.value_as_iter_expr_const().unwrap();
                IterExprConst(value.value())
            }

            g::IterExpr::IterExprName => {
                let value = iter_expr.value_as_iter_expr_name().unwrap();
                IterExprName(
                    value.name().ok_or("IterExpr: No name given")?.into()
                )
            }
            g::IterExpr::IterExprAdd => {
                let value = iter_expr.value_as_iter_expr_add().unwrap();

                IterExprAdd(
                    Box::new(IterExprWireNumber::try_from(value.left().ok_or("Missing left operand")?)?),
                    Box::new(IterExprWireNumber::try_from(value.right().ok_or("IterExprAdd: No right expression given")?)?),
                )
            }

            g::IterExpr::IterExprSub => {
                let value = iter_expr.value_as_iter_expr_sub().unwrap();

                IterExprSub(
                    Box::new(IterExprWireNumber::try_from(value.left().ok_or("IterExprSub: No left expression given")?)?),
                    Box::new(IterExprWireNumber::try_from(value.right().ok_or("IterExprub: No right expression given")?)?),
                )
            }
            g::IterExpr::IterExprMul => {
                let value = iter_expr.value_as_iter_expr_mul().unwrap();

                IterExprMul(
                    Box::new(IterExprWireNumber::try_from(value.left().ok_or("IterExprMul: No left expression given")?)?),
                    Box::new(IterExprWireNumber::try_from(value.right().ok_or("IterExprMul: No right expression given")?)?),
                )
            }
            g::IterExpr::IterExprDivConst => {
                let value = iter_expr.value_as_iter_expr_div_const().unwrap();

                IterExprDivConst(
                    Box::new(IterExprWireNumber::try_from(value.numer().ok_or("IterExprDivConst: No numerator expression given")?)?),
                    value.denom(),
                )
            }
        })
    }
}

impl IterExprWireNumber {
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::IterExprWireNumber<'bldr>> {
        match self {
            IterExprConst(val) => {
                let g_ite_const = g::IterExprConst::create(
                    builder,
                    &g::IterExprConstArgs {
                        value: *val,
                    }
                );

                g::IterExprWireNumber::create(
                    builder,
                    &g::IterExprWireNumberArgs {
                        value_type: g::IterExpr::IterExprConst,
                        value: Some(g_ite_const.as_union_value())
                    }
                )
            }

            IterExprName(name) => {
                let g_name = builder.create_string(name);
                let g_ite_name = g::IterExprName::create(
                    builder,
                    &g::IterExprNameArgs {
                        name: Some(g_name),
                    }
                );

                g::IterExprWireNumber::create(
                    builder,
                    &g::IterExprWireNumberArgs {
                        value_type: g::IterExpr::IterExprName,
                        value: Some(g_ite_name.as_union_value())
                    }
                )
            }

            IterExprAdd(left, right) => {
                let g_ite_left  = left.build(builder);
                let g_ite_right = right.build(builder);
                let g_ite_add = g::IterExprAdd::create(
                    builder,
                    &g::IterExprAddArgs {
                        left: Some(g_ite_left),
                        right: Some(g_ite_right),
                    }
                );

                g::IterExprWireNumber::create(
                    builder,
                    &g::IterExprWireNumberArgs {
                        value_type: g::IterExpr::IterExprAdd,
                        value: Some(g_ite_add.as_union_value())
                    }
                )
            }

            IterExprSub(left, right) => {
                let g_ite_left  = left.build(builder);
                let g_ite_right = right.build(builder);
                let g_ite_sub = g::IterExprSub::create(
                    builder,
                    &g::IterExprSubArgs {
                        left: Some(g_ite_left),
                        right: Some(g_ite_right),
                    }
                );

                g::IterExprWireNumber::create(
                    builder,
                    &g::IterExprWireNumberArgs {
                        value_type: g::IterExpr::IterExprSub,
                        value: Some(g_ite_sub.as_union_value())
                    }
                )
            }

            IterExprMul(left, right) => {
                let g_ite_left  = left.build(builder);
                let g_ite_right = right.build(builder);
                let g_ite_mul = g::IterExprMul::create(
                    builder,
                    &g::IterExprMulArgs {
                        left: Some(g_ite_left),
                        right: Some(g_ite_right),
                    }
                );

                g::IterExprWireNumber::create(
                    builder,
                    &g::IterExprWireNumberArgs {
                        value_type: g::IterExpr::IterExprMul,
                        value: Some(g_ite_mul.as_union_value())
                    }
                )
            }
            IterExprDivConst(numer, denom) => {
                let g_ite_numer  = numer.build(builder);
                let g_ite_divc = g::IterExprDivConst::create(
                    builder,
                    &g::IterExprDivConstArgs {
                        numer: Some(g_ite_numer),
                        denom: *denom,
                    }
                );

                g::IterExprWireNumber::create(
                    builder,
                    &g::IterExprWireNumberArgs {
                        value_type: g::IterExpr::IterExprDivConst,
                        value: Some(g_ite_divc.as_union_value())
                    }
                )
            }
        }
    }
}

// ***********************************
//
//     IterExprWireListElement
//
// ***********************************
#[derive(Clone, Debug, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum IterExprListElement {
    Single(IterExprWireNumber),
    Range(IterExprWireNumber, IterExprWireNumber),
}
use IterExprListElement::*;

pub type IterExprList = Vec<IterExprListElement>;

impl<'a> TryFrom<g::IterExprWireListElement<'a>> for IterExprListElement {
    type Error = Box<dyn Error>;

    fn try_from(element: g::IterExprWireListElement<'a>) -> Result<Self> {
        Ok(match element.element_type() {
            IterExprWireListElementU::NONE => return Err("Unknown type in IterExprWireListElement".into()),
            IterExprWireListElementU::IterExprWireNumber => {
                Single(IterExprWireNumber::try_from(element.element_as_iter_expr_wire_number().unwrap())?)
            },
            IterExprWireListElementU::IterExprWireRange => {
                let range = element.element_as_iter_expr_wire_range().unwrap();
                Range(
                    IterExprWireNumber::try_from(range.first().ok_or("Missing first value of range")?)?,
                    IterExprWireNumber::try_from(range.last().ok_or("Missing last value of range")?)?,
                )
            }
        })
    }
}

impl IterExprListElement {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::IterExprWireListElement<'bldr>> {
        match self {
            Single(iter_expr) => {
                let g_iter_expr = iter_expr.build(builder);

                g::IterExprWireListElement::create(
                    builder,
                    &IterExprWireListElementArgs {
                        element_type: IterExprWireListElementU::IterExprWireNumber,
                        element: Some(g_iter_expr.as_union_value()),
                    }
                )
            }
            Range(first, last) => {
                let g_first = first.build(builder);
                let g_last = last.build(builder);

                let g_range = g::IterExprWireRange::create(
                    builder,
                    &g::IterExprWireRangeArgs {
                        first: Some(g_first),
                        last: Some(g_last),
                    }
                );

                g::IterExprWireListElement::create(
                    builder,
                    &IterExprWireListElementArgs {
                        element_type: IterExprWireListElementU::IterExprWireRange,
                        element: Some(g_range.as_union_value()),
                    }
                )

            }
        }
    }
}

impl<'a> TryFrom<g::IterExprWireList<'a>> for IterExprList {
    type Error = Box<dyn Error>;

    fn try_from(list: g::IterExprWireList<'a>) -> Result<Self> {
        let fbs_vector = list.elements().ok_or("Missing wire list")?;
        let mut elements = vec![];
        for i in 0..fbs_vector.len() {
            let a = fbs_vector.get(i);
            elements.push(IterExprListElement::try_from(a)?);
        }
        Ok(IterExprList::from(elements))
    }
}


/// Add a vector of this structure into a Flatbuffers message builder.
pub fn build_iterexpr_list<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    elements: &'args [IterExprListElement],
) -> WIPOffset<g::IterExprWireList<'bldr>> {
    let g_elements: Vec<_> = elements.iter().map(|element| element.build(builder)).collect();
    let g_vector = builder.create_vector(&g_elements);

    g::IterExprWireList::create(
        builder,
        &g::IterExprWireListArgs {
            elements: Some(g_vector)
        }
    )
}


/// This function evaluates an iterator expression, replacing the iterator strings by their value
/// given in the 'known_iterators' parameter.
pub fn evaluate_iterexpr(iter_expr: IterExprWireNumber, known_iterators: &HashMap<String, WireId>) -> Result<WireId> {
    Ok(match iter_expr {
        IterExprConst(val) => val,
        IterExprName(name) => {
            *(known_iterators.get(&name).ok_or(format!("Unknown iterator name {}", &name))?)
        },
        IterExprAdd(left, right) => {
            evaluate_iterexpr(*left, known_iterators)? + evaluate_iterexpr(*right, known_iterators)?
        }
        IterExprSub(left, right) => {
            evaluate_iterexpr(*left, known_iterators)? - evaluate_iterexpr(*right, known_iterators)?
        }
        IterExprMul(left, right) => {
            evaluate_iterexpr(*left, known_iterators)? * evaluate_iterexpr(*right, known_iterators)?
        }
        IterExprDivConst(numer, denom) => {
            evaluate_iterexpr(*numer, known_iterators)? / denom
        }
    })
}