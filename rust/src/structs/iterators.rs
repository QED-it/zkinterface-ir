use crate::sieve_ir_generated::sieve_ir as generated;
use crate::{Result, WireId};
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;

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

use crate::sieve_ir_generated::sieve_ir::{IterExprWireListElementArgs, IterExprWireListElementU};
use IterExprWireNumber::*;

impl<'a> TryFrom<generated::IterExprWireNumber<'a>> for IterExprWireNumber {
    type Error = Box<dyn Error>;

    fn try_from(iter_expr: generated::IterExprWireNumber) -> Result<IterExprWireNumber> {
        Ok(match iter_expr.value_type() {
            generated::IterExpr::NONE => return Err("Unknown Iterator Expression type".into()),
            generated::IterExpr::IterExprConst => {
                let value = iter_expr.value_as_iter_expr_const().unwrap();
                IterExprConst(value.value())
            }

            generated::IterExpr::IterExprName => {
                let value = iter_expr.value_as_iter_expr_name().unwrap();
                IterExprName(value.name().ok_or("IterExpr: No name given")?.into())
            }
            generated::IterExpr::IterExprAdd => {
                let value = iter_expr.value_as_iter_expr_add().unwrap();

                IterExprAdd(
                    Box::new(IterExprWireNumber::try_from(
                        value.left().ok_or("Missing left operand")?,
                    )?),
                    Box::new(IterExprWireNumber::try_from(
                        value
                            .right()
                            .ok_or("IterExprAdd: No right expression given")?,
                    )?),
                )
            }

            generated::IterExpr::IterExprSub => {
                let value = iter_expr.value_as_iter_expr_sub().unwrap();

                IterExprSub(
                    Box::new(IterExprWireNumber::try_from(
                        value
                            .left()
                            .ok_or("IterExprSub: No left expression given")?,
                    )?),
                    Box::new(IterExprWireNumber::try_from(
                        value
                            .right()
                            .ok_or("IterExprub: No right expression given")?,
                    )?),
                )
            }
            generated::IterExpr::IterExprMul => {
                let value = iter_expr.value_as_iter_expr_mul().unwrap();

                IterExprMul(
                    Box::new(IterExprWireNumber::try_from(
                        value
                            .left()
                            .ok_or("IterExprMul: No left expression given")?,
                    )?),
                    Box::new(IterExprWireNumber::try_from(
                        value
                            .right()
                            .ok_or("IterExprMul: No right expression given")?,
                    )?),
                )
            }
            generated::IterExpr::IterExprDivConst => {
                let value = iter_expr.value_as_iter_expr_div_const().unwrap();

                IterExprDivConst(
                    Box::new(IterExprWireNumber::try_from(
                        value
                            .numer()
                            .ok_or("IterExprDivConst: No numerator expression given")?,
                    )?),
                    value.denom(),
                )
            }
        })
    }
}

impl IterExprWireNumber {
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::IterExprWireNumber<'bldr>> {
        match self {
            IterExprConst(val) => {
                let g_ite_const = generated::IterExprConst::create(
                    builder,
                    &generated::IterExprConstArgs { value: *val },
                );

                generated::IterExprWireNumber::create(
                    builder,
                    &generated::IterExprWireNumberArgs {
                        value_type: generated::IterExpr::IterExprConst,
                        value: Some(g_ite_const.as_union_value()),
                    },
                )
            }

            IterExprName(name) => {
                let g_name = builder.create_string(name);
                let g_ite_name = generated::IterExprName::create(
                    builder,
                    &generated::IterExprNameArgs { name: Some(g_name) },
                );

                generated::IterExprWireNumber::create(
                    builder,
                    &generated::IterExprWireNumberArgs {
                        value_type: generated::IterExpr::IterExprName,
                        value: Some(g_ite_name.as_union_value()),
                    },
                )
            }

            IterExprAdd(left, right) => {
                let g_ite_left = left.build(builder);
                let g_ite_right = right.build(builder);
                let g_ite_add = generated::IterExprAdd::create(
                    builder,
                    &generated::IterExprAddArgs {
                        left: Some(g_ite_left),
                        right: Some(g_ite_right),
                    },
                );

                generated::IterExprWireNumber::create(
                    builder,
                    &generated::IterExprWireNumberArgs {
                        value_type: generated::IterExpr::IterExprAdd,
                        value: Some(g_ite_add.as_union_value()),
                    },
                )
            }

            IterExprSub(left, right) => {
                let g_ite_left = left.build(builder);
                let g_ite_right = right.build(builder);
                let g_ite_sub = generated::IterExprSub::create(
                    builder,
                    &generated::IterExprSubArgs {
                        left: Some(g_ite_left),
                        right: Some(g_ite_right),
                    },
                );

                generated::IterExprWireNumber::create(
                    builder,
                    &generated::IterExprWireNumberArgs {
                        value_type: generated::IterExpr::IterExprSub,
                        value: Some(g_ite_sub.as_union_value()),
                    },
                )
            }

            IterExprMul(left, right) => {
                let g_ite_left = left.build(builder);
                let g_ite_right = right.build(builder);
                let g_ite_mul = generated::IterExprMul::create(
                    builder,
                    &generated::IterExprMulArgs {
                        left: Some(g_ite_left),
                        right: Some(g_ite_right),
                    },
                );

                generated::IterExprWireNumber::create(
                    builder,
                    &generated::IterExprWireNumberArgs {
                        value_type: generated::IterExpr::IterExprMul,
                        value: Some(g_ite_mul.as_union_value()),
                    },
                )
            }
            IterExprDivConst(numer, denom) => {
                let g_ite_numer = numer.build(builder);
                let g_ite_divc = generated::IterExprDivConst::create(
                    builder,
                    &generated::IterExprDivConstArgs {
                        numer: Some(g_ite_numer),
                        denom: *denom,
                    },
                );

                generated::IterExprWireNumber::create(
                    builder,
                    &generated::IterExprWireNumberArgs {
                        value_type: generated::IterExpr::IterExprDivConst,
                        value: Some(g_ite_divc.as_union_value()),
                    },
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

impl<'a> TryFrom<generated::IterExprWireListElement<'a>> for IterExprListElement {
    type Error = Box<dyn Error>;

    fn try_from(element: generated::IterExprWireListElement<'a>) -> Result<Self> {
        Ok(match element.element_type() {
            IterExprWireListElementU::NONE => {
                return Err("Unknown type in IterExprWireListElement".into())
            }
            IterExprWireListElementU::IterExprWireNumber => Single(IterExprWireNumber::try_from(
                element.element_as_iter_expr_wire_number().unwrap(),
            )?),
            IterExprWireListElementU::IterExprWireRange => {
                let range = element.element_as_iter_expr_wire_range().unwrap();
                Range(
                    IterExprWireNumber::try_from(
                        range.first().ok_or("Missing first value of range")?,
                    )?,
                    IterExprWireNumber::try_from(
                        range.last().ok_or("Missing last value of range")?,
                    )?,
                )
            }
        })
    }
}

impl IterExprListElement {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr>(
        &self,
        builder: &mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<generated::IterExprWireListElement<'bldr>> {
        match self {
            Single(iter_expr) => {
                let g_iter_expr = iter_expr.build(builder);

                generated::IterExprWireListElement::create(
                    builder,
                    &IterExprWireListElementArgs {
                        element_type: IterExprWireListElementU::IterExprWireNumber,
                        element: Some(g_iter_expr.as_union_value()),
                    },
                )
            }
            Range(first, last) => {
                let g_first = first.build(builder);
                let g_last = last.build(builder);

                let g_range = generated::IterExprWireRange::create(
                    builder,
                    &generated::IterExprWireRangeArgs {
                        first: Some(g_first),
                        last: Some(g_last),
                    },
                );

                generated::IterExprWireListElement::create(
                    builder,
                    &IterExprWireListElementArgs {
                        element_type: IterExprWireListElementU::IterExprWireRange,
                        element: Some(g_range.as_union_value()),
                    },
                )
            }
        }
    }
}

impl<'a> TryFrom<generated::IterExprWireList<'a>> for IterExprList {
    type Error = Box<dyn Error>;

    fn try_from(list: generated::IterExprWireList<'a>) -> Result<Self> {
        let fbs_vector = list.elements().ok_or("Missing wire list")?;
        let mut elements = vec![];
        for i in 0..fbs_vector.len() {
            let a = fbs_vector.get(i);
            elements.push(IterExprListElement::try_from(a)?);
        }
        Ok(elements)
    }
}

/// Add a vector of this structure into a Flatbuffers message builder.
pub fn build_iterexpr_list<'bldr>(
    builder: &mut FlatBufferBuilder<'bldr>,
    elements: &[IterExprListElement],
) -> WIPOffset<generated::IterExprWireList<'bldr>> {
    let g_elements: Vec<_> = elements
        .iter()
        .map(|element| element.build(builder))
        .collect();
    let g_vector = builder.create_vector(&g_elements);

    generated::IterExprWireList::create(
        builder,
        &generated::IterExprWireListArgs {
            elements: Some(g_vector),
        },
    )
}

/// This function evaluates an iterator expression, replacing the iterator strings by their value
/// given in the 'known_iterators' parameter.
pub fn evaluate_iterexpr(
    iter_expr: &IterExprWireNumber,
    known_iterators: &HashMap<String, WireId>,
) -> Result<WireId> {
    Ok(match iter_expr {
        IterExprConst(val) => *val,
        IterExprName(name) => {
            *(known_iterators
                .get(name)
                .ok_or(format!("Unknown iterator name {}", &name))?)
        }
        IterExprAdd(left, right) => {
            evaluate_iterexpr(left, known_iterators)? + evaluate_iterexpr(right, known_iterators)?
        }
        IterExprSub(left, right) => {
            evaluate_iterexpr(left, known_iterators)? - evaluate_iterexpr(right, known_iterators)?
        }
        IterExprMul(left, right) => {
            evaluate_iterexpr(left, known_iterators)? * evaluate_iterexpr(right, known_iterators)?
        }
        IterExprDivConst(numer, denom) => evaluate_iterexpr(numer, known_iterators)? / denom,
    })
}

/// This function evaluates an iterator expression, replacing the iterator strings by their value
/// given in the 'known_iterators' parameter.
pub fn evaluate_iterexpr_listelement(
    iter_expr_element: &IterExprListElement,
    known_iterators: &HashMap<String, WireId>,
) -> Result<Vec<WireId>> {
    let ret = match iter_expr_element {
        Single(val) => vec![evaluate_iterexpr(val, known_iterators)?],
        Range(first, last) => (evaluate_iterexpr(first, known_iterators)?
            ..=evaluate_iterexpr(last, known_iterators)?)
            .into_iter()
            .collect(),
    };

    Ok(ret)
}

/// This function evaluates an iterator expression, replacing the iterator strings by their value
/// given in the 'known_iterators' parameter.
pub fn evaluate_iterexpr_list(
    iter_expr_list: &IterExprList,
    known_iterators: &HashMap<String, WireId>,
) -> Vec<WireId> {
    iter_expr_list
        .iter()
        .flat_map(|element| {
            evaluate_iterexpr_listelement(element, known_iterators)
                .unwrap_or_else(|e| panic!("{}", e.to_string()))
        })
        .collect()
}
