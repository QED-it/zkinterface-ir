use crate::Result;
use flatbuffers::{FlatBufferBuilder, WIPOffset};
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;
use std::collections::{HashMap};

use super::gates::Gate;
use super::header::Header;
use crate::sieve_ir_generated::sieve_ir as g;
use crate::structs::function::Function;

// Masks to handle Arithmetic Gates internally
pub const ADD:   u16 = 0x0001;
pub const ADDC:  u16 = 0x0002;
pub const MUL:   u16 = 0x0004;
pub const MULC:  u16 = 0x0008;
pub const ARITH: u16 = ADD | ADDC | MUL | MULC;

// Masks to handle Boolean Gates internally
pub const XOR:  u16 = 0x0100;
pub const AND:  u16 = 0x0200;
pub const NOT:  u16 = 0x0400;
pub const BOOL: u16 = XOR | AND | NOT;


// Masks to handle Toggle features internally
pub const FUNCTION:              u16 = 0x1000;
pub const FOR:                   u16 = 0x2000;
pub const SWITCH:                u16 = 0x4000;
pub const FOR_FUNCTION_SWITCH:   u16 = FOR|FUNCTION|SWITCH;
pub const SIMPLE:                u16 = 0x0000;

#[derive(Clone, Default, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub struct Relation {
    pub header:    Header,
    pub gate_mask: u16,
    pub feat_mask: u16,
    pub functions: Vec<Function>,
    pub gates: Vec<Gate>,
}

impl<'a> TryFrom<g::Relation<'a>> for Relation {
    type Error = Box<dyn Error>;

    /// Convert from Flatbuffers references to owned structure.
    fn try_from(g_relation: g::Relation) -> Result<Relation> {
        let g_gates = g_relation.directives().ok_or("Missing directives")?;
        let functions = if let Some(g_functions) = g_relation.functions() {
            Function::try_from_vector(g_functions)?
        } else {
            vec![]
        };

        Ok(Relation {
            header: Header::try_from(g_relation.header())?,
            gate_mask: parse_gate_set(g_relation.gateset().ok_or("Missing gateset description")?)?,
            feat_mask: parse_feature_toggle(g_relation.features().ok_or("Missing feature toggles")?)?,
            functions,
            gates: Gate::try_from_vector(g_gates)?,
        })
    }
}

impl<'a> TryFrom<&'a [u8]> for Relation {
    type Error = Box<dyn Error>;

    fn try_from(buffer: &'a [u8]) -> Result<Relation> {
        Relation::try_from(
            g::get_size_prefixed_root_as_root(&buffer)
                .message_as_relation()
                .ok_or("Not a Relation message.")?,
        )
    }
}

impl Relation {
    /// Add this structure into a Flatbuffers message builder.
    pub fn build<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
        &'args self,
        builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
    ) -> WIPOffset<g::Root<'bldr>> {
        let header = Some(self.header.build(builder));
        let directives = Gate::build_vector(builder, &self.gates);
        let functions = Function::build_vector(builder, &self.functions);

        let gateset = build_gate_set(self.gate_mask, builder);
        let features = build_feature_toggle(self.feat_mask, builder);

        let relation = g::Relation::create(
            builder,
            &g::RelationArgs {
                header,
                gateset: Some(gateset),
                features: Some(features),
                functions: Some(functions),
                directives: Some(directives),
            },
        );

        g::Root::create(
            builder,
            &g::RootArgs {
                message_type: g::Message::Relation,
                message: Some(relation.as_union_value()),
            },
        )
    }

    /// Writes this Relation as a Flatbuffers message into the provided buffer.
    ///
    /// # Examples
    /// ```
    /// use zki_sieve::Relation;
    /// use std::convert::TryFrom;
    ///
    /// let relation = Relation::default();
    /// let mut buf = Vec::<u8>::new();
    /// relation.write_into(&mut buf).unwrap();
    /// let relation2 = Relation::try_from(&buf[..]).unwrap();
    /// assert_eq!(relation, relation2);
    /// ```
    pub fn write_into(&self, writer: &mut impl Write) -> Result<()> {
        let mut builder = FlatBufferBuilder::new();
        let message = self.build(&mut builder);
        g::finish_size_prefixed_root_buffer(&mut builder, message);
        writer.write_all(builder.finished_data())?;
        Ok(())
    }
}

/// This helper function will parse the string stored in a FBS Relation::gateset field
/// and will translate it into a internal mask handling the same information.
pub fn parse_gate_set_string(gateset: String) -> Result<u16> {

    let mut ret: u16 = 0x0000;
    // for substr in gateset.into().split(',') {
    for substr in gateset.split(',') {
        match &substr.replace(" ", "")[..] {
            "arithmetic" => return Ok(ARITH),
            "@add"       => ret |= ADD,
            "@addc"      => ret |= ADDC,
            "@mul"       => ret |= MUL,
            "@mulc"      => ret |= MULC,

            "boolean"    => return Ok(BOOL),
            "@xor"       => ret |= XOR,
            "@not"       => ret |= NOT,
            "@and"       => ret |= AND,

            ""           => {/* DO NOTHING */}

            // _ => return Err(format!("Unable to parse the following gateset: {}", gateset.into()).into()),
            _ => return Err(format!("Unable to parse the following gateset: {}", gateset).into()),
        }
    }
    Ok(ret)
}

pub fn parse_gate_set(gateset: impl Into<String> + Copy) -> Result<u16> {
    parse_gate_set_string(gateset.into())
}

    
/// This functions exports a Relation::gateset string into a FBS binary message.
fn build_gate_set<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    gateset: u16,
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
) -> WIPOffset<&'args str> {
    builder.create_string(&create_gateset_string(gateset))
}

/// This helper function will translate the gateset mask into an IR-compliant string by
/// concatenating associated strings to activated gatesets.
fn create_gateset_string(gateset: u16) -> String {
    let mut gateset_val = gateset;
    let mut ret = String::new();
    while gateset_val != 0 {
        match gateset_val {
            _ if contains_feature(gateset_val,ARITH)  => return "arithmetic".into(),
            _ if contains_feature(gateset_val,BOOL)   => return "boolean".into(),
            _ if contains_feature(gateset_val, ADD)   => {
                ret += "@add,";
                gateset_val ^= ADD;
            },
            _ if contains_feature(gateset_val, ADDC)  => {
                ret += "@addc,";
                gateset_val ^= ADDC;
            },
            _ if contains_feature(gateset_val, MUL)   => {
                ret += "@mul,";
                gateset_val ^= MUL;
            },
            _ if contains_feature(gateset_val, MULC)  => {
                ret += "@mulc,";
                gateset_val ^= MULC;
            },

            _ if contains_feature(gateset_val, XOR)   => {
                ret += "@xor,";
                gateset_val ^= XOR;
            },
            _ if contains_feature(gateset_val, NOT)   => {
                ret += "@not,";
                gateset_val ^= NOT;
            },
            _ if contains_feature(gateset_val, AND)   => {
                ret += "@and,";
                gateset_val ^= AND;
            },

            _ => {/* DO NOTHING */}
        }
    }

    ret
}

/// This helper function will parse the string stored in a FBS Relation::feature field
/// and will translate it into a internal mask handling the same information.
fn parse_feature_toggle(features: impl Into<String>) -> Result<u16> {

    let mut ret: u16 = 0x0000;
    for substr in features.into().split(',') {
        match &substr.replace(" ", "")[..] {
            "@function" => ret |= FUNCTION,
            "@for"      => ret |= FOR,
            "@switch"   => ret |= SWITCH,
            "simple"    => return Ok(SIMPLE),
            ""          => {/* DO NOTHING */}
            _ => return Err(format!("Unable to parse following feature toggles {}", substr).into()),
        }
    }
    Ok(ret)
}
/// This functions exports a Relation::features string into a FBS binary message.
fn build_feature_toggle<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
    features: u16,
    builder: &'mut_bldr mut FlatBufferBuilder<'bldr>,
) -> WIPOffset<&'args str> {
    builder.create_string(&create_feature_string(features))
}

/// This helper function will translate the features mask into an IR-compliant string by
/// concatenating associated strings to activated features.
fn create_feature_string(features: u16) -> String {
    if (features & FOR_FUNCTION_SWITCH) == 0 {
        return "simple".into();
    }
    let mut features_val = features & FOR_FUNCTION_SWITCH;
    let mut ret = String::new();
    while features_val != 0 {
        match features_val {
            _ if contains_feature(features_val, FOR)      => {
                ret += "@for,";
                features_val ^= FOR;
            },
            _ if contains_feature(features_val, SWITCH)   => {
                ret += "@switch,";
                features_val ^= SWITCH;
            },
            _ if contains_feature(features_val, FUNCTION) => {
                ret += "@function,";
                features_val ^= FUNCTION;
            },

            _ => {/* DO NOTHING */}
        }
    }
    ret
}

/// Helper function to determine whether a given 'feature' is included in the given 'feature_set'.
/// It's done by checking that "(feature_set & feature) == feature".
pub fn contains_feature(feature_set: u16, feature: u16) -> bool {
    (feature_set & feature) == feature
}

/// Get the known functions
pub fn get_known_functions(relation:&Relation) -> HashMap<String, (usize, usize, usize, usize, Vec<Gate>)>{
    let mut map = HashMap::new();
    for f in relation.functions.iter() {
	map.insert(f.name.clone(),(f.output_count,f.input_count,f.instance_count, f.witness_count, f.body.clone()));
    }
    map
}


#[test]
fn test_parse_gate_set() -> Result<()> {

    assert_eq!(parse_gate_set("arithmetic").unwrap(), ARITH);
    assert_eq!(parse_gate_set("boolean").unwrap(), BOOL);
    assert_eq!(parse_gate_set("arithmetic,@add").unwrap(), ARITH | ADD);
    assert_eq!(parse_gate_set("@add,@addc,").unwrap(), ADD|ADDC);
    assert_eq!(parse_gate_set("@add , @mulc , @mul ").unwrap(), ADD|MULC|MUL);
    assert_eq!(parse_gate_set("@add,@add,@mul").unwrap(), ADD|MUL);
    assert_eq!(parse_gate_set("@add,@addc,@mulc,@mul").unwrap(), ARITH);
    
    assert_eq!(parse_gate_set("boolean").unwrap(), BOOL);
    assert_eq!(parse_gate_set("boolean,@xor").unwrap(), BOOL|XOR);
    assert_eq!(parse_gate_set("@xor,@and,@not").unwrap(), XOR|AND|NOT);
    assert_eq!(parse_gate_set(" @xor ,@and , @not").unwrap(), XOR|AND|NOT);
    assert_eq!(parse_gate_set("@xor,@and,@not").unwrap(), BOOL);
    
    assert_eq!(parse_gate_set("boolean,arithmetic").unwrap(), BOOL);
    assert_eq!(parse_gate_set("arithmetic,boolean").unwrap(), ARITH);

    assert!(parse_gate_set("test").is_err());
    assert!(parse_gate_set("@addc,@test").is_err());
    assert!(parse_gate_set("add").is_err());
    assert!(parse_gate_set("@and,test").is_err());
    assert!(parse_gate_set("and").is_err());

    Ok(())
}

#[test]
fn test_create_gateset_string() -> Result<()> {

    assert_eq!(create_gateset_string(ARITH), "arithmetic");
    assert_eq!(create_gateset_string(ARITH | ADD), "arithmetic");
    assert_eq!(create_gateset_string(ADD|ADDC), "@add,@addc,");
    assert_eq!(create_gateset_string(ADD|MULC|MUL), "@add,@mul,@mulc,");
    assert_eq!(create_gateset_string(ADD|MUL), "@add,@mul,");

    assert_eq!(create_gateset_string(BOOL), "boolean");
    assert_eq!(create_gateset_string(BOOL|XOR), "boolean");
    assert_eq!(create_gateset_string(XOR|AND), "@xor,@and,");
    assert_eq!(create_gateset_string(XOR|NOT), "@xor,@not,");
    assert_eq!(create_gateset_string(XOR|AND|NOT), "boolean");

    Ok(())
}

#[test]
fn test_parse_feature_toggles() -> Result<()> {
    assert_eq!(parse_feature_toggle("@for,@switch,@function").unwrap(), FOR|SWITCH|FUNCTION);
    assert_eq!(parse_feature_toggle("@for,@switch,").unwrap(), FOR|SWITCH);
    assert_eq!(parse_feature_toggle("@for,@function").unwrap(), FOR|FUNCTION);
    assert_eq!(parse_feature_toggle("@switch,@function").unwrap(), SWITCH|FUNCTION);
    assert_eq!(parse_feature_toggle("simple").unwrap(), SIMPLE);

    assert_eq!(parse_feature_toggle("simple,@switch,@function").unwrap(), SIMPLE);
    assert_eq!(parse_feature_toggle("@for,simple,@function").unwrap(), SIMPLE);

    assert_eq!(parse_feature_toggle("@for , @switch ,@function").unwrap(), FOR|SWITCH|FUNCTION);
    assert_eq!(parse_feature_toggle("@for, @switch , @function ").unwrap(), FOR|SWITCH|FUNCTION);

    assert!(parse_feature_toggle("for").is_err());
    assert!(parse_feature_toggle("@for, test").is_err());

    Ok(())
}

#[test]
fn test_create_feature_toggles() -> Result<()> {
    assert_eq!(create_feature_string(FOR|SWITCH|FUNCTION), "@for,@switch,@function,");
    assert_eq!(create_feature_string(FOR|FUNCTION), "@for,@function,");
    assert_eq!(create_feature_string(FOR|SWITCH), "@for,@switch,");
    assert_eq!(create_feature_string(SWITCH|FUNCTION), "@switch,@function,");
    assert_eq!(create_feature_string(FOR), "@for,");
    assert_eq!(create_feature_string(SWITCH), "@switch,");
    assert_eq!(create_feature_string(FUNCTION), "@function,");

    assert_eq!(create_feature_string(SIMPLE), "simple");
    assert_eq!(create_feature_string(0), "simple");

    Ok(())
}
