use crate::{Gate, Header, Instance, Message, Relation, Result, Witness, WireId};
use num_traits::identities::{One, Zero};
use std::collections::{HashMap, VecDeque};
use crate::structs::wire::expand_wirelist;
use crate::structs::function::{CaseInvoke, ForLoopBody};
use crate::structs::iterators::evaluate_iterexpr_list;
use num_bigint::BigUint;
use std::ops::{BitAnd, BitXor, Shr};
use crate::structs::relation::{contains_feature, BOOL};

pub trait ZKInterpreter {
    type Wire: 'static + Clone;
    type FieldElement: 'static + Clone + Zero + One + Default;

    fn from_bytes_le(val: &[u8]) -> Result<Self::FieldElement>;
    // If the field is not compatible with this ZKInterpreter, then it should return Err
    fn set_field(&mut self, modulus: &[u8], degree: u32) -> Result<()>;

    fn one(&self) -> Self::Wire;
    fn minus_one(&self) -> Self::Wire;
    fn zero(&self) -> Self::Wire;

    fn constant(&mut self, val: Self::FieldElement) -> Result<Self::Wire>;
    fn assert_zero(&mut self, wire: &Self::Wire) -> Result<()>;
    fn copy(&mut self, source: &Self::Wire) -> Result<Self::Wire>;

    fn add(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    fn multiply(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    fn add_constant(&mut self, a: &Self::Wire, b: Self::FieldElement) -> Result<Self::Wire>;
    fn mul_constant(&mut self, a: &Self::Wire, b: Self::FieldElement) -> Result<Self::Wire>;

    fn and(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    fn xor(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire>;
    fn not(&mut self, a: &Self::Wire) -> Result<Self::Wire>;

    fn instance(&mut self, val: Self::FieldElement) -> Result<Self::Wire>;
    fn witness(&mut self, val: Option<Self::FieldElement>) -> Result<Self::Wire>;

    fn free(&mut self, wire: &Self::Wire) -> Result<()>;
}

/// This macro is used to evaluate a 'multiplication' in either the arithmetic case or the boolean,
/// where it's replaced by an AND operation.
macro_rules! as_mul {
    ($interp:expr, $a:expr, $b:expr, $is_bool:expr) => {{
        if $is_bool {
            $interp.and($a, $b)
        } else {
            $interp.multiply($a, $b)
        }
    }};
}

/// This macro is used to evaluate an 'addition' in either the arithmetic case or the boolean,
/// where it's replaced by an XOR operation.
macro_rules! as_add {
    ($interp:expr, $a:expr, $b:expr, $is_bool:expr) => {{
        if $is_bool {
            $interp.xor($a, $b)
        } else {
            $interp.add($a, $b)
        }
    }};
}

// I::FieldElement : BigUint
pub struct Evaluator<I: ZKInterpreter> {
    values: HashMap<WireId, I::Wire>,
    modulus: BigUint,
    instance_queue: VecDeque<I::FieldElement>,
    witness_queue: VecDeque<I::FieldElement>,
    is_boolean: bool,

    // name => (instance_nbr, witness_nbr, subcircuit)
    known_functions: HashMap<String, (usize, usize, Vec<Gate>)>,

    verified_at_least_one_gate: bool,
    found_error: Option<String>,
}

impl<I:ZKInterpreter> Default for Evaluator<I> {
    fn default() -> Self {
        Evaluator {
            values: Default::default(),
            modulus: BigUint::zero(),
            instance_queue: Default::default(),
            witness_queue: Default::default(),
            is_boolean: false,
            known_functions: Default::default(),
            verified_at_least_one_gate: false,
            found_error: None
        }
    }
}

impl<I: ZKInterpreter> Evaluator<I> {
    pub fn from_messages(messages: impl Iterator<Item=Result<Message>>, interp: &mut I) -> Self {
        let mut evaluator = Evaluator::default();
        messages.for_each(|msg| evaluator.ingest_message(&msg.unwrap(), interp));
        evaluator
    }

    pub fn get_violations(self) -> Vec<String> {
        let mut violations = vec![];
        if !self.verified_at_least_one_gate {
            violations.push("Did not receive any gate to verify.".to_string());
        }
        if let Some(err) = self.found_error {
            violations.push(err);
        }
        violations
    }

    pub fn ingest_message(&mut self, msg: &Message, interp: &mut I) {
        if self.found_error.is_some() {
            return;
        }

        match self.ingest_message_(msg, interp) {
            Err(err) => self.found_error = Some(err.to_string()),
            Ok(()) => {}
        }
    }

    fn ingest_message_(&mut self, msg: &Message, interp: &mut I) -> Result<()> {
        match msg {
            Message::Instance(i) => self.ingest_instance(&i, interp),
            Message::Witness(w) => self.ingest_witness(&w, interp),
            Message::Relation(r) => self.ingest_relation(&r, interp),
        }
    }

    fn ingest_header(&mut self, header: &Header, interp: &mut I) -> Result<()> {
        self.modulus = BigUint::from_bytes_le(&header.field_characteristic);
        interp.set_field(&header.field_characteristic, header.field_degree)
    }

    pub fn ingest_instance(&mut self, instance: &Instance, interp: &mut I) -> Result<()> {
        self.ingest_header(&instance.header, interp)?;

        for value in &instance.common_inputs {
            self.instance_queue.push_back(I::from_bytes_le(value)?);
        }
        Ok(())
    }

    pub fn ingest_witness(&mut self, witness: &Witness, interp: &mut I) -> Result<()> {
        self.ingest_header(&witness.header, interp)?;

        for value in &witness.short_witness {
            self.witness_queue.push_back(I::from_bytes_le(value)?);
        }
        Ok(())
    }

    pub fn ingest_relation(&mut self, relation: &Relation, interp: &mut I) -> Result<()> {
        self.ingest_header(&relation.header, interp)?;
        self.is_boolean = contains_feature(relation.gate_mask, BOOL);

        if relation.gates.len() > 0 {
            self.verified_at_least_one_gate = true;
        }

        for f in relation.functions.iter() {
            self.known_functions.insert(f.name.clone(), (f.instance_count, f.witness_count, f.body.clone()));
        }

        let mut known_iterators = HashMap::new();

        for gate in &relation.gates {
            Self::ingest_gate(
                gate,
                interp,
                &mut self.values,
                &self.known_functions,
                &mut known_iterators,
                &self.modulus,
                self.is_boolean,
                &mut self.instance_queue,
                &mut self.witness_queue)?;
        }
        Ok(())
    }

    fn ingest_gate(
        gate: &Gate,
        interp: &mut I,
        scope: &mut HashMap<WireId, I::Wire>,
        known_functions: &HashMap<String, (usize, usize, Vec<Gate>)>,
        known_iterators: &mut HashMap<String, u64>,
        modulus: &BigUint,
        is_boolean: bool,
        instances: &mut VecDeque<I::FieldElement>,
        witnesses: &mut VecDeque<I::FieldElement>,
    ) -> Result<()> {
        use Gate::*;

        macro_rules! get {
            ($wire_id:expr) => {{
                get::<I>(scope, $wire_id)
            }};
        }

        macro_rules! set {
            ($wire_id:expr, $wire_name:expr) => {{
                set::<I>(scope, $wire_id, $wire_name)
            }};
        }

        match gate {
            Constant(out, value) => {
                let wire = interp.constant(I::from_bytes_le(value)?)?;
                set::<I>(scope, *out, wire)?;
            },

            AssertZero(inp) => {
                if interp.assert_zero(get!(*inp)?).is_err() {
                    return Err(format!("Wire_{} should be 0, while it is not", *inp).into());
                }
            }

            Copy(out, inp) => {
                let in_wire = get!(*inp)?;
                let out_wire = interp.copy(in_wire)?;
                set!(*out, out_wire)?;
            }

            Add(out, left, right) => {
                let l = get!(*left)?;
                let r = get!(*right)?;
                let sum = interp.add(l, r)?;
                set!(*out, sum)?;
            }

            Mul(out, left, right) => {
                let l = get!(*left)?;
                let r = get!(*right)?;
                let prod = interp.multiply(l, r)?;
                set!(*out, prod)?;
            }

            AddConstant(out, inp, constant) => {
                let l = get!(*inp)?;
                let r = I::from_bytes_le(constant)?;
                let sum = interp.add_constant(l, r)?;
                set!(*out, sum)?;
            }

            MulConstant(out, inp, constant) => {
                let l = get!(*inp)?;
                let r = I::from_bytes_le(constant)?;
                let prod = interp.mul_constant(l, r)?;
                set!(*out, prod)?;
            }

            And(out, left, right) => {
                let l = get!(*left)?;
                let r = get!(*right)?;
                let and = interp.and(l, r)?;
                set!(*out, and)?;
            }

            Xor(out, left, right) => {
                let l = get!(*left)?;
                let r = get!(*right)?;
                let xor = interp.xor(l, r)?;
                set!(*out, xor)?;
            }

            Not(out, inp) => {
                let val = get!(*inp)?;
                let not = interp.not(val)?;
                set!(*out, not)?;
            }

            Instance(out) => {
                let val = if let Some(inner) = instances.pop_front() {
                    inner
                } else {
                    return Err("Not enough instance to consume".into());
                };
                set_instance(interp, scope, *out, val)?;
            }

            Witness(out) => {
                let val = witnesses.pop_front();
                set_witness(interp, scope, *out, val)?;
            }

            Free(first, last) => {
                let last_value = last.unwrap_or(*first);
                for current in *first..=last_value {
                    interp.free(&remove::<I>(scope, current)?)?;
                }
            }

            Call(name, output_wires, input_wires) => {
                let fonction = known_functions.get(name).ok_or("Unknown function")?;
                let expanded_output = expand_wirelist(output_wires);
                let expanded_input = expand_wirelist(input_wires);
                // in the case of an named call, iterators *ARE NOT* forwarded into inner bodies.
                Self::ingest_subcircuit(
                    &fonction.2,
                    interp,
                    &expanded_output,
                    &expanded_input,
                    scope,
                    known_functions,
                    &mut HashMap::new(),
                    modulus,
                    is_boolean,
                    instances,
                    witnesses,
                )?;
            }

            AnonCall(output_wires, input_wires, _, _, subcircuit) => {
                let expanded_output = expand_wirelist(output_wires);
                let expanded_input = expand_wirelist(input_wires);
                // in the case of an anoncall, iterators *ARE* forwarded into inner bodies.
                Self::ingest_subcircuit(
                    subcircuit,
                    interp,
                    &expanded_output,
                    &expanded_input,
                    scope,
                    known_functions,
                    known_iterators,
                    modulus,
                    is_boolean,
                    instances,
                    witnesses,
                )?;
            }

            For(
                iterator_name,
                start_val,
                end_val,
                _,
                body
            ) => {
                for i in *start_val..=*end_val {
                    known_iterators.insert(iterator_name.clone(), i);

                    match body {
                        ForLoopBody::IterExprCall(name, outputs, inputs) => {
                            let function = known_functions.get(name).ok_or_else(|| "Unknown function")?;
                            let expanded_output = evaluate_iterexpr_list(outputs, known_iterators);
                            let expanded_input = evaluate_iterexpr_list(inputs, known_iterators);
                            Self::ingest_subcircuit(
                                &function.2,
                                interp,
                                &expanded_output,
                                &expanded_input,
                                scope,
                                known_functions,
                                &mut HashMap::new(),
                                modulus,
                                is_boolean,
                                instances,
                                witnesses,
                            )?;
                        }
                        ForLoopBody::IterExprAnonCall(
                            output_wires,
                            input_wires,
                            _, _,
                            subcircuit
                        ) => {
                            let expanded_output = evaluate_iterexpr_list(output_wires, known_iterators);
                            let expanded_input = evaluate_iterexpr_list(input_wires, known_iterators);
                            Self::ingest_subcircuit(
                                subcircuit,
                                interp,
                                &expanded_output,
                                &expanded_input,
                                scope,
                                known_functions,
                                known_iterators,
                                modulus,
                                is_boolean,
                                instances,
                                witnesses,
                            )?;
                        }
                    }
                }
                known_iterators.remove(iterator_name);
            },

            Switch(condition, output_wires, cases, branches) => {
                // unimplemented!();
                //*
                // TODO make it compatible with flattening, maybe, define a new function for it...

                // determine the maximum instance/witness consumption
                let mut max_instance_count: usize = 0;
                let mut max_witness_count : usize = 0;
                for branch in branches.iter() {
                    let (instance_cnt, witness_cnt) = match branch {
                        CaseInvoke::AbstractGateCall(name, _) => {
                            let function = known_functions.get(name).ok_or_else(|| "Unknown function")?;
                            (function.0, function.1)
                        }
                        CaseInvoke::AbstractAnonCall(_, instance_count, witness_count, _) => (*instance_count, *witness_count)
                    };
                    max_instance_count = std::cmp::max(max_instance_count, instance_cnt);
                    max_witness_count = std::cmp::max(max_witness_count, witness_cnt);
                }

                let new_instances: VecDeque<I::FieldElement> = instances.drain(0..max_instance_count).collect::<VecDeque<_>>();
                let new_witnesses: VecDeque<I::FieldElement> = witnesses.drain(0..max_witness_count).collect::<VecDeque<_>>();
                let mut branches_scope = Vec::new();

                let expanded_output = expand_wirelist(output_wires);
                let mut weights = Vec::new();

                for (case, branch) in cases.iter().zip(branches.iter()) {
                    weights.push(compute_weight(interp, case, get!(*condition)?, modulus, is_boolean)?); /* Compute (1 - ('case' - 'condition') ^ (self.modulus - 1)) */
                    let mut branch_scope = HashMap::new();
                    match branch {
                        CaseInvoke::AbstractGateCall(name, input_wires) => {
                            let function = known_functions.get(name).ok_or_else(|| format!("Unknown function: {}", name))?;
                            let expanded_input = expand_wirelist(input_wires);
                            for wire in expanded_input.iter() {
                                branch_scope.insert(*wire, get!(*wire)?.clone());
                            }
                            Self::ingest_subcircuit(
                                &function.2,
                                interp,
                                &expanded_output,
                                &expanded_input,
                                &mut branch_scope,
                                known_functions,
                                &mut HashMap::new(),
                                modulus,
                                is_boolean,
                                &mut new_instances.clone(),
                                &mut new_witnesses.clone(),
                            )?;
                        },
                        CaseInvoke::AbstractAnonCall(input_wires, _, _, subcircuit) => {
                            let expanded_input = expand_wirelist(input_wires);
                            for wire in expanded_input.iter() {
                                branch_scope.insert(*wire, get!(*wire)?.clone());
                            }
                            Self::ingest_subcircuit(
                                subcircuit,
                                interp,
                                &expanded_output,
                                &expanded_input,
                                &mut branch_scope,
                                known_functions,
                                known_iterators,
                                modulus,
                                is_boolean,
                                &mut new_instances.clone(),
                                &mut new_witnesses.clone(),
                            )?;
                        }
                    }
                    // TODO we don't need all the scope here, only the output wires.
                    branches_scope.push(branch_scope);
                }

                // Compute the weighted sum for all output wire.
                for output_wire in expanded_output.iter() {
                    let weighted_output =
                        branches_scope.iter()
                            .zip(weights.iter())
                            .fold(Ok(interp.zero().clone()), |accu, (branch_scope, weight)| {
                                let weighted_wire = as_mul!(interp, get::<I>(branch_scope, *output_wire)?, weight, is_boolean)?;
                                as_add!(interp, &accu?, &weighted_wire, is_boolean)
                            }
                        )?;
                    set!(*output_wire, weighted_output)?;
                }

                // */
            }
        }
        Ok(())
    }

    fn ingest_subcircuit(
        subcircuit: &[Gate],
        interp: &mut I,
        output_list: &[WireId],
        input_list: &[WireId],
        scope: &mut HashMap<WireId, I::Wire>,
        known_functions: &HashMap<String, (usize, usize, Vec<Gate>)>,
        known_iterators: &mut HashMap<String, u64>,
        modulus: &BigUint,
        is_boolean: bool,
        instances: &mut VecDeque<I::FieldElement>,
        witnesses: &mut VecDeque<I::FieldElement>,
    ) -> Result<()> {
        let mut new_scope: HashMap<WireId, I::Wire> = HashMap::new();

        // copy the inputs required by this function into the new scope, at the proper index
        for (idx, input) in input_list.iter().enumerate() {
            set::<I>(&mut new_scope, (idx + output_list.len()) as u64, get::<I>(scope, *input)?.clone())?;
        }
        // evaluate the subcircuit in the new scope.
        for gate in subcircuit {
            Self::ingest_gate(gate, interp, &mut new_scope, known_functions, known_iterators, modulus, is_boolean, instances, witnesses)?;
        }
        // copy the outputs produced from 'new_scope', into 'scope'

        for (idx, output) in output_list.iter().enumerate() {
            set::<I>(scope, *output, get::<I>(&new_scope, idx as u64)?.clone())?;
        }

        Ok(())
    }

    pub fn get(&self, id: WireId) -> Result<&I::Wire> {
        get::<I>(&self.values, id)
    }
}

fn set_instance<I: ZKInterpreter>(interp: &mut I, scope: &mut HashMap<WireId, I::Wire>, id: WireId, value: I::FieldElement) -> Result<()> {
    let wire = interp.instance(value)?;
    set::<I>(scope, id, wire)
}

fn set_witness<I: ZKInterpreter>(interp: &mut I, scope: &mut HashMap<WireId, I::Wire>, id: WireId, value: Option<I::FieldElement>) -> Result<()> {
    let wire = interp.witness(value)?;
    set::<I>(scope, id, wire)
}

fn set<I: ZKInterpreter>(scope: &mut HashMap<WireId, I::Wire>, id: WireId, wire: I::Wire) -> Result<()> {
    if scope.insert(id, wire).is_some() {
        Err(format!("Wire_{} already has a value in this scope.", id).into())
    } else {
        Ok(())
    }
}

pub fn get<I: ZKInterpreter>(scope: &HashMap<WireId, I::Wire>, id: WireId) -> Result<&I::Wire> {
    scope
        .get(&id)
        .ok_or(format!("No value given for wire_{}", id).into())
}

fn remove<I: ZKInterpreter>(scope: &mut HashMap<WireId, I::Wire>, id: WireId) -> Result<I::Wire> {
    scope
        .remove(&id)
        .ok_or(format!("No value given for wire_{}", id).into())
}

/// This function will compute the exponentiation of a given base to a given exponent recursively.
/// It returns the wire holding the result.
fn exp<I: ZKInterpreter>(interp: &mut I, base: &I::Wire, exponent: &BigUint, modulus: &BigUint, is_boolean: bool) -> Result<I::Wire> {
    if exponent.is_one() {
        return Ok(base.clone());
    }

    let previous = exp(interp, base, &exponent.shr(1), modulus, is_boolean)?;

    let ret = as_mul!(interp, &previous, &previous, is_boolean);
    if exponent.bitand(BigUint::one()).is_one() {
        as_mul!(interp, &ret?, base, is_boolean)
    } else {
        ret
    }

}

/// This function will compute '1 - (case - condition)^(p-1)' using a bunch of mul/and add/xor addc/xorc gates
fn compute_weight<I: ZKInterpreter>(interp: &mut I, case: &[u8], condition: &I::Wire, modulus: &BigUint, is_boolean: bool) -> Result<I::Wire> {
    let case_wire = interp.constant(I::from_bytes_le(case)?)?;
    let one_wire = interp.one().clone();
    let minus_one_wire = interp.minus_one().clone();
    let minus_one = modulus - &BigUint::one();

    let minus_cond = as_mul!(interp, condition, &minus_one_wire, is_boolean)?;
    let base = as_add!(interp, &case_wire, &minus_cond, is_boolean)?;

    let base_to_exp = exp(interp, &base, &minus_one, modulus, is_boolean)?;
    let right = as_mul!(interp, &base_to_exp, &minus_one_wire, is_boolean)?;
    as_add!(interp, &one_wire, &right, is_boolean)

}


pub struct PlaintextInterpreter {
    pub m: BigUint,
    pub one: BigUint,
    pub minus_one: BigUint,
    pub zero: BigUint,
}

impl Default for PlaintextInterpreter {
    fn default() -> Self {
        PlaintextInterpreter {
            one: BigUint::one(),
            m: BigUint::zero(),
            minus_one: BigUint::zero(),
            zero: BigUint::zero(),
        }
    }
}

impl ZKInterpreter for PlaintextInterpreter {
    type Wire = BigUint;
    type FieldElement = BigUint;

    fn from_bytes_le(val: &[u8]) -> Result<Self::FieldElement> {
        Ok(BigUint::from_bytes_le(val))
    }

    fn set_field(&mut self, modulus: &[u8], degree: u32) -> Result<()> {
        self.m = BigUint::from_bytes_le(modulus);
        if self.m.is_zero() {
            Err("Modulus cannot be zero.".into())
        } else if degree != 1 {
            Err("Field should be of degree 1".into())
        } else {
            self.minus_one = &self.m - &self.one;
            Ok(())
        }
    }

    fn one(&self) -> Self::Wire {
        self.one.clone()
    }

    fn minus_one(&self) -> Self::Wire {
        if self.m.is_zero() {
            panic!("Modulus should have been set before calling this function.");
        }
        self.minus_one.clone()
    }

    fn zero(&self) -> Self::Wire {
        self.zero.clone()
    }

    fn constant(&mut self, val: Self::FieldElement) -> Result<Self::Wire> {
        Ok(val)
    }

    fn assert_zero(&mut self, wire: &Self::Wire) -> Result<()> {
        if wire.is_zero() {
            Ok(())
        } else {
            Err("AssertZero failed".into())
        }
    }

    fn copy(&mut self, source: &Self::Wire) -> Result<Self::Wire> {
        Ok(source.clone())
    }

    fn add(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        Ok((a + b) % &self.m)
    }

    fn multiply(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        Ok((a * b) % &self.m)
    }

    fn add_constant(&mut self, a: &Self::Wire, b: Self::FieldElement) -> Result<Self::Wire> {
        Ok((a + b) % &self.m)
    }

    fn mul_constant(&mut self, a: &Self::Wire, b: Self::FieldElement) -> Result<Self::Wire> {
        Ok((a * b) % &self.m)
    }

    fn and(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        Ok((a.bitand(b)) % &self.m)
    }

    fn xor(&mut self, a: &Self::Wire, b: &Self::Wire) -> Result<Self::Wire> {
        Ok((a.bitxor(b)) % &self.m)
    }

    fn not(&mut self, a: &Self::Wire) -> Result<Self::Wire> {
        Ok(if a.is_zero() {BigUint::one()} else {BigUint::zero()})
    }

    fn instance(&mut self, val: Self::FieldElement) -> Result<Self::Wire> {
        self.constant(val)
    }

    fn witness(&mut self, val: Option<Self::FieldElement>) -> Result<Self::Wire> {
        self.constant(val.unwrap_or_else(|| panic!("Missing witness value for PlaintextInterpreter")))
    }

    fn free(&mut self, _: &Self::Wire) -> Result<()> {
        Ok(())
    }
}

#[test]
fn test_exponentiation() -> Result<()> {

    let mut interp = PlaintextInterpreter::default();

    let moduli = vec![
        BigUint::from(16249742125730185677094195492597105093u128),
        BigUint::from(101u64),
    ];

    let bases = vec![
        BigUint::from(2u128),
        BigUint::from(42u64),
    ];

    let exponents = vec![
        BigUint::from(2206000150907221872269901214599500635u128),
        BigUint::from(100u64),
    ];

    let expecteds = vec![
        BigUint::from(5834907326474057072663503101785122138u128),
        BigUint::one()
    ];

    for (((modulus, base), exponent), expected) in moduli.iter()
                .zip(bases.iter())
                .zip(exponents.iter())
                .zip(expecteds.iter())
    {
        interp.set_field(&modulus.to_bytes_le(), 1)?;
        let result = exp(&mut interp, base, exponent, modulus, false)?;
        assert_eq!(result, *expected);
    }

    Ok(())
}

#[test]
fn test_evaluator() -> crate::Result<()> {
    use crate::producers::examples::*;
    use crate::consumers::evaluator::Evaluator;

    let relation = example_relation();
    let instance = example_instance();
    let witness = example_witness();

    let mut zkinterpreter = PlaintextInterpreter::default();
    let mut simulator = Evaluator::default();
    simulator.ingest_message(&Message::Instance(instance), &mut zkinterpreter);
    simulator.ingest_message(&Message::Witness(witness), &mut zkinterpreter);
    simulator.ingest_message(&Message::Relation(relation), &mut zkinterpreter);

    assert_eq!(simulator.get_violations().len(), 0);

    Ok(())
}

#[test]
fn test_evaluator_wrong_result() -> crate::Result<()> {
    use crate::producers::examples::*;
    use crate::consumers::evaluator::Evaluator;

    let relation = example_relation();
    let instance = example_instance();
    let witness = example_witness_incorrect();

    let mut zkinterpreter = PlaintextInterpreter::default();
    let mut simulator = Evaluator::default();
    simulator.ingest_message(&Message::Instance(instance), &mut zkinterpreter);
    simulator.ingest_message(&Message::Witness(witness), &mut zkinterpreter);
    simulator.ingest_message(&Message::Relation(relation), &mut zkinterpreter);

    assert_ne!(simulator.get_violations().len(), 0);

    Ok(())
}
