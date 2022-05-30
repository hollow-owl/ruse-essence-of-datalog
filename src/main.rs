use std::{collections::HashMap, ops::Sub, hash::Hash};

type Program = Vec<Rule>;
struct Rule {
    head: Atom,
    body: Vec<Atom>,
}

#[derive(PartialEq, Eq)]
struct Atom {
    pred_sym: String,
    terms: Vec<Term>,
}

#[derive(PartialEq, Eq)]
pub enum Term {
    Var(String),
    Sym(String),
}

type KnowlegeBase = Vec<Atom>;
type Substitution = HashMap<Term, Term>;

fn empty_substition() -> Substitution {
    HashMap::new()
}

fn solve(rules: Program) -> KnowlegeBase {
    vec![]
}

fn is_range_restricted(r: Rule) -> Bool {
    false
}

fn immediate_consequence(rules: Program, kb: KnowlegeBase) -> KnowlegeBase {
    vec![]
}

fn eval_rule(kb: KnowlegeBase, Rule { head, body }: Rule) -> KnowlegeBase {
    vec![]
}

fn walk(kb: KnowlegeBase) -> fn(Vec<Atom>) -> Vec<Substitution> {
    |x| { vec![] }
}

fn eval_atom(kb: KnowlegeBase, atom: Atom, substitutions: Vec<Substitution>) -> Vec<Substitution> {
    vec![]
}

fn substitute(atom: Atom, substitution: Substitution) -> Atom {
    vec![]
}

fn unify(a: Atom, b: Atom) -> Option<Substitution> {
    None
}

fn main() {
    println!("Hello, world!");
}
