use std::{collections::HashMap, ops::Sub};

struct Rule {
    head: Atom,
    body: Vec<Atom>
}

#[derive(PartialEq, Eq)]
struct Atom {
    pred_sym: String,
    terms: Vec<Term>
}

#[derive(PartialEq, Eq,Clone,Hash)]
enum Term {
    Var(String),
    Sym(String)
}

type Program = Vec<Rule>;
type KnowlegeBase = Vec<Atom>;
type Substitution = HashMap<Term,Term>;

fn empty_substition() -> Substitution {
    HashMap::new()
}

fn substitute(atom: Atom, sub: Substitution) -> Atom {
    let terms = atom.terms.to_vec();
    let terms = terms.iter().map(|t| sub.get(t).unwrap_or(t)).cloned().collect();
    Atom {
        terms,
        ..atom
    }
}

fn unify(a: Atom, b: Atom) -> Option<Substitution> {
    if a.pred_sym == b.pred_sym {
        let terms = a.terms.into_iter().zip(b.terms.into_iter());
        let mut subs = empty_substition();
        for t in terms {
            match t {
                (sa @ Term::Sym(_), sb @ Term::Sym(_)) => if sa == sb {
                    continue;
                } else {
                    return None;
                }
                (v @ Term::Var(_), s @ Term::Sym(_)) => {
                    match subs.get(&v) {
                        Some(ss) if &s != ss => return None,
                        _ => {
                            subs.insert(v, s);
                        }
                    }
                }
                (_, Term::Var(_)) => panic!("The second atom is assumed to be ground"),
            }
        }
        Some(subs)
    } else {
        None
    }
}

fn main() {
    
    println!("Hello, world!");
}
