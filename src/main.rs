use crate::Term::*;
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    iter,
};

type Program = Vec<Rule>;

#[derive(Debug, Clone)]
struct Rule {
    head: Atom,
    body: Vec<Atom>,
}

impl Rule {
    fn new(head: Atom, body: Vec<Atom>) -> Self {
        Rule { head, body }
    }
    fn fact(head: Atom) -> Self {
        Rule {
            head,
            body: Vec::new(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
struct Atom {
    pred_sym: String,
    terms: Vec<Term>,
}

macro_rules! Atom {
    ($pre_sym:ident()) => {
        Atom {
            pred_sym: stringify!($pre_sym).to_string(),
            terms: vec![]
        }
    };
    ($pre_sym:ident($($e:tt),*)) => {
        Atom {
            pred_sym: stringify!($pre_sym).to_string(),
            terms: vec![$(
                Term!($e),
            )*]
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Term {
    Var(String),
    Sym(String),
}

macro_rules! Term {
    ($v:ident) => {
        Var(stringify!($v).to_string())
    };
    ($s:literal) => {
        Sym($s.to_owned())
    };
}

type KnowlegeBase = HashSet<Atom>;
type Substitution = HashMap<Term, Term>;

fn empty_substition() -> Substitution {
    HashMap::new()
}

fn solve(rules: Program) -> KnowlegeBase {
    if rules.iter().all(is_range_restricted) {
        let mut kb = HashSet::new();
        loop {
            let nextkb = immediate_consequence(rules.clone(), kb.clone());
            if nextkb == kb {
                return kb;
            } else {
                kb = nextkb;
            }
        }
    } else {
        panic!("The input program is not range restricted");
    }
}

fn is_range_restricted(r: &Rule) -> bool {
    // every variable in the head appears somewhere in the body
    let head_vars: HashSet<&Term> = r
        .head
        .terms
        .iter()
        .filter(|t| matches!(t, Var(_)))
        .collect();
    let body_vars: HashSet<&Term> = r
        .body
        .iter()
        .flat_map(|x| &x.terms)
        .filter(|t| matches!(t, Var(_)))
        .collect();
    head_vars.is_subset(&body_vars)
}

fn immediate_consequence(rules: Program, kb: KnowlegeBase) -> KnowlegeBase {
    // evaluate each rule independently and then concatenate the newly derived facts together and bundle them with what we already know
    let out = rules.into_iter().flat_map(|x| eval_rule(kb.clone(), x));
    let new_kb: HashSet<_> = kb.clone().into_iter().chain(out).collect();
    new_kb
}

fn eval_rule(kb: KnowlegeBase, Rule { head, body }: Rule) -> KnowlegeBase {
    let w = walk(kb, body);
    w.into_iter().map(|x| substitute(head.clone(), x)).collect()
}

fn walk(kb: KnowlegeBase, body: Vec<Atom>) -> Vec<Substitution> {
    body.into_iter().fold(vec![empty_substition()], |acc, x| {
        eval_atom(kb.clone(), x, acc)
    })
}

fn eval_atom(kb: KnowlegeBase, atom: Atom, substitutions: Vec<Substitution>) -> Vec<Substitution> {
    let mut new_subs = vec![];
    for sub in substitutions {
        let down_to_earth_atom = substitute(atom.clone(), sub.clone());
        dbg!(&kb, &down_to_earth_atom, &sub);
        let ext: HashMap<Term, Term> = kb
            .clone()
            .into_iter()
            .filter_map(|x| unify(down_to_earth_atom.clone(), x))
            .chain(iter::once(sub))
            .flatten()
            .collect();
        if !ext.is_empty() {
            new_subs.push(ext);
        }
    }
    new_subs
}

fn substitute(atom: Atom, substitution: Substitution) -> Atom {
    let terms = atom
        .terms
        .into_iter()
        .map(|x| match x {
            s @ Sym(_) => s,
            v @ Var(_) => substitution.get(&v).cloned().unwrap_or(v),
        })
        .collect();
    Atom { terms, ..atom }
}

fn unify(a: Atom, b: Atom) -> Option<Substitution> {
    fn go(mut x: impl Iterator<Item = (Term, Term)>) -> Option<Substitution> {
        let ts = x.next();
        match ts {
            None => Some(empty_substition()),
            Some((sa @ Sym(_), sb @ Sym(_))) => {
                if sa == sb {
                    go(x)
                } else {
                    None
                }
            }
            Some((v @ Var(_), s @ Sym(_))) => {
                let mut incomplete_sub = go(x)?;
                match incomplete_sub.get(&v) {
                    Some(ss) if ss != &s => None,
                    _ => {
                        incomplete_sub.insert(v, s);
                        Some(incomplete_sub)
                    }
                }
            }
            Some((_, Var(_))) => panic!("The Second atom is assumed to be ground"),
        }
    }
    if a.pred_sym == b.pred_sym {
        go(a.terms.into_iter().zip(b.terms.into_iter()))
    } else {
        None
    }
}

fn query(pred_sym: String, pr: Program) -> Vec<Substitution> {
    let kb = solve(pr.clone())
        .into_iter()
        .filter(|x| x.pred_sym == pred_sym)
        .map(|x| x.terms);
    let qv = pr
        .into_iter()
        .filter(|x| x.head.pred_sym == pred_sym)
        .map(|x| x.head.terms)
        .collect::<Vec<_>>();

    if let Some(qv) = qv.first() {
        kb.map(|k| k.into_iter().zip(qv.clone().into_iter()).collect())
            .collect()
    } else if qv.is_empty() {
        panic!("empty");
    } else {
        panic!("multiple clauses");
    }
}

fn ancestor() -> Vec<Rule> {
    let facts = vec![
        Rule::fact(Atom!(adviser("Andrew Rice", "Mistral Contrastin"))),
        Rule::fact(Atom!(adviser("Dominic Orchard", "Andrew Rice"))),
        Rule::fact(Atom!(adviser("Andy Hopper", "Andrew Rice"))),
        Rule::fact(Atom!(adviser("Alan Mycroft", "Dominic Orchard"))),
        Rule::fact(Atom!(adviser("David Wheeler", "Andy Hopper"))),
        Rule::fact(Atom!(adviser("Rod Burstall", "Alan Mycroft"))),
        Rule::fact(Atom!(adviser("Robin Milner", "Alan Mycroft"))),
    ]
    .into_iter();
    let rules = vec![
        Rule::new(
            Atom!(academicAncestor(X, Y)),
            vec![Atom!(academicAncestor(X, Y)), Atom!(adviser(Y, Z))],
        ),
        Rule::new(Atom!(academicAncestor(X, Y)), vec![Atom!(adviser(X, Y))]),
    ]
    .into_iter();
    let queries = vec![
        Rule::new(
            Atom!(query()),
            vec![
                Atom!(academicAncestor("Robin Milner", Intermediate)),
                Atom!(academicAncestor(Intermediate, "Mistral Contrastin")),
            ],
        ),
        Rule::new(
            Atom!(query()),
            vec![Atom!(academicAncestor("Alan Turing", "Mistral Contrastin"))],
        ),
        Rule::new(
            Atom!(query()),
            vec![Atom!(academicAncestor(
                "David Wheeler",
                "Mistral Contrastin"
            ))],
        ),
    ]
    .into_iter();
    facts.chain(rules).chain(queries).collect()
}

fn main() {
    solve( ancestor());
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_essense_example() {
        let facts = vec![
            Rule::fact(Atom!(adviser("Andrew Rice", "Mistral Contrastin"))),
            Rule::fact(Atom!(adviser("Dominic Orchard", "Andrew Rice"))),
            Rule::fact(Atom!(adviser("Andy Hopper", "Andrew Rice"))),
            Rule::fact(Atom!(adviser("Alan Mycroft", "Dominic Orchard"))),
            Rule::fact(Atom!(adviser("David Wheeler", "Andy Hopper"))),
            Rule::fact(Atom!(adviser("Rod Burstall", "Alan Mycroft"))),
            Rule::fact(Atom!(adviser("Robin Milner", "Alan Mycroft"))),
        ]
        .into_iter();
        let rules = vec![
            Rule::new(
                Atom!(academicAncestor(X, Y)),
                vec![Atom!(academicAncestor(X, Y)), Atom!(adviser(Y, Z))],
            ),
            Rule::new(Atom!(academicAncestor(X, Y)), vec![Atom!(adviser(X, Y))]),
        ]
        .into_iter();
        let queries = vec![
            Rule::new(
                Atom!(query()),
                vec![
                    Atom!(academicAncestor("Robin Milner", Intermediate)),
                    Atom!(academicAncestor(Intermediate, "Mistral Contrastin")),
                ],
            ),
            Rule::new(
                Atom!(query()),
                vec![Atom!(academicAncestor("Alan Turing", "Mistral Contrastin"))],
            ),
            Rule::new(
                Atom!(query()),
                vec![Atom!(academicAncestor(
                    "David Wheeler",
                    "Mistral Contrastin"
                ))],
            ),
        ]
        .into_iter();

        let program: Program = facts.chain(rules).chain(queries).collect();
        dbg!(solve(program));
    }
    #[test]
    fn test_eval_rule() {
        let fact = Rule::new(Atom!(adviser("AR", "MC")), vec![]);
        let rule = Rule::new(
            Atom!(academicAncestor(X, Y)),
            vec![Atom!(academicAncestor(X, Y)), Atom!(adviser(Y, Z))],
        );
        let r2 = Rule::new(Atom!(academicAncestor(X, Y)), vec![Atom!(adviser(X, Y))]);

        let program = vec![fact, rule, r2];
        let kb = vec![Atom!(adviser("AR", "MC"))].into_iter().collect();

        let out = immediate_consequence(program, kb);
        let expected = vec![
            Atom!(adviser("AR", "MC")),
            Atom!(academicAncestor("AR", "MC")),
        ]
        .into_iter()
        .collect();
        assert_eq!(out, expected);
    }
}
