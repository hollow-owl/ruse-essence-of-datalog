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

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
struct Atom {
    pred_sym: String,
    terms: Vec<Term>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Term {
    Var(String),
    Sym(String),
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
    substitutions
        .into_iter()
        .map(|sub| {
            let down_to_earth_atom = substitute(atom.clone(), sub.clone());
            let ext: HashMap<Term, Term> = kb
                .clone()
                .into_iter()
                .filter_map(|x| unify(down_to_earth_atom.clone(), x))
                .chain(iter::once(sub))
                .flatten()
                .collect();
            ext
        })
        .collect()
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

fn main() {
    let ancestor: Program = {
        let facts = vec![
            vec![
                Sym("Andrew Rice".to_string()),
                Sym("Mistral Contrastin".to_string()),
            ],
            vec![
                Sym("Dominic Orchard".to_string()),
                Sym("Mistral Contrastin".to_string()),
            ],
            vec![
                Sym("Andy Hopper".to_string()),
                Sym("Andrew Rice".to_string()),
            ],
            vec![
                Sym("Alan Mycroft".to_string()),
                Sym("Dominic Orchard".to_string()),
            ],
            vec![
                Sym("David Wheeler".to_string()),
                Sym("Andy Hopper".to_string()),
            ],
            vec![
                Sym("Rod Burstall".to_string()),
                Sym("Alan Mycroft".to_string()),
            ],
            vec![
                Sym("Robin Milner".to_string()),
                Sym("Alan Mycroft".to_string()),
            ],
        ]
        .into_iter()
        .map(|terms| Rule {
            head: Atom {
                pred_sym: "adviser".to_string(),
                terms,
            },
            body: vec![],
        });
        let rules = vec![
            Rule {
                head: Atom {
                    pred_sym: "academicAncestor".to_string(),
                    terms: vec![Var("X".to_string()), Var("Y".to_string())],
                },
                body: vec![Atom {
                    pred_sym: "advisor".to_string(),
                    terms: vec![Var("X".to_string()), Var("Y".to_string())],
                }],
            },
            Rule {
                head: Atom {
                    pred_sym: "academicAncestor".to_string(),
                    terms: vec![Var("X".to_string()), Var("Z".to_string())],
                },
                body: vec![
                    Atom {
                        pred_sym: "academicAncestor".to_string(),
                        terms: vec![Var("X".to_string()), Var("Y".to_string())],
                    },
                    Atom {
                        pred_sym: "advisor".to_string(),
                        terms: vec![Var("Y".to_string()), Var("Z".to_string())],
                    },
                ],
            },
        ];
        let queries = vec![
            Rule {
                head: Atom {
                    pred_sym: "query1".to_string(),
                    terms: vec![Var("Intermediate".to_string())],
                },
                body: vec![
                    vec![
                        Sym("Robin Milner".to_string()),
                        Sym("Intermediate".to_string()),
                    ],
                    vec![
                        Sym("Intermediate".to_string()),
                        Sym("Mistral Contrastin".to_string()),
                    ],
                ]
                .into_iter()
                .map(|x| Atom {
                    pred_sym: "academicAncestor".to_string(),
                    terms: x,
                })
                .collect(),
            },
            Rule {
                head: Atom {
                    pred_sym: "query2".to_string(),
                    terms: vec![],
                },
                body: vec![Atom {
                    pred_sym: "academicAncestor".to_string(),
                    terms: vec![
                        Sym("Alan Turing".to_string()),
                        Sym("Mistral Contrastin".to_string()),
                    ],
                }],
            },
            Rule {
                head: Atom {
                    pred_sym: "query3".to_string(),
                    terms: vec![],
                },
                body: vec![Atom {
                    pred_sym: "academicAncestor".to_string(),
                    terms: vec![
                        Sym("David Wheeler".to_string()),
                        Sym("Mistral Conrastin".to_string()),
                    ],
                }],
            },
        ];

        let a: Vec<_> = facts.chain(rules.into_iter()).collect(); //.chain(queries.into_iter()).collect();
        a
    };
    dbg!(solve(ancestor));
}
