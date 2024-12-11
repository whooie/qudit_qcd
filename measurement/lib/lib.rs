#![allow(dead_code)]

use std::marker::PhantomData;
use rand::Rng;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Q8 { e: bool, n: bool, m: u8 }

impl Q8 {
    pub const BASIS: [Q8; 8] = [
        Q8 { e: false, n: false, m: 0 },
        Q8 { e: false, n: true,  m: 0 },
        Q8 { e: true,  n: false, m: 0 },
        Q8 { e: true,  n: true,  m: 0 },
        Q8 { e: false, n: false, m: 1 },
        Q8 { e: false, n: true,  m: 1 },
        Q8 { e: true,  n: false, m: 1 },
        Q8 { e: true,  n: true,  m: 1 },
    ];
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Meas { B, D }

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Gate {
    NotEN,
    ESwapN,
    ESwapM,
    ENSwapM,
    ESwapNM,
    E1NSwapM,
    E0NSwapM,
    EMShelve,
    N1NotE,
    N0NotE,
    E1NotN,
    E0NotN,
    E1NotNM,
    E0NotNM,
}

impl Gate {
    pub fn gen<R>(rng: &mut R) -> Self
    where R: Rng + ?Sized
    {
        match rng.gen_range(0..14_u8) {
            0 => Self::NotEN,
            1 => Self::ESwapN,
            2 => Self::ESwapM,
            3 => Self::ENSwapM,
            4 => Self::ESwapNM,
            5 => Self::E1NSwapM,
            6 => Self::E0NSwapM,
            7 => Self::EMShelve,
            8 => Self::N1NotE,
            9 => Self::N0NotE,
            10 => Self::E1NotN,
            11 => Self::E0NotN,
            12 => Self::E1NotNM,
            13 => Self::E0NotNM,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State {
    orig: Q8,
    cur: Q8,
    meas: Vec<Meas>,
}

impl State {
    pub fn new(q: Q8) -> Self {
        Self { orig: q, cur: q, meas: Vec::new() }
    }

    pub fn measure(&mut self) -> Meas {
        if self.cur.e {
            self.meas.push(Meas::D);
            self.cur.m = 0;
            Meas::D
        } else {
            self.meas.push(Meas::B);
            self.cur.m = 0;
            Meas::B
        }
    }

    pub fn apply_gate(&mut self, gate: &Gate) {
        match gate {
            Gate::NotEN => {
                if !(self.cur.e ^ self.cur.n) {
                    self.cur.e = !self.cur.e;
                    self.cur.n = !self.cur.n;
                }
            },
            Gate::ESwapN => {
                std::mem::swap(&mut self.cur.e, &mut self.cur.n);
            },
            Gate::ESwapM => {
                match (self.cur.e, self.cur.m) {
                    (false, 1) => { self.cur.e = true;  self.cur.m = 0; },
                    (true,  0) => { self.cur.e = false; self.cur.m = 1; },
                    _ => { },
                }
            },
            Gate::ENSwapM => {
                match self.cur {
                    Q8 { e: true, n: true, m: 0 } => {
                        self.cur.e = false;
                        self.cur.n = false;
                        self.cur.m = 1;
                    },
                    Q8 { e: false, n: false, m: 1 } => {
                        self.cur.e = true;
                        self.cur.n = true;
                        self.cur.m = 0;
                    },
                    _ => { },
                }
            },
            Gate::ESwapNM => {
                match self.cur {
                    Q8 { e: true, n: false, m: 0 } => {
                        self.cur.e = false;
                        self.cur.n = true;
                        self.cur.m = 1;
                    },
                    Q8 { e: false, n: true, m: 1 } => {
                        self.cur.e = true;
                        self.cur.n = false;
                        self.cur.m = 0;
                    },
                    _ => { },
                }
            },
            Gate::N1NotE => {
                if self.cur.n { self.cur.e = !self.cur.e; }
            },
            Gate::N0NotE => {
                if !self.cur.n { self.cur.e = !self.cur.e; }
            },
            Gate::E1NotN => {
                if self.cur.e { self.cur.n = !self.cur.n; }
            },
            Gate::E0NotN => {
                if !self.cur.e { self.cur.n = !self.cur.n; }
            },
            Gate::E1NSwapM => {
                if self.cur.e {
                    match (self.cur.n, self.cur.m) {
                        (false, 1) => { self.cur.n = true;  self.cur.m = 0; },
                        (true,  0) => { self.cur.n = false; self.cur.m = 1; },
                        _ => { },
                    }
                }
            },
            Gate::E0NSwapM => {
                if !self.cur.e {
                    match (self.cur.n, self.cur.m) {
                        (false, 1) => { self.cur.n = true;  self.cur.m = 0; },
                        (true,  0) => { self.cur.n = false; self.cur.m = 1; },
                        _ => { },
                    }
                }
            },
            Gate::E1NotNM => {
                if self.cur.e && !(self.cur.n ^ (self.cur.m == 1)) {
                    self.cur.n = !self.cur.n;
                    self.cur.m = 1 - self.cur.m;
                }
            },
            Gate::E0NotNM => {
                if !self.cur.e && !(self.cur.n ^ (self.cur.m == 1)) {
                    self.cur.n = !self.cur.n;
                    self.cur.m = 1 - self.cur.m;
                }
            },
            Gate::EMShelve => {
                match (self.cur.e, self.cur.m) {
                    (false, 1) => { self.cur.e = true;  self.cur.m = 2; },
                    (true,  2) => { self.cur.e = false; self.cur.m = 1; },
                    _ => { },
                }
            },
        }
    }
}

fn sample_poisson<R>(mean: f64, rng: &mut R) -> usize
where R: Rng + ?Sized
{
    let r: f64 = rng.gen();
    let mut prob: f64 = (-mean).exp();
    let mut cuprob: f64 = 0.0;
    let mut k: usize = 0;
    loop {
        if k != 0 { prob *= mean / k as f64; }
        cuprob += prob;
        if r < cuprob { return k; }
        k += 1;
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Params {
    pub mean_rounds: f64,
    pub mean_gates: f64,
    pub p_rounds_inc: f64,
    pub p_rounds_dec: f64,
    pub p_gates_inc: f64,
    pub p_gates_dec: f64,
    pub p_resample_gate: f64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Protocol {
    seq: Vec<Vec<Gate>>,
    score: Option<(usize, bool)>,
}

impl std::fmt::Display for Protocol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} :: {:?}", self.seq, self.score)
    }
}

impl Protocol {
    pub fn gen<R>(params: Params, rng: &mut R) -> Self
    where R: Rng + ?Sized
    {
        let num_rounds = sample_poisson(params.mean_rounds, rng).max(1);
        let num_gates = sample_poisson(params.mean_gates, rng);
        let seq: Vec<Vec<Gate>> =
            (0..num_rounds)
            .map(|_| { (0..num_gates).map(|_| Gate::gen(rng)).collect() })
            .collect();
        Self { seq, score: None }
    }

    pub fn simulate(&self) -> Vec<State> {
        Q8::BASIS.into_iter()
            .map(|init| {
                let mut state = State::new(init);
                for round in self.seq.iter() {
                    for gate in round.iter() {
                        state.apply_gate(gate);
                    }
                    state.measure();
                }
                state
            })
            .collect()
    }

    #[allow(unused_variables, unused_mut)]
    pub fn eval(&mut self) -> (usize, bool) {
        if let Some((score, no_dups)) = self.score {
            (score, no_dups)
        } else {
            let states: Vec<State> = self.simulate();
            let num_rounds: usize = self.seq.len();
            let num_gates: usize =
                self.seq.iter()
                .map(|round| round.len())
                .sum();
            let num_dups: usize =
                states.iter().enumerate()
                .map(|(k, state0)| {
                    states.iter().take(k)
                    .filter(|state1| state0.meas == state1.meas)
                    .count()
                })
                .sum();
            // let score = (1 + num_rounds) * (1 + num_gates) * (1 + num_dups);
            let score = num_dups;
            let no_dups = num_dups == 0;
            self.score = Some((score, no_dups));
            (score, no_dups)
        }
    }

    pub fn get_score(&self) -> Option<(usize, bool)> { self.score }

    pub fn is_sol(&self) -> Option<bool> {
        self.score.map(|(_, b)| b)
    }

    pub fn mutate<R>(&mut self, params: Params, rng: &mut R)
    where R: Rng + ?Sized
    {
        let r_rounds: f64 = rng.gen();
        if r_rounds < params.p_rounds_inc {
            let num_gates = sample_poisson(params.mean_gates, rng);
            let new: Vec<Gate> =
                (0..num_gates).map(|_| Gate::gen(rng)).collect();
            self.seq.push(new);
            self.score = None;
        } else if r_rounds < params.p_rounds_inc + params.p_rounds_dec {
            let n = self.seq.len();
            if n > 1 {
                let k: usize = rng.gen_range(0..n);
                self.seq.remove(k);
            }
            self.score = None;
        }
        for round in self.seq.iter_mut() {
            let r_gates: f64 = rng.gen();
            if r_gates < params.p_gates_inc {
                let new = Gate::gen(rng);
                round.push(new);
                self.score = None;
            } else if r_gates < params.p_gates_inc + params.p_gates_dec {
                let n = round.len();
                if n > 1 {
                    let k: usize = rng.gen_range(0..n);
                    round.remove(k);
                }
                self.score = None;
            }
            for gate in round.iter_mut() {
                if rng.gen::<f64>() < params.p_resample_gate {
                    *gate = Gate::gen(rng);
                }
                self.score = None;
            }
        }
    }

    pub fn crossover<R>(&self, other: &Self, rng: &mut R) -> Self
    where R: Rng + ?Sized
    {
        let seq: Vec<Vec<Gate>> =
            ZipLongest::new(self.seq.iter(), other.seq.iter())
            .flat_map(|it| {
                match it {
                    ZipLongestItem::Left(l) =>
                        if rng.gen::<bool>() {
                            Some(l.clone())
                        } else {
                            None
                        },
                    ZipLongestItem::Right(r) =>
                        if rng.gen::<bool>() {
                            Some(r.clone())
                        } else {
                            None
                        },
                    ZipLongestItem::Both(l, r) =>
                        if rng.gen::<bool>() {
                            Some(r.clone())
                        } else {
                            Some(l.clone())
                        },
                }
            })
            .collect();
        Self { seq, score: None }
    }
}

enum ZipLongestItem<T, U> {
    Left(T),
    Right(U),
    Both(T, U),
}

struct ZipLongest<I, J, T, U> {
    iter_left: I,
    iter_right: J,
    it_l: PhantomData<T>,
    it_r: PhantomData<U>,
}

impl<I, J, T, U> ZipLongest<I, J, T, U>
where
    I: Iterator<Item = T>,
    J: Iterator<Item = U>,
{
    fn new(iter_left: I, iter_right: J) -> Self {
        Self {
            iter_left,
            iter_right,
            it_l: PhantomData,
            it_r: PhantomData,
        }
    }
}

impl<I, J, T, U> Iterator for ZipLongest<I, J, T, U>
where
    I: Iterator<Item = T>,
    J: Iterator<Item = U>,
{
    type Item = ZipLongestItem<T, U>;

    fn next(&mut self) -> Option<Self::Item> {
        match (self.iter_left.next(), self.iter_right.next()) {
            (Some(l), Some(r)) => Some(ZipLongestItem::Both(l, r)),
            (Some(l), None   ) => Some(ZipLongestItem::Left(l)),
            (None,    Some(r)) => Some(ZipLongestItem::Right(r)),
            (None,    None   ) => None
        }
    }
}

