#![allow(dead_code, unused_imports)]

use rand::{ Rng, SeedableRng, rngs::StdRng, thread_rng };
use lib::*;

const POPULATION: usize = 100;
const MAXITERS: usize = 1_000_000;
const SURVIVE: f64 = 0.5;
const GEN_NEW: f64 = 0.1;
const SEED: u64 = 10546;

fn main() {
    // let mut rng = StdRng::seed_from_u64(SEED);
    let mut rng = thread_rng();

    let params =
        Params {
            mean_rounds: 5.0,
            mean_gates: 5.0,
            p_rounds_inc: 0.30,
            p_rounds_dec: 0.30,
            p_gates_inc: 0.30,
            p_gates_dec: 0.30,
            p_resample_gate: 0.50,
        };

    let surv = (POPULATION as f64 * SURVIVE).ceil() as usize;
    let gen_new = (POPULATION as f64 * GEN_NEW).floor() as usize;
    let crossover = POPULATION - surv - gen_new;

    let mut population: Vec<Protocol> =
        (0..POPULATION).map(|_| Protocol::gen(params, &mut rng)).collect();
    let mut sols: Vec<Protocol> = Vec::new();

    let z: usize = (MAXITERS as f64).log10().floor() as usize + 1;
    eprint!("  {:w$} / {:w$} ", 0, MAXITERS, w=z);
    for k in 0..MAXITERS {
        population.iter_mut().for_each(|proc| { proc.eval(); });
        population.sort_unstable_by(|l, r| {
            let ((l_s, l_b), (r_s, r_b)) =
                l.get_score().zip(r.get_score()).unwrap();
            (!l_b).cmp(&(!r_b)).then(l_s.cmp(&r_s))
        });
        for proc in population.iter() {
            if proc.is_sol().is_some_and(|b| b) {
                sols.push(proc.clone());
                eprintln!("{}", proc);
            }
        }
        let _ = population.split_off(surv);
        for _ in 0..crossover {
            let i: usize = rng.gen_range(0..surv);
            let mut j: usize = rng.gen_range(0..surv);
            while i == j { j = rng.gen_range(0..surv); }
            population.push(population[i].crossover(&population[j], &mut rng));
        }
        population.iter_mut()
            .for_each(|proc| { proc.mutate(params, &mut rng); });
        for _ in 0..gen_new {
            population.push(Protocol::gen(params, &mut rng));
        }
        eprint!("\r  {:w$} / {:w$} ", k + 1, MAXITERS, w=z);
    }

    eprintln!("\nsolutions:");
    for sol in sols.iter() { eprintln!("{}", sol); }

    // eprintln!("\nfinal population:");
    // population.iter_mut().for_each(|proc| { proc.eval(); });
    // population.sort_unstable_by(|l, r| {
    //     let ((l_s, l_b), (r_s, r_b)) =
    //         l.get_score().zip(r.get_score()).unwrap();
    //     (!l_b).cmp(&(!r_b)).then(l_s.cmp(&r_s))
    // });
    // for proc in population.iter() { eprintln!("{}", proc); }

    if let Some(proc) = population.first() {
        eprintln!("demo:");
        eprintln!("{}", proc);
        let states = proc.simulate();
        for state in states.iter() { println!("{:?}", state); }
    }
}
