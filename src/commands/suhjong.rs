use serenity::framework::standard::{macros::command, Args, CommandResult};
use serenity::model::prelude::*;
use serenity::prelude::*;

use std::collections::HashMap;
use std::collections::HashSet;

use itertools::Itertools;
use rand::seq::SliceRandom;
use rand::Rng;
use regex::Regex;

#[derive(PartialEq, Eq, std::hash::Hash, Clone, Copy)]
enum Pettern1 {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
}

#[derive(PartialEq, Eq, std::hash::Hash, Clone, Copy)]
enum Pettern2L {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I1,
    I2,
    J1,
    J2,
    K1,
    K2,
    L,
    M,
    N,
    O,
    P,
    Q,
}

#[derive(PartialEq, Eq, std::hash::Hash, Clone, Copy)]
enum Pettern2R {
    R,
    S,
    T,
    U,
}

#[derive(PartialEq, Eq, std::hash::Hash, Clone, Copy)]
struct Narabi {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
    e: u32,
    p1: Pettern1,
    p2l: Pettern2L,
    p2r: Pettern2R,
}

impl Narabi {
    fn execute_pettern_l(&self) -> u32 {
        let (a, b, c, _, _) = trade_by_pettern1(self.a, self.b, self.c, self.d, self.e, self.p1);
        match self.p2l {
            Pettern2L::A => a + b + c,
            Pettern2L::B => a + b * c,
            Pettern2L::C => b + c * a,
            Pettern2L::D => c + a * b,
            Pettern2L::E => (a + b) * c,
            Pettern2L::F => (b + c) * a,
            Pettern2L::G => (c + a) * b,
            Pettern2L::H => a * b * c,
            Pettern2L::I1 | Pettern2L::I2 => a * 10 + b + c,
            Pettern2L::J1 | Pettern2L::J2 => b * 10 + c + a,
            Pettern2L::K1 | Pettern2L::K2 => c * 10 + a + b,
            Pettern2L::L => (a * 10 + b) * c,
            Pettern2L::M => (a * 10 + c) * b,
            Pettern2L::N => (b * 10 + a) * c,
            Pettern2L::O => (b * 10 + c) * a,
            Pettern2L::P => (c * 10 + a) * b,
            Pettern2L::Q => (c * 10 + b) * a,
        }
    }

    fn execute_pettern_r(&self) -> u32 {
        let (_, _, _, d, e) = trade_by_pettern1(self.a, self.b, self.c, self.d, self.e, self.p1);
        match self.p2r {
            Pettern2R::R => d + e,
            Pettern2R::S => d * e,
            Pettern2R::T => d * 10 + e,
            Pettern2R::U => e * 10 + d,
        }
    }

    fn to_string(self) -> String {
        let (a, b, c, d, e) = trade_by_pettern1(self.a, self.b, self.c, self.d, self.e, self.p1);
        let left_string = match self.p2l {
            Pettern2L::A => format!("{} + {} + {}", a, b, c),
            Pettern2L::B => format!("{} + {} * {}", a, b, c),
            Pettern2L::C => format!("{} + {} * {}", b, a, c),
            Pettern2L::D => format!("{} + {} * {}", c, a, b),
            Pettern2L::E => format!("({} + {}) * {}", a, b, c),
            Pettern2L::F => format!("({} + {}) * {}", b, c, a),
            Pettern2L::G => format!("({} + {}) * {}", a, c, b),
            Pettern2L::H => format!("{} * {} * {}", a, b, c),
            Pettern2L::I1 => format!("{}{} + {}", a, b, c),
            Pettern2L::I2 => format!("{}{} + {}", a, c, b),
            Pettern2L::J1 => format!("{}{} + {}", b, a, c),
            Pettern2L::J2 => format!("{}{} + {}", b, c, a),
            Pettern2L::K1 => format!("{}{} + {}", c, a, b),
            Pettern2L::K2 => format!("{}{} + {}", c, b, a),
            Pettern2L::L => format!("{}{} * {}", a, b, c),
            Pettern2L::M => format!("{}{} * {}", a, c, b),
            Pettern2L::N => format!("{}{} * {}", b, a, c),
            Pettern2L::O => format!("{}{} * {}", b, c, a),
            Pettern2L::P => format!("{}{} * {}", c, a, b),
            Pettern2L::Q => format!("{}{} * {}", c, b, a),
        };
        let right_string = match self.p2r {
            Pettern2R::R => format!("{} + {}", d, e),
            Pettern2R::S => format!("{} * {}", d, e),
            Pettern2R::T => format!("{}{}", d, e),
            Pettern2R::U => format!("{}{}", e, d),
        };
        format!("{} = {}", left_string, right_string)
    }
}

fn trade_by_pettern1(
    a1: u32,
    b1: u32,
    c1: u32,
    d1: u32,
    e1: u32,
    p1: Pettern1,
) -> (u32, u32, u32, u32, u32) {
    match p1 {
        Pettern1::X0 => (a1, b1, c1, d1, e1),
        Pettern1::X1 => (a1, b1, d1, c1, e1),
        Pettern1::X2 => (a1, b1, e1, c1, d1),
        Pettern1::X3 => (a1, c1, d1, b1, e1),
        Pettern1::X4 => (a1, c1, e1, b1, d1),
        Pettern1::X5 => (a1, d1, e1, b1, c1),
        Pettern1::X6 => (b1, c1, d1, a1, e1),
        Pettern1::X7 => (b1, c1, e1, a1, d1),
        Pettern1::X8 => (b1, d1, e1, a1, c1),
        Pettern1::X9 => (c1, d1, e1, a1, b1),
    }
}

fn hantei(a: u32, b: u32, c: u32, d: u32, e: u32) -> HashSet<Narabi> {
    let mut result: HashMap<u32, HashSet<Narabi>> = HashMap::new();
    for p1 in vec![
        Pettern1::X0,
        Pettern1::X1,
        Pettern1::X2,
        Pettern1::X3,
        Pettern1::X4,
        Pettern1::X5,
        Pettern1::X6,
        Pettern1::X7,
        Pettern1::X8,
        Pettern1::X9,
    ] {
        for p2l in vec![
            Pettern2L::A,
            Pettern2L::B,
            Pettern2L::C,
            Pettern2L::D,
            Pettern2L::E,
            Pettern2L::F,
            Pettern2L::G,
            Pettern2L::H,
            Pettern2L::I1,
            Pettern2L::I2,
            Pettern2L::J1,
            Pettern2L::J2,
            Pettern2L::K1,
            Pettern2L::K2,
            Pettern2L::L,
            Pettern2L::M,
            Pettern2L::N,
            Pettern2L::O,
            Pettern2L::P,
            Pettern2L::Q,
        ] {
            for p2r in vec![Pettern2R::R, Pettern2R::S, Pettern2R::T, Pettern2R::U] {
                let narabi = Narabi {
                    a,
                    b,
                    c,
                    d,
                    e,
                    p1,
                    p2l,
                    p2r,
                };
                if narabi.execute_pettern_l() != narabi.execute_pettern_r() {
                    continue;
                }
                match result.get_mut(&narabi.execute_pettern_l()) {
                    Some(set) => {
                        set.insert(narabi);
                    }
                    None => {
                        result.insert(
                            narabi.execute_pettern_l(),
                            vec![narabi].into_iter().collect(),
                        );
                    }
                };
            }
        }
    }
    let max_opt = result.clone().into_keys().max();
    match max_opt {
        Some(max) => match result.get(&max) {
            Some(set) => set.clone(),
            None => HashSet::new(),
        },
        None => HashSet::new(),
    }
}

fn shoub(yaku1: String, yaku2: String) -> String {
    if yaku1.len() != 5 || yaku2.len() != 5 {
        return "引数がおかしいです".to_string();
    }

    let mut temp1: Vec<_> = yaku1
        .chars()
        .filter(|c| c >= &'0' && c <= &'9')
        .map(|c| (c as u32) - ('0' as u32))
        .collect();
    let mut temp2: Vec<_> = yaku2
        .chars()
        .filter(|c| c >= &'0' && c <= &'9')
        .map(|c| (c as u32) - ('0' as u32))
        .collect();
    if temp1.len() != 5 || temp2.len() != 5 {
        return "引数がおかしいです".to_string();
    }

    temp1.sort();
    temp2.sort();

    let hantei1 = hantei(temp1[0], temp1[1], temp1[2], temp1[3], temp1[4]);
    let hantei2 = hantei(temp2[0], temp2[1], temp2[2], temp2[3], temp2[4]);
    let score1 = if hantei1.is_empty() {
        0
    } else {
        hantei1.iter().next().expect("絶対ある").execute_pettern_l()
    };
    let score2 = if hantei2.is_empty() {
        0
    } else {
        hantei2.iter().next().expect("絶対ある").execute_pettern_l()
    };
    let expressions1 = if hantei1.is_empty() {
        "no solutions!!".to_string()
    } else {
        hantei1
            .into_iter()
            .map(|narabi| narabi.to_string())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
            .join("\n")
    };
    let expressions2 = if hantei2.is_empty() {
        "no solutions!!".to_string()
    } else {
        hantei2
            .into_iter()
            .map(|narabi| narabi.to_string())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
            .join("\n")
    };

    format!(
        "{}(得点{}) {}\n```\n{}\n```\n{}(得点{}) {}\n```\n{}\n```",
        yaku1,
        score1,
        if score1 > score2 {
            "WIN"
        } else if score1 < score2 {
            "LOSE"
        } else {
            "DRAW"
        },
        expressions1,
        yaku2,
        score2,
        if score2 > score1 {
            "WIN"
        } else if score2 < score1 {
            "LOSE"
        } else {
            "DRAW"
        },
        expressions2
    )
}

#[command]
#[description = "数雀判定"]
async fn suhjong(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let (param_1, param_2) = match (args.single::<String>(), args.single::<String>()) {
        (Ok(s1), Ok(s2)) => (s1, s2),
        (Err(_), _) | (_, Err(_)) => {
            msg.reply(&ctx.http, "引数がおかしいです").await?;
            return Ok(());
        }
    };
    let m = shoub(param_1, param_2);
    msg.reply(&ctx.http, m).await?;

    Ok(())
}

fn random_yaku() -> String {
    let mut rng = rand::thread_rng();
    let i: u32 = rng.gen();
    format!("{:>05}", i % 100000)
}

#[command]
#[description = "ランダム数雀"]
async fn suhjong_random(ctx: &Context, msg: &Message) -> CommandResult {
    let param_1 = random_yaku();
    let param_2 = random_yaku();
    let m = shoub(param_1, param_2);
    msg.reply(&ctx.http, m).await?;

    Ok(())
}

#[command]
#[description = "数雀チェックメイト補助"]
async fn check_suhjong(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let (param_1, param_2, param_3) = match (
        args.single::<String>(),
        args.single::<String>(),
        args.single::<String>(),
    ) {
        (Ok(s1), Ok(s2), Ok(s3)) => (s1, s2, s3),
        (Err(_), _, _) | (_, Err(_), _) | (_, _, Err(_)) => {
            msg.reply(&ctx.http, "引数がおかしいです").await?;
            return Ok(());
        }
    };
    msg.reply(&ctx.http, "時間がかかる場合があります").await?;
    let (m1, m2) = raw_check_suhjong(param_1, param_2, param_3);
    msg.reply(&ctx.http, m1).await?;
    msg.reply(&ctx.http, m2).await?;

    Ok(())
}

fn raw_check_suhjong(yaku1: String, yaku2: String, kouho: String) -> (String, String) {
    if yaku1.len() != 5 || yaku2.len() != 5 || kouho.len() == 0 || kouho.len() > 5 {
        return (
            "引数がおかしいです".to_string(),
            "引数がおかしいです".to_string(),
        );
    }

    let kouho_len = kouho.len();

    let mut temp1: Vec<_> = yaku1
        .chars()
        .filter(|c| c >= &'0' && c <= &'9')
        .map(|c| (c as u32) - ('0' as u32))
        .collect();
    let mut temp2: Vec<_> = yaku2
        .chars()
        .filter(|c| c >= &'0' && c <= &'9')
        .map(|c| (c as u32) - ('0' as u32))
        .collect();
    let mut temp3: Vec<_> = kouho
        .chars()
        .filter(|c| c >= &'0' && c <= &'9')
        .map(|c| (c as u32) - ('0' as u32))
        .collect();
    if temp1.len() != 5 || temp2.len() != 5 || temp3.len() != kouho_len {
        return (
            "引数がおかしいです".to_string(),
            "引数がおかしいです".to_string(),
        );
    }

    temp1.sort();
    temp2.sort();
    temp3.sort();

    let mut max_yaku1 = (Vec::new(), Vec::new());

    let mut max_yaku1_score = None;

    let mut max_zero = (Vec::new(), Vec::new());

    let mut min_yaku = (Vec::new(), Vec::new(), Vec::new());

    let mut min_yaku_score = None;

    let mut max_1 = Vec::new();
    let mut max_1_score = None;
    let mut min_2 = Vec::new();
    let mut min_2_score = None;

    let mut vs_miracle_max_yaku1 = (Vec::new(), Vec::new());
    let mut miracle_max_score1 = None;
    let mut miracle_max_score2 = None;

    let mut vs_miracle_min_yaku1 = (Vec::new(), Vec::new());
    let mut miracle_min_score1 = None;
    let mut miracle_min_score2 = None;

    let mut vs_miracle_mean_yaku1 = (Vec::new(), Vec::new());
    let mut miracle_mean_score1 = None;
    let mut miracle_mean_score2 = None;
    let mut miracle_mean_k = None;

    let mut vs_miracle_kakuritsu_yaku1 = (Vec::new(), Vec::new());
    let mut miracle_kakuritsu_score1 = None;
    let mut miracle_kakuritsu_score2 = None;
    let mut miracle_kakuritsu_k = None;

    for i in 1..=kouho_len {
        for pettern1 in (0..kouho_len).combinations(i) {
            for pettern2 in (0..5).combinations(i) {
                let (temp12, temp32) = temp1.clone().into_iter().enumerate().fold(
                    (Vec::new(), Vec::new()),
                    |(mut t1, mut t3), (i1, e1)| {
                        if pettern2.contains(&i1) {
                            t3.push(e1);
                        } else {
                            t1.push(e1);
                        }
                        (t1, t3)
                    },
                );
                let (mut temp12, temp32) = temp3.clone().into_iter().enumerate().fold(
                    (temp12, temp32),
                    |(mut t1, mut t3), (i3, e3)| {
                        if pettern1.contains(&i3) {
                            t1.push(e3);
                        } else {
                            t3.push(e3);
                        }
                        (t1, t3)
                    },
                );
                let temp22 = temp2.clone();
                temp12.sort();
                let hantei1 = hantei(temp12[0], temp12[1], temp12[2], temp12[3], temp12[4]);
                let score1 = if hantei1.is_empty() {
                    0
                } else {
                    hantei1.iter().next().expect("絶対ある").execute_pettern_l()
                };

                if Some(score1) > max_1_score {
                    max_1_score = Some(score1);
                    max_1 = temp12.clone();
                }

                let mut yyaku2 = Vec::new();
                let mut score2 = None;

                let mut max_yaku2 = (Vec::new(), Vec::new(), Vec::new());

                let mut max_yaku2_score = None;

                let mut max_zero_temp = (Vec::new(), Vec::new());

                let mut min_2_max = Vec::new();
                let mut min_2_max_score = None;

                let mut max_yaku_miracle = Vec::new();
                let max_score_miracle1 = Some(score1);
                let mut max_score_miracle2 = None;
                let mut min_yaku_miracle = Vec::new();
                let min_score_miracle1 = Some(score1);
                let mut min_score_miracle2 = None;
                let mut mean_yaku_miracle = Vec::new();
                let mean_score_miracle1 = Some(score1);
                let mut mean_score_miracle2 = None;
                let mut mean_k_miracle = None;
                let mut kakuritsu_yaku_miracle = Vec::new();
                let kakuritsu_score_miracle1 = Some(score1);
                let mut kakuritsu_score_miracle2 = None;
                let mut kakuritsu_k_miracle = None;
                for j in 1..=5 {
                    for k in 0..=std::cmp::min(i, j) {
                        for pettern3 in (0..i).combinations(k) {
                            for pettern4 in (0..5).combinations(j) {
                                let (temp23, temp33) = temp22.clone().into_iter().enumerate().fold(
                                    (Vec::new(), Vec::new()),
                                    |(mut t2, mut t3), (i2, e2)| {
                                        if pettern4.contains(&i2) {
                                            t3.push(e2);
                                        } else {
                                            t2.push(e2);
                                        }
                                        (t2, t3)
                                    },
                                );
                                let (temp23, _) = temp32.clone().into_iter().enumerate().fold(
                                    (temp23, temp33),
                                    |(mut t2, t3), (i3, e3)| {
                                        if pettern3.contains(&i3) {
                                            t2.push(e3);
                                        }
                                        (t2, t3)
                                    },
                                );
                                let mut max_yaku_temp = Vec::new();
                                let mut max_score_temp = None;
                                let mut min_yaku_temp = Vec::new();
                                let mut min_score_temp = None;
                                let mean_yaku_temp = temp23.clone();
                                let mean_score_temp;
                                let kakuritsu_yaku_temp = temp23.clone();
                                let kakuritsu_temp;
                                let mut scores = Vec::new();
                                for pettern5 in (0..=9).combinations_with_replacement(j - k) {
                                    let mut temp23 =
                                        pettern5.into_iter().fold(temp23.clone(), |mut t2, ez| {
                                            t2.push(ez);
                                            t2
                                        });
                                    temp23.sort();
                                    let hantei2 = hantei(
                                        temp23[0], temp23[1], temp23[2], temp23[3], temp23[4],
                                    );
                                    let score2 = if hantei2.is_empty() {
                                        0
                                    } else {
                                        hantei2.iter().next().expect("絶対ある").execute_pettern_l()
                                    };

                                    if Some(score2) > max_score_temp {
                                        max_score_temp = Some(score2);
                                        max_yaku_temp = temp23.clone();
                                    }

                                    if Some(score2) < min_score_temp || min_score_temp.is_none() {
                                        min_score_temp = Some(score2);
                                        min_yaku_temp = temp23.clone();
                                    }
                                    scores.push(score2);
                                }
                                let (sum, filter_sum, count) = scores.into_iter().fold(
                                    (0, 0, 0),
                                    |(sum, filter_sum, count), value| {
                                        if value > score1 {
                                            (sum + value, filter_sum + 1, count + 1)
                                        } else {
                                            (sum + value, filter_sum, count + 1)
                                        }
                                    },
                                );
                                mean_score_temp = sum as f64 / count as f64;
                                kakuritsu_temp = filter_sum as f64 / count as f64;

                                if max_score_temp > max_score_miracle2 {
                                    max_score_miracle2 = max_score_temp;
                                    max_yaku_miracle = max_yaku_temp;
                                }

                                if min_score_temp > min_score_miracle2 {
                                    min_score_miracle2 = min_score_temp;
                                    min_yaku_miracle = min_yaku_temp;
                                }

                                if Some(mean_score_temp) > mean_score_miracle2 {
                                    mean_score_miracle2 = Some(mean_score_temp);
                                    mean_yaku_miracle = mean_yaku_temp;
                                    mean_k_miracle = Some(kakuritsu_temp);
                                }

                                if Some(kakuritsu_temp) > kakuritsu_k_miracle {
                                    kakuritsu_k_miracle = Some(kakuritsu_temp);
                                    kakuritsu_yaku_miracle = kakuritsu_yaku_temp;
                                    kakuritsu_score_miracle2 = Some(mean_score_temp);
                                }
                            }
                        }
                    }
                }

                if max_score_miracle2 < miracle_max_score2 || miracle_max_score2.is_none() {
                    miracle_max_score1 = max_score_miracle1;
                    miracle_max_score2 = max_score_miracle2;
                    vs_miracle_max_yaku1 = (temp12.clone(), max_yaku_miracle);
                }

                if min_score_miracle2 < miracle_min_score2 || miracle_min_score2.is_none() {
                    miracle_min_score1 = min_score_miracle1;
                    miracle_min_score2 = min_score_miracle2;
                    vs_miracle_min_yaku1 = (temp12.clone(), min_yaku_miracle);
                }

                if mean_score_miracle2 < miracle_mean_score2 || miracle_mean_score2.is_none() {
                    miracle_mean_score1 = mean_score_miracle1;
                    miracle_mean_score2 = mean_score_miracle2;
                    miracle_mean_k = mean_k_miracle;
                    vs_miracle_mean_yaku1 = (temp12.clone(), mean_yaku_miracle);
                }

                if kakuritsu_k_miracle < miracle_kakuritsu_k || miracle_kakuritsu_k.is_none() {
                    miracle_kakuritsu_k = kakuritsu_k_miracle;
                    miracle_kakuritsu_score1 = kakuritsu_score_miracle1;
                    miracle_kakuritsu_score2 = kakuritsu_score_miracle2;
                    vs_miracle_kakuritsu_yaku1 = (temp12.clone(), kakuritsu_yaku_miracle);
                }
                for j in 1..=i {
                    for pettern3 in (0..j).combinations(j) {
                        for pettern4 in (0..5).combinations(j) {
                            let (temp23, temp33) = temp22.clone().into_iter().enumerate().fold(
                                (Vec::new(), Vec::new()),
                                |(mut t2, mut t3), (i2, e2)| {
                                    if pettern4.contains(&i2) {
                                        t3.push(e2);
                                    } else {
                                        t2.push(e2);
                                    }
                                    (t2, t3)
                                },
                            );
                            let (mut temp23, temp33) = temp32.clone().into_iter().enumerate().fold(
                                (temp23, temp33),
                                |(mut t2, t3), (i3, e3)| {
                                    if pettern3.contains(&i3) {
                                        t2.push(e3);
                                    }
                                    (t2, t3)
                                },
                            );
                            let temp13 = temp12.clone();
                            temp23.sort();
                            let hantei2_temp =
                                hantei(temp23[0], temp23[1], temp23[2], temp23[3], temp23[4]);
                            let score2_temp = if hantei2_temp.is_empty() {
                                0
                            } else {
                                hantei2_temp
                                    .iter()
                                    .next()
                                    .expect("絶対ある")
                                    .execute_pettern_l()
                            };
                            if Some(score2_temp) > min_2_max_score {
                                min_2_max_score = Some(score2_temp);
                                min_2_max = temp23.clone();
                            }
                            if score2_temp == 0 {
                                max_zero_temp = (temp13.clone(), temp23.clone());
                            }
                            if Some(score2_temp) > score2 {
                                score2 = Some(score2_temp);
                                yyaku2 = temp23.clone();
                            }

                            temp23.sort();
                            let hantei22 =
                                hantei(temp23[0], temp23[1], temp23[2], temp23[3], temp23[4]);
                            let score22 = if hantei22.is_empty() {
                                0
                            } else {
                                hantei22
                                    .iter()
                                    .next()
                                    .expect("絶対ある")
                                    .execute_pettern_l()
                            };
                            let mut yyaku12 = Vec::new();
                            let mut score12 = None;
                            for k in 1..=j {
                                for pettern5 in (0..k).combinations(k) {
                                    for pettern6 in (0..5).combinations(k) {
                                        let (temp14, temp34) =
                                            temp13.clone().into_iter().enumerate().fold(
                                                (Vec::new(), Vec::new()),
                                                |(mut t1, mut t3), (i1, e1)| {
                                                    if pettern6.contains(&i1) {
                                                        t3.push(e1);
                                                    } else {
                                                        t1.push(e1);
                                                    }
                                                    (t1, t3)
                                                },
                                            );
                                        let (mut temp14, _) = temp33
                                            .clone()
                                            .into_iter()
                                            .enumerate()
                                            .fold((temp14, temp34), |(mut t1, t3), (i3, e3)| {
                                                if pettern5.contains(&i3) {
                                                    t1.push(e3);
                                                }
                                                (t1, t3)
                                            });
                                        temp14.sort();
                                        let hantei1_temp = hantei(
                                            temp14[0], temp14[1], temp14[2], temp14[3], temp14[4],
                                        );
                                        let score1_temp = if hantei1_temp.is_empty() {
                                            0
                                        } else {
                                            hantei1_temp
                                                .iter()
                                                .next()
                                                .expect("絶対ある")
                                                .execute_pettern_l()
                                        };
                                        if Some(score1_temp) > score12 {
                                            score12 = Some(score1_temp);
                                            yyaku12 = temp14.clone();
                                        }
                                    }
                                }
                            }
                            let score12 = score12.expect("絶対Some");
                            match max_yaku2_score {
                                Some((sc12, sc22))
                                    if score22 as i64 - score12 as i64
                                        > sc22 as i64 - sc12 as i64 =>
                                {
                                    max_yaku2_score = Some((score12, score22));
                                    max_yaku2 = (temp12.clone(), yyaku12, temp23.clone());
                                }
                                None => {
                                    max_yaku2_score = Some((score12, score22));
                                    max_yaku2 = (temp12.clone(), yyaku12, temp23.clone());
                                }
                                _ => (),
                            }
                        }
                    }
                }
                if min_2_max_score < min_2_score || min_2_score.is_none() {
                    min_2_score = min_2_max_score;
                    min_2 = min_2_max;
                }
                let score2 = score2.expect("絶対Some");
                if score2 == 0 {
                    max_zero = max_zero_temp;
                }
                match max_yaku1_score {
                    Some((sc1, sc2)) if score1 as i64 - score2 as i64 > sc1 as i64 - sc2 as i64 => {
                        max_yaku1_score = Some((score1, score2));
                        max_yaku1 = (temp12.clone(), yyaku2.clone());
                    }
                    None => {
                        max_yaku1_score = Some((score1, score2));
                        max_yaku1 = (temp12.clone(), yyaku2.clone());
                    }
                    _ => (),
                }

                match (min_yaku_score, max_yaku2_score) {
                    (Some((sc_min1, sc_min2)), Some((sc_max1, sc_max2)))
                        if (sc_max2 as i64 - sc_max1 as i64) < sc_min2 as i64 - sc_min1 as i64 =>
                    {
                        min_yaku_score = max_yaku2_score;
                        min_yaku = max_yaku2;
                    }
                    (None, Some(_)) => {
                        min_yaku_score = max_yaku2_score;
                        min_yaku = max_yaku2;
                    }
                    _ => (),
                }
            }
        }
    }
    let (yaku1, yaku2) = max_yaku1;
    let (score1, score2) = max_yaku1_score.expect("絶対Some!!");
    let expressions1 = yaku1
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let expressions2 = yaku2
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let (yaku3_pre, yaku3, yaku4) = min_yaku;
    let (score3, score4) = min_yaku_score.expect("絶対Some!!");
    let expressions3_pre = yaku3_pre
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let expressions3 = yaku3
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let expressions4 = yaku4
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let (yaku5, yaku6) = (max_1, min_2);
    let score5 = max_1_score.expect("絶対Some!!");
    let score6 = min_2_score.expect("絶対Some!!");
    let expressions5 = yaku5
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let expressions6 = yaku6
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let (yaku7, yaku8) = vs_miracle_min_yaku1;
    let score7 = miracle_min_score1.expect("絶対Some!!");
    let score8 = miracle_min_score2.expect("絶対Some!!");
    let expressions7 = yaku7
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let expressions8 = yaku8
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let (yaku9, yaku10) = vs_miracle_mean_yaku1;
    let score9 = miracle_mean_score1.expect("絶対Some!!");
    let score10 = miracle_mean_score2.expect("絶対Some!!");
    let kakuritsu_10 = miracle_mean_k.expect("絶対Some!!");
    let expressions9 = yaku9
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let expressions10 = yaku10
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let (yaku11, yaku12) = vs_miracle_kakuritsu_yaku1;
    let score11 = miracle_kakuritsu_score1.expect("絶対Some!!");
    let score12 = miracle_kakuritsu_score2.expect("絶対Some!!");
    let kakuritsu_12 = miracle_kakuritsu_k.expect("絶対Some!!");
    let expressions11 = yaku11
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let expressions12 = yaku12
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let (yaku13, yaku14) = vs_miracle_max_yaku1;
    let score13 = miracle_max_score1.expect("絶対Some!!");
    let score14 = miracle_max_score2.expect("絶対Some!!");
    let expressions13 = yaku13
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    let expressions14 = yaku14
        .into_iter()
        .map(|value| value.to_string())
        .collect::<String>();
    (
        format!("{:?}\n(得点{}) {}\n```\n{}\n```\n(得点{}) {}\n```\n{}\n```\n\n(得点{}) {}\n```\n{}\n{}\n```\n(得点{}) {}\n```\n{}\n```\n(得点{})\n```\n{}\n```\n\n(得点{})\n```\n{}\n```", max_zero, score1, if score1 > score2 {"WIN"} else if score1 < score2 {"LOSE"} else {"DRAW"}, expressions1, score2, if score2 > score1 {"WIN"} else if score2 < score1 {"LOSE"} else {"DRAW"}, expressions2, score3, if score3 > score4 {"WIN"} else if score3 < score4 {"LOSE"} else {"DRAW"}, expressions3_pre, expressions3, score4, if score4 > score3 {"WIN"} else if score4 < score3 {"LOSE"} else {"DRAW"}, expressions4, score5, expressions5, score6, expressions6),
        format!("min:\n(得点{}) {}\n```\n{}\n```\n(得点{}) {}\n```\n{}\n```\n\nmean:\n(得点{}) {}\n```\n{}\n```\n(得点{:.2}){:.2}% {}\n```\n[{}]\n```\n\nkakuritsu:\n(得点{}) {}\n```\n{}\n```\n(得点{:.2}){:.2}% {}\n```\n[{}]\n```\n\nmax:\n(得点{}) {}\n```\n{}\n```\n(得点{}) {}\n```\n{}\n```", score7, if score7 > score8 {"WIN"} else if score7 < score8 {"LOSE"} else {"DRAW"}, expressions7, score8, if score8 > score7 {"WIN"} else if score8 < score7 {"LOSE"} else {"DRAW"}, expressions8, score9, if score9 as f64 > score10 {"WIN"} else if (score9 as f64) < score10 {"LOSE"} else {"DRAW"}, expressions9, score10, kakuritsu_10 * 100.0, if score10 > score9 as f64 {"WIN"} else if score10 < score9 as f64 {"LOSE"} else {"DRAW"}, expressions10, score11, if score11 as f64 > score12 {"WIN"} else if (score11 as f64) < score12 {"LOSE"} else {"DRAW"}, expressions11, score12, kakuritsu_12 * 100.0, if score12 > score11 as f64 {"WIN"} else if score12 < score11 as f64 {"LOSE"} else {"DRAW"}, expressions12, score13, if score13 > score14 {"WIN"} else if score13 < score14 {"LOSE"} else {"DRAW"}, expressions13, score14, if score14 > score13 {"WIN"} else if score14 < score13 {"LOSE"} else {"DRAW"}, expressions14, ),
    )
}

#[command]
#[description = "数雀の軌跡"]
async fn suhjong_miracle(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let (param_1, param_2, param_3) = match (
        args.single::<String>(),
        args.single::<String>(),
        args.single::<String>(),
    ) {
        (Ok(s1), Ok(s2), Ok(s3)) => (s1, s2, s3),
        (Err(_), _, _) | (_, Err(_), _) | (_, _, Err(_)) => {
            msg.reply(&ctx.http, "引数がおかしいです").await?;
            return Ok(());
        }
    };
    let m = raw_suhjong_miracle(param_1, param_2, param_3);
    msg.reply(&ctx.http, m).await?;

    Ok(())
}

fn raw_suhjong_miracle(yaku1: String, yaku2: String, kouho: String) -> String {
    if yaku1.len() != 5 || yaku2.len() != 5 || kouho.len() == 0 || kouho.len() > 5 {
        return "引数がおかしいです".to_string();
    }

    let kouho = if kouho == "None" {
        String::new()
    } else {
        kouho
    };

    let kouho_len = kouho.len();

    let mut temp1: Vec<_> = yaku1
        .chars()
        .filter(|c| c >= &'0' && c <= &'9')
        .map(|c| (c as u32) - ('0' as u32))
        .collect();
    let mut temp2: Vec<_> = yaku2
        .chars()
        .filter(|c| c >= &'0' && c <= &'9')
        .map(|c| (c as u32) - ('0' as u32))
        .collect();
    let mut temp3: Vec<_> = kouho
        .chars()
        .filter(|c| c >= &'0' && c <= &'9')
        .map(|c| (c as u32) - ('0' as u32))
        .collect();
    if temp1.len() != 5 || temp2.len() != 5 || temp3.len() != kouho_len {
        return "引数がおかしいです".to_string();
    }

    temp1.sort();
    temp2.sort();
    temp3.sort();

    let mut max_yaku = HashSet::new();
    let mut max_score = None;
    let mut min_yaku = HashSet::new();
    let mut min_score = None;
    let mut mean_yaku = Vec::new();
    let mut mean_score = None;
    let mut mean_k = None;
    let mut kakuritsu_yaku = Vec::new();
    let mut kakuritsu_k = None;
    let mut kakuritsu_score = None;

    let hantei2 = hantei(temp2[0], temp2[1], temp2[2], temp2[3], temp2[4]);
    let score2 = if hantei2.is_empty() {
        0
    } else {
        hantei2.iter().next().expect("絶対ある").execute_pettern_l()
    };

    for i in 1..=5 {
        for j in 0..=std::cmp::min(i, kouho_len) {
            for pettern1 in (0..kouho_len).combinations(j) {
                for pettern2 in (0..5).combinations(i) {
                    let (temp12, temp32) = temp1.clone().into_iter().enumerate().fold(
                        (Vec::new(), Vec::new()),
                        |(mut t1, mut t3), (i1, e1)| {
                            if pettern2.contains(&i1) {
                                t3.push(e1);
                            } else {
                                t1.push(e1);
                            }
                            (t1, t3)
                        },
                    );
                    let (temp12, _) = temp3.clone().into_iter().enumerate().fold(
                        (temp12, temp32),
                        |(mut t1, t3), (i3, e3)| {
                            if pettern1.contains(&i3) {
                                t1.push(e3);
                            }
                            (t1, t3)
                        },
                    );
                    let mut max_yaku_temp = HashSet::new();
                    let mut max_score_temp = None;
                    let mut min_yaku_temp = HashSet::new();
                    let mut min_score_temp = None;
                    let mean_yaku_temp = temp12.clone();
                    let mean_score_temp;
                    let kakuritsu_yaku_temp = temp12.clone();
                    let kakuritsu_temp;
                    let mut scores = Vec::new();
                    for pettern3 in (0..=9).combinations_with_replacement(i - j) {
                        let mut temp12 = pettern3.into_iter().fold(temp12.clone(), |mut t1, ez| {
                            t1.push(ez);
                            t1
                        });
                        temp12.sort();
                        let hantei1 = hantei(temp12[0], temp12[1], temp12[2], temp12[3], temp12[4]);
                        let score1 = if hantei1.is_empty() {
                            0
                        } else {
                            hantei1.iter().next().expect("絶対ある").execute_pettern_l()
                        };

                        if Some(score1) > max_score_temp {
                            max_score_temp = Some(score1);
                            max_yaku_temp = hantei1.clone();
                        }

                        if Some(score1) < min_score_temp || min_score_temp.is_none() {
                            min_score_temp = Some(score1);
                            min_yaku_temp = hantei1.clone();
                        }

                        scores.push(score1);
                    }

                    let (sum, filter_sum, count) =
                        scores
                            .into_iter()
                            .fold((0, 0, 0), |(sum, filter_sum, count), value| {
                                if value >= score2 {
                                    (sum + value, filter_sum + 1, count + 1)
                                } else {
                                    (sum + value, filter_sum, count + 1)
                                }
                            });
                    mean_score_temp = sum as f64 / count as f64;
                    kakuritsu_temp = filter_sum as f64 / count as f64;

                    if max_score_temp > max_score {
                        max_score = max_score_temp;
                        max_yaku = max_yaku_temp;
                    }

                    if min_score_temp > min_score {
                        min_score = min_score_temp;
                        min_yaku = min_yaku_temp;
                    }

                    if Some(mean_score_temp) > mean_score {
                        mean_score = Some(mean_score_temp);
                        mean_yaku = mean_yaku_temp;
                        mean_k = Some(kakuritsu_temp);
                    }

                    if Some(kakuritsu_temp) > kakuritsu_k {
                        kakuritsu_k = Some(kakuritsu_temp);
                        kakuritsu_yaku = kakuritsu_yaku_temp;
                        kakuritsu_score = Some(mean_score_temp);
                    }
                }
            }
        }
    }

    let max_score = max_score.expect("絶対Some!!");
    let min_score = min_score.expect("絶対Some!!");
    let mean_score = mean_score.expect("絶対Some!!");
    let mean_k = mean_k.expect("絶対Some!!");
    let kakuritsu_score = kakuritsu_score.expect("絶対Some!!");
    let kakuritsu_k = kakuritsu_k.expect("絶対Some!!");
    let max_expressions = if max_yaku.is_empty() {
        "no solutions!!".to_string()
    } else {
        max_yaku
            .into_iter()
            .map(|narabi| narabi.to_string())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
            .join("\n")
    };
    let min_expressions = if min_yaku.is_empty() {
        "no solutions!!".to_string()
    } else {
        min_yaku
            .into_iter()
            .map(|narabi| narabi.to_string())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>()
            .join("\n")
    };

    format!("max: 得点({})\n```\n{}\n```\nmin: 得点({})\n```\n{}\n```\nmean: 得点({:.2}) {:.2}%\n```\n{:?}\n```\nkakuritsu: 得点({:.2}) {:.2}%\n```\n{:?}\n```", max_score, max_expressions, min_score, min_expressions, mean_score, mean_k * 100.0, mean_yaku, kakuritsu_score, kakuritsu_k * 100.0, kakuritsu_yaku)
}

#[command]
#[description = r"
数雀をプレイする
>sj START
>sj {河に捨てる} {河から拾う}
>sj SHOUB {河に捨てる} {河から拾う}
>sj (BOT|BOT2|BOT3|BOT4)
"]
async fn sj(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    play_suhjong(ctx, msg, args).await
}

#[command]
#[description = r"
数雀をプレイする
>play_suhjong START
>play_suhjong {河に捨てる} {河から拾う}
>play_suhjong SHOUB {河に捨てる} {河から拾う}
>play_suhjong (BOT|BOT2|BOT3|BOT4)
"]
async fn play_suhjong(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let param1 = match args.single::<String>() {
        Ok(s) => s,
        Err(_) => {
            msg.reply(&ctx.http, "引数がおかしいです").await?;
            return Ok(());
        }
    };

    match &(param1.to_uppercase())[..] {
        "START" => {
            let m = raw_play_start_suhjong();
            msg.reply(&ctx.http, m).await?;
        },
        "MIRACLE" => {
            let ref_content = match msg.message_reference.clone() {
                Some(reference) => {
                    let channel_id = reference.channel_id;
                    let message_id = match reference.message_id {
                        Some(m_id) => m_id,
                        None => {
                            msg.reply_ping(&ctx.http, "エラー").await?;
                            return Ok(());
                        }
                    };
                    let ref_msg = channel_id.message(&ctx.http, message_id).await?;
                    ref_msg.content
                }
                None => {
                    msg.reply_ping(&ctx.http, "エラー").await?;
                    return Ok(());
                }
            };

            let ban = raw_format_play_suhjong(ref_content);

            let m = match ban {
                Some(b) => b.miracle().to_string(),
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            msg.reply(&ctx.http, m).await?;
        },
        "CHECK" => {
            let ref_content = match msg.message_reference.clone() {
                Some(reference) => {
                    let channel_id = reference.channel_id;
                    let message_id = match reference.message_id {
                        Some(m_id) => m_id,
                        None => {
                            msg.reply_ping(&ctx.http, "エラー").await?;
                            return Ok(());
                        }
                    };
                    let ref_msg = channel_id.message(&ctx.http, message_id).await?;
                    ref_msg.content
                }
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            let ban = raw_format_play_suhjong(ref_content);

            let (m1, m2) = match ban {
                Some(b) if b.turn != 0 => {
                    msg.reply(&ctx.http, "時間がかかる場合があります").await?;
                    b.check().to_tuple_string()
                }
                _ => {
                    msg.reply_ping(&ctx.http, "エラー").await?;
                    return Ok(());
                }
            };
            msg.reply(&ctx.http, m1).await?;
            msg.reply(&ctx.http, m2).await?;
        },
        "S" | "SHOUB" => {
            let (sute_str, hiro_str) = match (args.single::<String>(), args.single::<String>()) {
                (Ok(s1), Ok(s2)) if s1.len() >= 1 && s1.len() <= 5 && s2.len() <= 5 => (s1, s2),
                _ => {
                    msg.reply(&ctx.http, "引数がおかしいです").await?;
                    return Ok(());
                }
            };

            let sute: Vec<_> = sute_str.chars().map(|c| c as u32 - '0' as u32).collect();
            let hiro: Vec<_> = match &(hiro_str.to_lowercase())[..] {
                "-" | "none" => Vec::new(),
                _ => hiro_str.chars().map(|c| c as u32 - '0' as u32).collect()
            };

            let ref_content = match msg.message_reference.clone() {
                Some(reference) => {
                    let channel_id = reference.channel_id;
                    let message_id = match reference.message_id {
                        Some(m_id) => m_id,
                        None => {
                            msg.reply_ping(&ctx.http, "エラー").await?;
                            return Ok(());
                        }
                    };
                    let ref_msg = channel_id.message(&ctx.http, message_id).await?;
                    ref_msg.content
                }
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            let ban = raw_format_play_suhjong(ref_content);

            let m = match ban {
                Some(b) => match b.shoub(sute, hiro) {
                    Some(nb) => nb.to_string(),
                    None => {
                        msg.reply_ping(&ctx.http, "進行エラー").await?;
                        return Ok(());
                    }
                },
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            msg.reply(&ctx.http, m).await?;
        },
        "KANSOU" => {
            let (sute_str, hiro_str, draw_str) = match (
                args.single::<String>(),
                args.single::<String>(),
                args.single::<String>(),
            ) {
                (Ok(s1), Ok(s2), Ok(s3)) if s1.len() <= 5 && s2.len() <= 5 && s3.len() <= 5 => {
                    (s1, s2, s3)
                }
                _ => {
                    msg.reply(&ctx.http, "引数がおかしいです").await?;
                    return Ok(());
                }
            };

            let sute: Vec<_> = sute_str.chars().map(|c| c as u32 - '0' as u32).collect();
            let hiro: Vec<_> = match &(hiro_str.to_lowercase())[..] {
                "-" | "none" => Vec::new(),
                _ => hiro_str.chars().map(|c| c as u32 - '0' as u32).collect(),
            };
            let draw: Vec<_> = match &(draw_str.to_lowercase())[..] {
                "-" | "none" => Vec::new(),
                _ => draw_str.chars().map(|c| c as u32 - '0' as u32).collect(),
            };

            let ref_content = match msg.message_reference.clone() {
                Some(reference) => {
                    let channel_id = reference.channel_id;
                    let message_id = match reference.message_id {
                        Some(m_id) => m_id,
                        None => {
                            msg.reply_ping(&ctx.http, "エラー").await?;
                            return Ok(());
                        }
                    };
                    let ref_msg = channel_id.message(&ctx.http, message_id).await?;
                    ref_msg.content
                }
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            let ban = raw_format_play_suhjong(ref_content);

            let m = match ban {
                Some(b) => match b.kansou(sute, hiro, draw) {
                    Some(nb) => nb.to_string(),
                    None => {
                        msg.reply_ping(&ctx.http, "進行エラー").await?;
                        return Ok(());
                    }
                },
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            msg.reply(&ctx.http, m).await?;
        },
        "BOT" | "BOT2" | "BOT3" | "BOT4" => {
            let ref_content = match msg.message_reference.clone() {
                Some(reference) => {
                    let channel_id = reference.channel_id;
                    let message_id = match reference.message_id {
                        Some(m_id) => m_id,
                        None => {
                            msg.reply_ping(&ctx.http, "エラー").await?;
                            return Ok(());
                        }
                    };
                    let ref_msg = channel_id.message(&ctx.http, message_id).await?;
                    ref_msg.content
                }
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            let ban = raw_format_play_suhjong(ref_content);

            msg.reply(&ctx.http, "時間がかかる場合があります").await?;

            let m = match ban {
                Some(b) => match &(param1.to_uppercase())[..] {
                    "BOT2" => b.bot2_play().to_string(),
                    "BOT3" => b.bot3_play().to_string(),
                    "BOT4" => b.bot4_play().to_string(),
                    _ => b.bot_play().to_string(),
                },
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            msg.reply(&ctx.http, m).await?;
        },
        _ => {
            let sute_str = param1;
            let hiro_str = match args.single::<String>() {
                Ok(s2) if sute_str.len() >= 1 && sute_str.len() <= 5 && s2.len() <= 5 => s2,
                _ => {
                    msg.reply(&ctx.http, "引数がおかしいです").await?;
                    return Ok(());
                }
            };

            let sute: Vec<_> = sute_str.chars().map(|c| c as u32 - '0' as u32).collect();
            let hiro: Vec<_> = match &(hiro_str.to_lowercase())[..] {
                "none" | "-" => Vec::new(),
                _ => hiro_str.chars().map(|c| c as u32 - '0' as u32).collect(),
            };

            let ref_content = match msg.message_reference.clone() {
                Some(reference) => {
                    let channel_id = reference.channel_id;
                    let message_id = match reference.message_id {
                        Some(m_id) => m_id,
                        None => {
                            msg.reply_ping(&ctx.http, "エラー").await?;
                            return Ok(());
                        }
                    };
                    let ref_msg = channel_id.message(&ctx.http, message_id).await?;
                    ref_msg.content
                }
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            let ban = raw_format_play_suhjong(ref_content);

            let m = match ban {
                Some(b) => match b.next(sute, hiro) {
                    Some(nb) => nb.to_string(),
                    None => {
                        msg.reply_ping(&ctx.http, "進行エラー").await?;
                        return Ok(());
                    }
                },
                None => {
                    msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
                    return Ok(());
                }
            };

            msg.reply(&ctx.http, m).await?;
        }
    };

    Ok(())
}

fn raw_play_start_suhjong() -> String {
    let mut yama = vec![5; 10]
        .into_iter()
        .enumerate()
        .map(|(index, num)| vec![index as u32; num])
        .fold(Vec::new(), |mut r, mut v| {
            r.append(&mut v);
            r
        });
    yama.shuffle(&mut rand::thread_rng());
    let mut player1 = yama.split_off(yama.len() - 5);
    let mut player2 = yama.split_off(yama.len() - 5);
    player1.sort();
    player2.sort();
    let player1 = [player1[0], player1[1], player1[2], player1[3], player1[4]];
    let player2 = [player2[0], player2[1], player2[2], player2[3], player2[4]];
    let yama: Vec<_> = (0..=9)
        .into_iter()
        .map(|n| yama.iter().filter(|&&e| e == n).count())
        .collect();
    let yama = [
        yama[0], yama[1], yama[2], yama[3], yama[4], yama[5], yama[6], yama[7], yama[8], yama[9],
    ];

    let ban = SuhjongMen {
        turn: 0,
        is_shoubed: false,
        player1,
        player2,
        river1: Vec::new(),
        river2: Vec::new(),
        yama,
        is_player1: true,
        is_finised: false,
    };

    ban.to_string()
}

#[derive(Debug, Clone)]
struct SuhjongMen {
    turn: usize,
    is_shoubed: bool,
    player1: [u32; 5],
    player2: [u32; 5],
    river1: Vec<u32>,
    river2: Vec<u32>,
    yama: [usize; 10],
    is_player1: bool,
    is_finised: bool,
}

impl SuhjongMen {
    fn to_string(self) -> String {
        let turn = self.turn;
        let is_shoubed = match self.is_shoubed {
            true => "〇",
            false => "-",
        };
        let player1: String = self.player1.iter().map(|n| n.to_string()).collect();
        let player2: String = self.player2.iter().map(|n| n.to_string()).collect();
        let river1: String = self.river1.iter().map(|n| n.to_string()).collect();
        let river2: String = self.river2.iter().map(|n| n.to_string()).collect();
        let is_player1 = match self.is_player1 {
            true => "〇",
            false => "-",
        };
        let is_player2 = match !self.is_player1 {
            true => "〇",
            false => "-",
        };
        format!(
            r"数雀{} ターン経過:{} 勝負:{}
player1 番:{}
```
{}
````河 {}`
山 ||0:{} 1:{} 2:{} 3:{} 4:{} 5:{} 6:{} 7:{} 8:{} 9:{}||
`河 {}`
```
{}
```player2 番:{}",
            if self.is_finised {
                "勝負あり"
            } else {
                "プレイ中"
            },
            turn,
            is_shoubed,
            is_player1,
            player1,
            river1,
            self.yama[0],
            self.yama[1],
            self.yama[2],
            self.yama[3],
            self.yama[4],
            self.yama[5],
            self.yama[6],
            self.yama[7],
            self.yama[8],
            self.yama[9],
            river2,
            player2,
            is_player2
        )
    }

    fn check(&self) -> SuhjongCheck {
        let (mut temp1, mut temp2, mut temp3) = if self.is_player1 {
            (
                self.player1.clone(),
                self.player2.clone(),
                self.river2.clone(),
            )
        } else {
            (
                self.player2.clone(),
                self.player1.clone(),
                self.river1.clone(),
            )
        };

        temp1.sort();
        temp2.sort();
        temp3.sort();

        let kouho_len = temp3.len();
        let mut max_yaku = (Vec::new(), Vec::new());
        let mut max_score = None;
        let mut max_swap = (Vec::new(), Vec::new());
        let mut min_yaku = (Vec::new(), Vec::new(), Vec::new());
        let mut min_score = None;
        let mut min_swap = (Vec::new(), Vec::new());
        let mut max_1 = Vec::new();
        let mut max_1_score = None;
        let mut max_1_swap = (Vec::new(), Vec::new());

        let mut min_2 = Vec::new();
        let mut min_2_score = None;
        let mut min_2_swap = (Vec::new(), Vec::new());
        let mut vs_miracle_max_yaku = (Vec::new(), Vec::new());
        let mut miracle_max_score1 = None;
        let mut miracle_max_score2 = None;
        let mut miracle_max_swap = (Vec::new(), Vec::new());
        let mut vs_miracle_min_yaku = (Vec::new(), Vec::new());
        let mut miracle_min_score1 = None;
        let mut miracle_min_score2 = None;
        let mut miracle_min_swap = (Vec::new(), Vec::new());
        let mut vs_miracle_mean_yaku = (Vec::new(), Vec::new());
        let mut miracle_mean_score1 = None;
        let mut miracle_mean_score2 = None;
        let mut miracle_mean_k = None;
        let mut miracle_mean_swap = (Vec::new(), Vec::new());
        let mut vs_miracle_kakuritsu_yaku = (Vec::new(), Vec::new());
        let mut miracle_kakuritsu_score1 = None;
        let mut miracle_kakuritsu_score2 = None;
        let mut miracle_kakuritsu_k = None;
        let mut miracle_kakuritsu_swap = (Vec::new(), Vec::new());
        for i in 1..=kouho_len {
            for pettern1 in (0..kouho_len).combinations(i) {
                for pettern2 in (0..5).combinations(i) {
                    let (temp12, temp32, sute) = temp1.clone().iter().enumerate().fold(
                        (Vec::new(), Vec::new(), Vec::new()),
                        |(mut t1, mut t3, mut sute), (i1, &e1)| {
                            if pettern2.contains(&i1) {
                                t3.push(e1);
                                sute.push(e1);
                            } else {
                                t1.push(e1);
                            }
                            (t1, t3, sute)
                        },
                    );
                    let (mut temp12, temp32, hiro) = temp3.clone().into_iter().enumerate().fold(
                        (temp12, temp32, Vec::new()),
                        |(mut t1, mut t3, mut hiro), (i3, e3)| {
                            if pettern1.contains(&i3) {
                                t1.push(e3);
                                hiro.push(e3);
                            } else {
                                t3.push(e3);
                            }
                            (t1, t3, hiro)
                        },
                    );
                    let swap = (sute, hiro);
                    let temp22 = temp2.clone();
                    temp12.sort();
                    let hantei1 = hantei(temp12[0], temp12[1], temp12[2], temp12[3], temp12[4]);
                    let score1 = if hantei1.is_empty() {
                        0
                    } else {
                        hantei1.iter().next().expect("絶対ある").execute_pettern_l()
                    };
                    if Some(score1) > max_1_score {
                        max_1_score = Some(score1);
                        max_1 = temp12.clone();
                        max_1_swap = swap.clone();
                    }
                    let mut yyaku2 = Vec::new();
                    let mut score2 = None;
                    let mut max_yaku2 = (Vec::new(), Vec::new(), Vec::new());
                    let mut max_yaku2_score = None;
                    let mut min_2_max = Vec::new();
                    let mut min_2_max_score = None;
                    let mut max_yaku_miracle = Vec::new();
                    let max_score_miracle1 = Some(score1);
                    let mut max_score_miracle2 = None;
                    let mut min_yaku_miracle = Vec::new();
                    let min_score_miracle1 = Some(score1);
                    let mut min_score_miracle2 = None;
                    let mut mean_yaku_miracle = Vec::new();
                    let mean_score_miracle1 = Some(score1);
                    let mut mean_score_miracle2 = None;
                    let mut mean_k_miracle = None;
                    let mut kakuritsu_yaku_miracle = Vec::new();
                    let kakuritsu_score_miracle1 = Some(score1);
                    let mut kakuritsu_score_miracle2 = None;
                    let mut kakuritsu_k_miracle = None;
                    for j in 1..=5 {
                        for k in 0..=std::cmp::min(i, j) {
                            for pettern3 in (0..i).combinations(k) {
                                for pettern4 in (0..5).combinations(j) {
                                    let (temp23, temp33) = temp22.clone().iter().enumerate().fold(
                                        (Vec::new(), Vec::new()),
                                        |(mut t2, mut t3), (i2, &e2)| {
                                            if pettern4.contains(&i2) {
                                                t3.push(e2);
                                            } else {
                                                t2.push(e2);
                                            }
                                            (t2, t3)
                                        },
                                    );
                                    let (temp23, _) = temp32.clone().into_iter().enumerate().fold(
                                        (temp23, temp33),
                                        |(mut t2, t3), (i3, e3)| {
                                            if pettern3.contains(&i3) {
                                                t2.push(e3);
                                            }
                                            (t2, t3)
                                        },
                                    );
                                    let mut max_yaku_temp = Vec::new();
                                    let mut max_score_temp = None;
                                    let mut min_yaku_temp = Vec::new();
                                    let mut min_score_temp = None;
                                    let mean_yaku_temp = temp23.clone();
                                    let mean_score_temp;
                                    let kakuritsu_yaku_temp = temp23.clone();
                                    let kakuritsu_temp;
                                    let mut scores = Vec::new();
                                    for pettern5 in
                                        (0..=9).combinations_with_replacement(j - k).filter(|p| {
                                            (0..=9).into_iter().all(|n| {
                                                p.iter().filter(|&&m| n == m).count()
                                                    <= self.yama[n as usize]
                                            })
                                        })
                                    {
                                        let mut temp23 = pettern5.into_iter().fold(
                                            temp23.clone(),
                                            |mut t2, ez| {
                                                t2.push(ez);
                                                t2
                                            },
                                        );
                                        temp23.sort();
                                        let hantei2 = hantei(
                                            temp23[0], temp23[1], temp23[2], temp23[3], temp23[4],
                                        );
                                        let score2 = if hantei2.is_empty() {
                                            0
                                        } else {
                                            hantei2
                                                .iter()
                                                .next()
                                                .expect("絶対ある")
                                                .execute_pettern_l()
                                        };
                                        if Some(score2) > max_score_temp {
                                            max_score_temp = Some(score2);
                                            max_yaku_temp = temp23.clone();
                                        }

                                        if Some(score2) < min_score_temp || min_score_temp.is_none()
                                        {
                                            min_score_temp = Some(score2);
                                            min_yaku_temp = temp23.clone();
                                        }
                                        scores.push(score2);
                                    }
                                    let (sum, filter_sum, count) = scores.into_iter().fold(
                                        (0, 0, 0),
                                        |(sum, filter_sum, count), value| {
                                            if value > score1 {
                                                (sum + value, filter_sum + 1, count + 1)
                                            } else {
                                                (sum + value, filter_sum, count + 1)
                                            }
                                        },
                                    );
                                    mean_score_temp = sum as f64 / count as f64;
                                    kakuritsu_temp = filter_sum as f64 / count as f64;
                                    if max_score_temp > max_score_miracle2 {
                                        max_score_miracle2 = max_score_temp;
                                        max_yaku_miracle = max_yaku_temp;
                                    }
                                    if min_score_temp > min_score_miracle2 {
                                        min_score_miracle2 = min_score_temp;
                                        min_yaku_miracle = min_yaku_temp;
                                    }
                                    if Some(mean_score_temp) > mean_score_miracle2 {
                                        mean_score_miracle2 = Some(mean_score_temp);
                                        mean_yaku_miracle = mean_yaku_temp;
                                        mean_k_miracle = Some(kakuritsu_temp);
                                    }
                                    if Some(kakuritsu_temp) > kakuritsu_k_miracle {
                                        kakuritsu_k_miracle = Some(kakuritsu_temp);
                                        kakuritsu_yaku_miracle = kakuritsu_yaku_temp;
                                        kakuritsu_score_miracle2 = Some(mean_score_temp);
                                    }
                                }
                            }
                        }
                    }
                    if max_score_miracle2 < miracle_max_score2 || miracle_max_score2.is_none() {
                        miracle_max_score1 = max_score_miracle1;
                        miracle_max_score2 = max_score_miracle2;
                        miracle_max_swap = swap.clone();
                        vs_miracle_max_yaku = (temp12.clone(), max_yaku_miracle);
                    }
                    if min_score_miracle2 < miracle_min_score2 || miracle_min_score2.is_none() {
                        miracle_min_score1 = min_score_miracle1;
                        miracle_min_score2 = min_score_miracle2;
                        miracle_min_swap = swap.clone();
                        vs_miracle_min_yaku = (temp12.clone(), min_yaku_miracle);
                    }
                    if mean_score_miracle2 < miracle_mean_score2 || miracle_mean_score2.is_none() {
                        miracle_mean_score1 = mean_score_miracle1;
                        miracle_mean_score2 = mean_score_miracle2;
                        miracle_mean_k = mean_k_miracle;
                        miracle_mean_swap = swap.clone();
                        vs_miracle_mean_yaku = (temp12.clone(), mean_yaku_miracle);
                    }
                    if kakuritsu_k_miracle < miracle_kakuritsu_k || miracle_kakuritsu_k.is_none() {
                        miracle_kakuritsu_k = kakuritsu_k_miracle;
                        miracle_kakuritsu_score1 = kakuritsu_score_miracle1;
                        miracle_kakuritsu_score2 = kakuritsu_score_miracle2;
                        miracle_kakuritsu_swap = swap.clone();
                        vs_miracle_kakuritsu_yaku = (temp12.clone(), kakuritsu_yaku_miracle);
                    }
                    for j in 1..=i {
                        for pettern3 in (0..i).combinations(j) {
                            for pettern4 in (0..5).combinations(j) {
                                let (temp23, temp33) = temp22.clone().iter().enumerate().fold(
                                    (Vec::new(), Vec::new()),
                                    |(mut t2, mut t3), (i2, &e2)| {
                                        if pettern4.contains(&i2) {
                                            t3.push(e2);
                                        } else {
                                            t2.push(e2);
                                        }
                                        (t2, t3)
                                    },
                                );
                                let (mut temp23, temp33) = temp32
                                    .clone()
                                    .into_iter()
                                    .enumerate()
                                    .fold((temp23, temp33), |(mut t2, t3), (i3, e3)| {
                                        if pettern3.contains(&i3) {
                                            t2.push(e3);
                                        }
                                        (t2, t3)
                                    });
                                let temp13 = temp12.clone();
                                temp23.sort();
                                let hantei2_temp =
                                    hantei(temp23[0], temp23[1], temp23[2], temp23[3], temp23[4]);
                                let score2_temp = if hantei2_temp.is_empty() {
                                    0
                                } else {
                                    hantei2_temp
                                        .iter()
                                        .next()
                                        .expect("絶対ある")
                                        .execute_pettern_l()
                                };
                                if Some(score2_temp) > min_2_max_score {
                                    min_2_max_score = Some(score2_temp);
                                    min_2_max = temp23.clone();
                                }
                                if Some(score2_temp) > score2 {
                                    score2 = Some(score2_temp);
                                    yyaku2 = temp23.clone();
                                }
                                temp23.sort();
                                let hantei22 =
                                    hantei(temp23[0], temp23[1], temp23[2], temp23[3], temp23[4]);
                                let score22 = if hantei22.is_empty() {
                                    0
                                } else {
                                    hantei22
                                        .iter()
                                        .next()
                                        .expect("絶対ある")
                                        .execute_pettern_l()
                                };
                                let mut yyaku12 = Vec::new();
                                let mut score12 = None;
                                for k in 1..=j {
                                    for pettern5 in (0..k).combinations(k) {
                                        for pettern6 in (0..5).combinations(k) {
                                            let (temp14, temp34) =
                                                temp13.clone().into_iter().enumerate().fold(
                                                    (Vec::new(), Vec::new()),
                                                    |(mut t1, mut t3), (i1, e1)| {
                                                        if pettern6.contains(&i1) {
                                                            t3.push(e1);
                                                        } else {
                                                            t1.push(e1);
                                                        }
                                                        (t1, t3)
                                                    },
                                                );
                                            let (mut temp14, _) =
                                                temp33.clone().into_iter().enumerate().fold(
                                                    (temp14, temp34),
                                                    |(mut t1, t3), (i3, e3)| {
                                                        if pettern5.contains(&i3) {
                                                            t1.push(e3);
                                                        }
                                                        (t1, t3)
                                                    },
                                                );
                                            temp14.sort();
                                            let hantei1_temp = hantei(
                                                temp14[0], temp14[1], temp14[2], temp14[3],
                                                temp14[4],
                                            );
                                            let score1_temp = if hantei1_temp.is_empty() {
                                                0
                                            } else {
                                                hantei1_temp
                                                    .iter()
                                                    .next()
                                                    .expect("絶対ある")
                                                    .execute_pettern_l()
                                            };
                                            if Some(score1_temp) > score12 {
                                                score12 = Some(score1_temp);
                                                yyaku12 = temp14.clone();
                                            }
                                        }
                                    }
                                }
                                let score12 = score12.expect("絶対Some");
                                match max_yaku2_score {
                                    Some((sc12, sc22))
                                        if score22 as i64 - score12 as i64
                                            > sc22 as i64 - sc12 as i64 =>
                                    {
                                        max_yaku2_score = Some((score12, score22));
                                        max_yaku2 = (temp12.clone(), yyaku12, temp23.clone());
                                    }
                                    None => {
                                        max_yaku2_score = Some((score12, score22));
                                        max_yaku2 = (temp12.clone(), yyaku12, temp23.clone());
                                    }
                                    _ => (),
                                }
                            }
                        }
                    }
                    if min_2_max_score < min_2_score || min_2_score.is_none() {
                        min_2_score = min_2_max_score;
                        min_2 = min_2_max;
                        min_2_swap = swap.clone();
                    }
                    let score2 = score2.expect("絶対Some");
                    match max_score {
                        Some((sc1, sc2))
                            if score1 as i64 - score2 as i64 > sc1 as i64 - sc2 as i64 =>
                        {
                            max_score = Some((score1, score2));
                            max_yaku = (temp12.clone(), yyaku2.clone());
                            max_swap = swap.clone();
                        }
                        None => {
                            max_score = Some((score1, score2));
                            max_yaku = (temp12.clone(), yyaku2.clone());
                            max_swap = swap.clone();
                        }
                        _ => (),
                    }
                    match (min_score, max_yaku2_score) {
                        (Some((sc_min1, sc_min2)), Some((sc_max1, sc_max2)))
                            if (sc_max2 as i64 - sc_max1 as i64)
                                < sc_min2 as i64 - sc_min1 as i64 =>
                        {
                            min_score = max_yaku2_score;
                            min_yaku = max_yaku2;
                            min_swap = swap.clone();
                        }
                        (None, Some(_)) => {
                            min_score = max_yaku2_score;
                            min_yaku = max_yaku2;
                            min_swap = swap.clone();
                        }
                        _ => (),
                    }
                }
            }
        }
        let (max_yaku1, max_yaku2) = max_yaku;
        let max_yaku1 = [
            max_yaku1[0],
            max_yaku1[1],
            max_yaku1[2],
            max_yaku1[3],
            max_yaku1[4],
        ];
        let max_yaku2 = [
            max_yaku2[0],
            max_yaku2[1],
            max_yaku2[2],
            max_yaku2[3],
            max_yaku2[4],
        ];
        let (max_score1, max_score2) = max_score.expect("絶対Some!!");
        let (min_yaku1, min_yaku1_after, min_yaku2) = min_yaku;
        let min_yaku1 = [
            min_yaku1[0],
            min_yaku1[1],
            min_yaku1[2],
            min_yaku1[3],
            min_yaku1[4],
        ];
        let min_yaku1_after = [
            min_yaku1_after[0],
            min_yaku1_after[1],
            min_yaku1_after[2],
            min_yaku1_after[3],
            min_yaku1_after[4],
        ];
        let min_yaku2 = [
            min_yaku2[0],
            min_yaku2[1],
            min_yaku2[2],
            min_yaku2[3],
            min_yaku2[4],
        ];
        let (min_score1, min_score2) = min_score.expect("絶対Some!!");
        let max_1 = [max_1[0], max_1[1], max_1[2], max_1[3], max_1[4]];
        let max_1_score = max_1_score.expect("絶対Some!!");
        let min_2 = [min_2[0], min_2[1], min_2[2], min_2[3], min_2[4]];
        let min_2_score = min_2_score.expect("絶対Some!!");
        let (vs_miracle_max_yaku1, vs_miracle_max_yaku2) = vs_miracle_max_yaku;
        let vs_miracle_max_yaku1 = [
            vs_miracle_max_yaku1[0],
            vs_miracle_max_yaku1[1],
            vs_miracle_max_yaku1[2],
            vs_miracle_max_yaku1[3],
            vs_miracle_max_yaku1[4],
        ];
        let vs_miracle_max_yaku2 = [
            vs_miracle_max_yaku2[0],
            vs_miracle_max_yaku2[1],
            vs_miracle_max_yaku2[2],
            vs_miracle_max_yaku2[3],
            vs_miracle_max_yaku2[4],
        ];
        let miracle_max_score1 = miracle_max_score1.expect("絶対Some!!");
        let miracle_max_score2 = miracle_max_score2.expect("絶対Some!!");
        let (vs_miracle_min_yaku1, vs_miracle_min_yaku2) = vs_miracle_min_yaku;
        let vs_miracle_min_yaku1 = [
            vs_miracle_min_yaku1[0],
            vs_miracle_min_yaku1[1],
            vs_miracle_min_yaku1[2],
            vs_miracle_min_yaku1[3],
            vs_miracle_min_yaku1[4],
        ];
        let vs_miracle_min_yaku2 = [
            vs_miracle_min_yaku2[0],
            vs_miracle_min_yaku2[1],
            vs_miracle_min_yaku2[2],
            vs_miracle_min_yaku2[3],
            vs_miracle_min_yaku2[4],
        ];
        let miracle_min_score1 = miracle_min_score1.expect("絶対Some!!");
        let miracle_min_score2 = miracle_min_score2.expect("絶対Some!!");
        let (vs_miracle_mean_yaku1, vs_miracle_mean_yaku2) = vs_miracle_mean_yaku;
        let vs_miracle_mean_yaku1 = [
            vs_miracle_mean_yaku1[0],
            vs_miracle_mean_yaku1[1],
            vs_miracle_mean_yaku1[2],
            vs_miracle_mean_yaku1[3],
            vs_miracle_mean_yaku1[4],
        ];
        let miracle_mean_score1 = miracle_mean_score1.expect("絶対Some!!");
        let miracle_mean_score2 = miracle_mean_score2.expect("絶対Some!!");
        let miracle_mean_k = miracle_mean_k.expect("絶対Some!!");
        let (vs_miracle_kakuritsu_yaku1, vs_miracle_kakuritsu_yaku2) = vs_miracle_kakuritsu_yaku;
        let vs_miracle_kakuritsu_yaku1 = [
            vs_miracle_kakuritsu_yaku1[0],
            vs_miracle_kakuritsu_yaku1[1],
            vs_miracle_kakuritsu_yaku1[2],
            vs_miracle_kakuritsu_yaku1[3],
            vs_miracle_kakuritsu_yaku1[4],
        ];
        let miracle_kakuritsu_score1 = miracle_kakuritsu_score1.expect("絶対Some!!");
        let miracle_kakuritsu_score2 = miracle_kakuritsu_score2.expect("絶対Some!!");
        let miracle_kakuritsu_k = miracle_kakuritsu_k.expect("絶対Some!!");

        SuhjongCheck {
            max_yaku1,
            max_yaku2,
            max_score1,
            max_score2,
            max_swap,
            min_yaku1,
            min_yaku1_after,
            min_yaku2,
            min_score1,
            min_score2,
            min_swap,
            max_1,
            max_1_score,
            max_1_swap,
            min_2,
            min_2_score,
            min_2_swap,
            vs_miracle_max_yaku1,
            vs_miracle_max_yaku2,
            miracle_max_score1,
            miracle_max_score2,
            miracle_max_swap,
            vs_miracle_min_yaku1,
            vs_miracle_min_yaku2,
            miracle_min_score1,
            miracle_min_score2,
            miracle_min_swap,
            vs_miracle_mean_yaku1,
            vs_miracle_mean_yaku2,
            miracle_mean_score1,
            miracle_mean_score2,
            miracle_mean_k,
            miracle_mean_swap,
            vs_miracle_kakuritsu_yaku1,
            vs_miracle_kakuritsu_yaku2,
            miracle_kakuritsu_score1,
            miracle_kakuritsu_score2,
            miracle_kakuritsu_k,
            miracle_kakuritsu_swap,
        }
    }

    fn miracle(&self) -> SuhjongMiracle {
        let (mut temp1, mut temp2, mut temp3) = if self.is_player1 {
            (
                self.player1.clone(),
                self.player2.clone(),
                self.river2.clone(),
            )
        } else {
            (
                self.player2.clone(),
                self.player1.clone(),
                self.river1.clone(),
            )
        };
        temp1.sort();
        temp2.sort();
        temp3.sort();
        let kouho_len = temp3.len();
        let mut max_yaku = Vec::new();
        let mut max_score = None;
        let mut max_swap = (Vec::new(), Vec::new());
        let mut min_yaku = Vec::new();
        let mut min_score = None;
        let mut min_swap = (Vec::new(), Vec::new());
        let mut mean_yaku = Vec::new();
        let mut mean_score = None;
        let mut mean_k = None;
        let mut mean_swap = (Vec::new(), Vec::new());
        let mut kakuritsu_yaku = Vec::new();
        let mut kakuritsu_k = None;
        let mut kakuritsu_score = None;
        let mut kakuritsu_swap = (Vec::new(), Vec::new());
        let mut sum_yaku = Vec::new();
        let mut sum_k = None;
        let mut sum_score = None;
        let mut sum_swap = (Vec::new(), Vec::new());
        let mut max_digit_yaku = Vec::new();
        let mut max_digit_k_win = None;
        let mut max_digit_k_max = None;
        let mut max_digit_mean_score = None;
        let mut max_digit_swap = (Vec::new(), Vec::new());
        let hantei2 = hantei(temp2[0], temp2[1], temp2[2], temp2[3], temp2[4]);
        let score2 = if hantei2.is_empty() {
            0
        } else {
            hantei2.iter().next().expect("絶対ある").execute_pettern_l()
        };
        let is_digit_win =
            temp1[4] * 10000 + temp1[3] * 1000 + temp1[2] * 100 + temp1[1] * 10 + temp1[0]
                > temp2[4] * 10000 + temp2[3] * 1000 + temp2[2] * 100 + temp2[1] * 10 + temp2[0];
        for i in 1..=5 {
            for pettern2 in (0..5).combinations(i) {
                let temp32 =
                    temp1
                        .clone()
                        .iter()
                        .enumerate()
                        .fold(Vec::new(), |mut t3, (i1, &e1)| {
                            if pettern2.contains(&i1) {
                                t3.push(e1);
                            }
                            t3
                        });
                let mut is_score2_greater_than_80 = false;
                for k in 1..=i {
                    for pettern3 in (0..i).combinations(k) {
                        for pettern4 in (0..5).combinations(k) {
                            let (temp23, temp33) = temp2.clone().iter().enumerate().fold(
                                (Vec::new(), Vec::new()),
                                |(mut t2, mut t3), (i2, &e2)| {
                                    if pettern4.contains(&i2) {
                                        t3.push(e2);
                                    } else {
                                        t2.push(e2);
                                    }
                                    (t2, t3)
                                },
                            );
                            let (mut temp23, _) = temp32.clone().into_iter().enumerate().fold(
                                (temp23, temp33),
                                |(mut t2, t3), (i3, e3)| {
                                    if pettern3.contains(&i3) {
                                        t2.push(e3);
                                    }
                                    (t2, t3)
                                },
                            );
                            temp23.sort();
                            let hantei2 =
                                hantei(temp23[0], temp23[1], temp23[2], temp23[3], temp23[4]);
                            let score2 = if hantei2.is_empty() {
                                0
                            } else {
                                hantei2.iter().next().expect("絶対ある").execute_pettern_l()
                            };
                            if score2 > 80 {
                                is_score2_greater_than_80 = true;
                            }
                        }
                    }
                }
                for j in 0..=std::cmp::min(i, kouho_len) {
                    for pettern1 in (0..kouho_len).combinations(j) {
                        let (temp12, temp32, sute) = temp1.clone().iter().enumerate().fold(
                            (Vec::new(), Vec::new(), Vec::new()),
                            |(mut t1, mut t3, mut sute), (i1, &e1)| {
                                if pettern2.contains(&i1) {
                                    t3.push(e1);
                                    sute.push(e1);
                                } else {
                                    t1.push(e1);
                                }
                                (t1, t3, sute)
                            },
                        );
                        let (temp12, _, hiro) = temp3.clone().into_iter().enumerate().fold(
                            (temp12, temp32, Vec::new()),
                            |(mut t1, t3, mut hiro), (i3, e3)| {
                                if pettern1.contains(&i3) {
                                    t1.push(e3);
                                    hiro.push(e3);
                                }
                                (t1, t3, hiro)
                            },
                        );
                        let swap = (sute, hiro);
                        let mut max_yaku_temp = Vec::new();
                        let mut max_score_temp = None;
                        let mut min_yaku_temp = Vec::new();
                        let mut min_score_temp = None;
                        let mean_yaku_temp = temp12.clone();
                        let mean_score_temp;
                        let kakuritsu_yaku_temp = temp12.clone();
                        let kakuritsu_temp;
                        let sum_yaku_temp = temp12.clone();
                        let mut sum_score_temp = 0;
                        let mut scores = Vec::new();
                        let max_digit_yaku_temp = temp12.clone();
                        let mut digit_win_numbers = 0;
                        for pettern3 in (0..=9).combinations_with_replacement(i - j).filter(|p| {
                            (0..=9).into_iter().all(|n| {
                                p.iter().filter(|&&m| n == m).count() <= self.yama[n as usize]
                            })
                        }) {
                            let mut temp12 =
                                pettern3.into_iter().fold(temp12.clone(), |mut t1, ez| {
                                    t1.push(ez);
                                    t1
                                });
                            temp12.sort();
                            let hantei1 =
                                hantei(temp12[0], temp12[1], temp12[2], temp12[3], temp12[4]);
                            let sum_ex: u32 = temp12.iter().sum();
                            sum_score_temp += sum_ex;
                            let score1 = if hantei1.is_empty() {
                                0
                            } else {
                                hantei1.iter().next().expect("絶対ある").execute_pettern_l()
                            };
                            if Some(score1) > max_score_temp {
                                max_score_temp = Some(score1);
                                max_yaku_temp = temp12.clone();
                            }
                            if Some(score1) < min_score_temp || min_score_temp.is_none() {
                                min_score_temp = Some(score1);
                                min_yaku_temp = temp12.clone();
                            }
                            scores.push(score1);
                            if !is_score2_greater_than_80 {
                                if temp12[4] * 10000
                                    + temp12[3] * 1000
                                    + temp12[2] * 100
                                    + temp12[1] * 10
                                    + temp12[0]
                                    > temp2[4] * 10000
                                        + temp2[3] * 1000
                                        + temp2[2] * 100
                                        + temp2[1] * 10
                                        + temp2[0]
                                {
                                    digit_win_numbers += 1;
                                }
                            }
                        }

                        let (sum, filter_sum, count) = scores.into_iter().fold(
                            (0, 0, 0),
                            |(sum, filter_sum, count), value| {
                                if value >= score2 && !is_score2_greater_than_80 {
                                    (sum + value, filter_sum + 1, count + 1)
                                } else {
                                    (sum + value, filter_sum, count + 1)
                                }
                            },
                        );
                        mean_score_temp = sum as f64 / count as f64;
                        kakuritsu_temp = filter_sum as f64 / count as f64;
                        let mean_sum = sum_score_temp as f64 / count as f64;
                        let digit_win_k = digit_win_numbers as f64 / count as f64;
                        if max_score_temp > max_score {
                            max_score = max_score_temp;
                            max_yaku = max_yaku_temp;
                            max_swap = swap.clone();
                        }
                        if min_score_temp > min_score {
                            min_score = min_score_temp;
                            min_yaku = min_yaku_temp;
                            min_swap = swap.clone();
                        }
                        if Some(mean_score_temp) > mean_score {
                            mean_score = Some(mean_score_temp);
                            mean_yaku = mean_yaku_temp;
                            mean_k = Some(kakuritsu_temp);
                            mean_swap = swap.clone();
                        }
                        if Some(kakuritsu_temp) > kakuritsu_k {
                            kakuritsu_k = Some(kakuritsu_temp);
                            kakuritsu_yaku = kakuritsu_yaku_temp;
                            kakuritsu_score = Some(mean_score_temp);
                            kakuritsu_swap = swap.clone();
                        }
                        if Some(mean_sum) > sum_score {
                            sum_score = Some(mean_sum);
                            sum_yaku = sum_yaku_temp;
                            sum_k = Some(kakuritsu_temp);
                            sum_swap = swap.clone();
                        }
                        if Some(digit_win_k) > max_digit_k_max {
                            max_digit_k_max = Some(digit_win_k);
                            max_digit_yaku = max_digit_yaku_temp;
                            max_digit_k_win = Some(kakuritsu_temp);
                            max_digit_mean_score = Some(mean_score_temp);
                            max_digit_swap = swap.clone();
                        }
                    }
                }
            }
        }
        let max_score = max_score.expect("絶対Some!!");
        let min_score = min_score.expect("絶対Some!!");
        let mean_score = mean_score.expect("絶対Some!!");
        let mean_k = mean_k.expect("絶対Some!!");
        let kakuritsu_score = kakuritsu_score.expect("絶対Some!!");
        let kakuritsu_k = kakuritsu_k.expect("絶対Some!!");
        let sum_score = sum_score.expect("絶対Some!!");
        let sum_k = sum_k.expect("絶対Some!!");
        let max_digit_k_win = max_digit_k_win.expect("絶対Some!!");
        let max_digit_k_max = max_digit_k_max.expect("絶対Some!!");
        let max_digit_mean_score = max_digit_mean_score.expect("絶対Some!!");
        let max_yaku = [
            max_yaku[0],
            max_yaku[1],
            max_yaku[2],
            max_yaku[3],
            max_yaku[4],
        ];
        let min_yaku = [
            min_yaku[0],
            min_yaku[1],
            min_yaku[2],
            min_yaku[3],
            min_yaku[4],
        ];

        SuhjongMiracle {
            aite_score: score2,
            max_yaku,
            max_score,
            max_swap,
            min_yaku,
            min_score,
            min_swap,
            mean_yaku,
            mean_k,
            mean_score,
            mean_swap,
            kakuritsu_yaku,
            kakuritsu_k,
            kakuritsu_score,
            kakuritsu_swap,
            sum_yaku,
            sum_score,
            sum_k,
            sum_swap,
            max_digit_yaku,
            max_digit_k_win,
            max_digit_k_max,
            max_digit_mean_score,
            max_digit_swap,
            is_digit_win,
        }
    }

    fn kansou(self, mut sute: Vec<u32>, mut hiro: Vec<u32>, mut draw: Vec<u32>) -> Option<Self> {
        let (mut temp1, mut temp3): (Vec<_>, Vec<_>) = if self.is_player1 {
            (
                self.player1.iter().map(|&n| n).collect(),
                self.river2.clone(),
            )
        } else {
            (
                self.player2.iter().map(|&n| n).collect(),
                self.river1.clone(),
            )
        };
        if (0..=9).into_iter().any(|n| {
            temp1.iter().filter(|&&m| n == m).count() < sute.iter().filter(|&&m| n == m).count()
                || temp3.iter().filter(|&&m| n == m).count()
                    < hiro.iter().filter(|&&m| n == m).count()
        }) {
            return None;
        }

        let (len1, len2, len3) = (sute.len(), hiro.len(), draw.len());

        if len1 == 0 || len1 > 5 || len2 > 5 || len3 > 5 || len1 != len2 + len3 {
            return None;
        }

        let mut yama = self
            .yama
            .iter()
            .enumerate()
            .map(|(index, &num)| vec![index as u32; num])
            .fold(Vec::new(), |mut r, mut v| {
                r.append(&mut v);
                r
            });

        if yama.len() < len3 {
            return None;
        }

        let mut temp32 = Vec::new();

        for _ in 0..len1 {
            let element = sute.remove(0);
            let index = temp1
                .iter()
                .enumerate()
                .find(|(_, &e)| e == element)
                .map(|(i, _)| i)?;
            temp32.push(temp1.remove(index));
        }

        for _ in 0..len2 {
            let element = hiro.remove(0);
            let index = temp3
                .iter()
                .enumerate()
                .find(|(_, &e)| e == element)
                .map(|(i, _)| i)?;
            temp1.push(temp3.remove(index));
        }

        for _ in 0..len3 {
            let element = draw.remove(0);
            let index = yama
                .iter()
                .enumerate()
                .find(|(_, &e)| e == element)
                .map(|(i, _)| i)?;
            temp1.push(yama.remove(index));
        }

        temp1.sort();
        temp32.sort();

        let yama: Vec<_> = (0..=9)
            .into_iter()
            .map(|n| yama.iter().filter(|&&e| e == n).count())
            .collect();
        let yama = [
            yama[0], yama[1], yama[2], yama[3], yama[4], yama[5], yama[6], yama[7], yama[8],
            yama[9],
        ];
        let temp1 = [temp1[0], temp1[1], temp1[2], temp1[3], temp1[4]];

        let mut ban = self;
        if ban.is_player1 {
            ban.player1 = temp1;
            ban.river1 = temp32;
            ban.river2.clear();
        } else {
            ban.player2 = temp1;
            ban.river2 = temp32;
            ban.river1.clear();
        }
        if ban.is_shoubed {
            ban.is_finised = true;
        }
        ban.yama = yama;
        ban.turn += 1;
        ban.is_player1 = !ban.is_player1;

        Some(ban)
    }

    fn shoub(self, sute: Vec<u32>, hiro: Vec<u32>) -> Option<Self> {
        if self.turn < 2 || self.is_shoubed || sute.len() != hiro.len() {
            return None;
        }
        let mut ban = self.next(sute, hiro)?;
        ban.is_shoubed = true;
        Some(ban)
    }

    fn next(self, mut sute: Vec<u32>, mut hiro: Vec<u32>) -> Option<Self> {
        let (mut temp1, mut temp3): (Vec<_>, Vec<_>) = if self.is_player1 {
            (
                self.player1.iter().map(|&n| n).collect(),
                self.river2.clone(),
            )
        } else {
            (
                self.player2.iter().map(|&n| n).collect(),
                self.river1.clone(),
            )
        };
        if (0..=9).into_iter().any(|n| {
            temp1.iter().filter(|&&m| n == m).count() < sute.iter().filter(|&&m| n == m).count()
                || temp3.iter().filter(|&&m| n == m).count()
                    < hiro.iter().filter(|&&m| n == m).count()
        }) {
            return None;
        }

        let (len1, len2) = (sute.len(), hiro.len());

        if len1 == 0 || len1 > 5 || len2 > 5 || len2 > len1 {
            return None;
        }

        let len3 = len1 - len2;

        let mut yama = self
            .yama
            .iter()
            .enumerate()
            .map(|(index, &num)| vec![index as u32; num])
            .fold(Vec::new(), |mut r, mut v| {
                r.append(&mut v);
                r
            });
        yama.shuffle(&mut rand::thread_rng());

        if yama.len() < len3 {
            return None;
        }

        let mut temp32 = Vec::new();

        for _ in 0..len1 {
            let element = sute.remove(0);
            let index = temp1
                .iter()
                .enumerate()
                .find(|(_, &e)| e == element)
                .map(|(i, _)| i)?;
            temp32.push(temp1.remove(index));
        }

        for _ in 0..len2 {
            let element = hiro.remove(0);
            let index = temp3
                .iter()
                .enumerate()
                .find(|(_, &e)| e == element)
                .map(|(i, _)| i)?;
            temp1.push(temp3.remove(index));
        }

        for _ in 0..len3 {
            temp1.push(yama.pop().expect("絶対Some!!"));
        }

        temp1.sort();
        temp32.sort();

        let yama: Vec<_> = (0..=9)
            .into_iter()
            .map(|n| yama.iter().filter(|&&e| e == n).count())
            .collect();
        let yama = [
            yama[0], yama[1], yama[2], yama[3], yama[4], yama[5], yama[6], yama[7], yama[8],
            yama[9],
        ];
        let temp1 = [temp1[0], temp1[1], temp1[2], temp1[3], temp1[4]];

        let mut ban = self;
        if ban.is_player1 {
            ban.player1 = temp1;
            ban.river1 = temp32;
            ban.river2.clear();
        } else {
            ban.player2 = temp1;
            ban.river2 = temp32;
            ban.river1.clear();
        }
        if ban.is_shoubed {
            ban.is_finised = true;
        }
        ban.yama = yama;
        ban.turn += 1;
        ban.is_player1 = !ban.is_player1;

        Some(ban)
    }

    fn bot_play(self) -> SuhjongMen {
        if self.turn == 0 {
            let miracle = self.miracle();
            if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else if self.is_shoubed {
            let miracle = self.miracle();
            if miracle.kakuritsu_score as f64 + miracle.kakuritsu_k * 100.0
                > miracle.mean_score + miracle.mean_k * 100.0
            {
                let (sute, hiro) = miracle.kakuritsu_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.mean_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else if self.turn == 1 {
            let check = self.check();
            let miracle = self.miracle();
            if check.min_score1 > check.min_score2 - 10 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.max_digit_k_max < 0.01 || miracle.sum_k < 0.01 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else {
            let miracle = self.miracle();
            let check = self.check();
            if check.max_score1 >= 90 {
                let (sute, hiro) = check.max_swap;
                self.shoub(sute, hiro).expect("Some！！")
            } else if check.miracle_kakuritsu_k < 0.1 {
                let (sute, hiro) = check.miracle_kakuritsu_swap;
                self.shoub(sute, hiro).expect("Some！！")
            } else if check.min_score1 > check.min_score2 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.max_digit_k_max < 0.01 || miracle.sum_k < 0.01 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        }
    }

    fn bot2_play(self) -> SuhjongMen {
        if self.turn == 0 {
            let miracle = self.miracle();
            if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else if self.is_shoubed {
            let miracle = self.miracle();
            if miracle.kakuritsu_score as f64 + miracle.kakuritsu_k * 100.0
                > miracle.mean_score + miracle.mean_k * 100.0
            {
                let (sute, hiro) = miracle.kakuritsu_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.mean_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else if self.turn == 1 {
            let check = self.check();
            let miracle = self.miracle();
            if check.min_score1 > check.min_score2 - 10 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.max_digit_k_max < 0.01 || miracle.sum_k < 0.01 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else {
            let miracle = self.miracle();
            let check = self.check();
            if check.miracle_kakuritsu_k < 0.2 {
                let (sute, hiro) = check.miracle_kakuritsu_swap;
                self.shoub(sute, hiro).expect("Some！！")
            } else if check.max_score1 > check.max_score2 {
                let (sute, hiro) =check.max_swap;
                self.shoub(sute, hiro).expect("Some！！")
            } else if check.min_score1 > check.min_score2 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.max_digit_k_max < 0.01 || miracle.sum_k < 0.01 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        }
    }

    fn bot3_play(self) -> SuhjongMen {
        if self.turn == 0 {
            let miracle = self.miracle();
            if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else if self.is_shoubed {
            let miracle = self.miracle();
            if miracle.min_score as f64 > miracle.kakuritsu_k * miracle.kakuritsu_score {
                let (sute, hiro) = miracle.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.kakuritsu_score as f64 + miracle.kakuritsu_k * 100.0
                > miracle.mean_score + miracle.mean_k * 100.0
            {
                let (sute, hiro) = miracle.kakuritsu_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.mean_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else if self.turn == 1 {
            let miracle = self.miracle();
            if miracle.max_digit_k_max < 0.01 || miracle.sum_k < 0.01 {
                let check = self.check();
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else {
            let miracle = self.miracle();
            let check = self.check();
            if check.max_score1 >= 90 {
                let (sute, hiro) = check.max_swap;
                self.shoub(sute, hiro).expect("Some！！")
            } else if check.miracle_kakuritsu_k < 0.1 {
                let (sute, hiro) = check.miracle_kakuritsu_swap;
                self.shoub(sute, hiro).expect("Some！！")
            } else if miracle.max_digit_k_max < 0.01 || miracle.sum_k < 0.01 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        }
    }

    fn bot4_play(self) -> SuhjongMen {
        if self.turn == 0 {
            let miracle = self.miracle();
            if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else if self.is_shoubed {
            let miracle = self.miracle();
            if miracle.min_score as f64 > miracle.kakuritsu_k * miracle.kakuritsu_score {
                let (sute, hiro) = miracle.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.kakuritsu_score as f64 + miracle.kakuritsu_k * 100.0
                > miracle.mean_score + miracle.mean_k * 100.0
            {
                let (sute, hiro) = miracle.kakuritsu_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.mean_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else if self.turn == 1 {
            let miracle = self.miracle();
            if miracle.max_digit_k_max < 0.01 || miracle.sum_k < 0.01 {
                let check = self.check();
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        } else {
            let miracle = self.miracle();
            let check = self.check();
            if check.miracle_kakuritsu_k < 0.2 {
                let (sute, hiro) = check.miracle_kakuritsu_swap;
                self.shoub(sute, hiro).expect("Some！！")
            } else if check.max_score1 > check.max_score2 {
                let (sute, hiro) =check.max_swap;
                self.shoub(sute, hiro).expect("Some！！")
            } else if miracle.max_digit_k_max < 0.01 || miracle.sum_k < 0.01 {
                let (sute, hiro) = check.min_swap;
                self.next(sute, hiro).expect("Some！！")
            } else if miracle.is_digit_win {
                let (sute, hiro) = miracle.sum_swap;
                self.next(sute, hiro).expect("Some！！")
            } else {
                let (sute, hiro) = miracle.max_digit_swap;
                self.next(sute, hiro).expect("Some！！")
            }
        }
    }
}

#[command]
#[description = "数雀フォーマットチェック"]
async fn format_play_suhjong(ctx: &Context, msg: &Message) -> CommandResult {
    let ref_content = match msg.message_reference.clone() {
        Some(reference) => {
            let channel_id = reference.channel_id;
            let message_id = match reference.message_id {
                Some(m_id) => m_id,
                None => {
                    msg.reply_ping(&ctx.http, "エラー").await?;
                    return Ok(());
                }
            };
            let ref_msg = channel_id.message(&ctx.http, message_id).await?;
            ref_msg.content
        }
        None => {
            msg.reply_ping(&ctx.http, "エラー").await?;
            return Ok(());
        }
    };
    let ban = raw_format_play_suhjong(ref_content);

    let m = match ban {
        Some(b) => b.to_string(),
        None => {
            msg.reply_ping(&ctx.http, "フォーマットエラー").await?;
            return Ok(());
        }
    };

    msg.reply(&ctx.http, m).await?;

    Ok(())
}

fn raw_format_play_suhjong(s: String) -> Option<SuhjongMen> {
    let re = Regex::new(
        r"数雀プレイ中 ターン経過:(\d+) 勝負:([-〇])
player1 番:([-〇])
```
(\d{5})
````河 (\d{0,5})`
山 \|\|0:(\d) 1:(\d) 2:(\d) 3:(\d) 4:(\d) 5:(\d) 6:(\d) 7:(\d) 8:(\d) 9:(\d)\|\|
`河 (\d{0,5})`
```
(\d{5})
```player2 番:([-〇])",
    )
    .unwrap();

    if !re.is_match(&s) {
        return None;
    }

    let caps = re.captures(&s).unwrap();
    let turn = caps[1].parse().ok()?;
    let is_shoubed = caps[2] == *"〇";
    let is_player1 = caps[3] == *"〇";
    let player1: Vec<_> = caps[4].chars().map(|c| c as u32 - '0' as u32).collect();
    let player1 = [player1[0], player1[1], player1[2], player1[3], player1[4]];
    let river1: Vec<_> = caps[5].chars().map(|c| c as u32 - '0' as u32).collect();
    let yama: Vec<_> = (6..=15)
        .into_iter()
        .map(|i| {
            caps[i]
                .chars()
                .next()
                .map(|c| c as usize - '0' as usize)
                .unwrap()
        })
        .collect();
    if yama.iter().any(|n| n > &5) {
        return None;
    }
    let yama = [
        yama[0], yama[1], yama[2], yama[3], yama[4], yama[5], yama[6], yama[7], yama[8], yama[9],
    ];
    let river2: Vec<_> = caps[16].chars().map(|c| c as u32 - '0' as u32).collect();
    let player2: Vec<_> = caps[17].chars().map(|c| c as u32 - '0' as u32).collect();
    let player2 = [player2[0], player2[1], player2[2], player2[3], player2[4]];
    let is_player2 = caps[18] == *"〇";
    if turn <= 2 && is_shoubed {
        return None;
    }
    if turn % 2 == 0 && (!is_player1 || is_player2) || turn % 2 != 0 && (is_player1 || !is_player2)
    {
        return None;
    }
    if turn == 0 && (river1.len() > 0 || river2.len() > 0)
        || turn > 0
            && (is_player1 && (river1.len() > 0 || river2.len() == 0)
                || is_player2 && (river1.len() == 0 || river2.len() > 0))
    {
        return None;
    }

    Some(SuhjongMen {
        turn,
        is_shoubed,
        is_player1,
        player1,
        player2,
        river1,
        river2,
        yama,
        is_finised: false,
    })
}

struct SuhjongMiracle {
    aite_score: u32,
    max_yaku: [u32; 5],
    max_score: u32,
    max_swap: (Vec<u32>, Vec<u32>),
    min_yaku: [u32; 5],
    min_score: u32,
    min_swap: (Vec<u32>, Vec<u32>),
    mean_yaku: Vec<u32>,
    mean_score: f64,
    mean_k: f64,
    mean_swap: (Vec<u32>, Vec<u32>),
    kakuritsu_yaku: Vec<u32>,
    kakuritsu_score: f64,
    kakuritsu_k: f64,
    kakuritsu_swap: (Vec<u32>, Vec<u32>),
    sum_yaku: Vec<u32>,
    sum_score: f64,
    sum_k: f64,
    sum_swap: (Vec<u32>, Vec<u32>),
    max_digit_yaku: Vec<u32>,
    max_digit_k_win: f64,
    max_digit_k_max: f64,
    max_digit_mean_score: f64,
    max_digit_swap: (Vec<u32>, Vec<u32>),
    is_digit_win: bool,
}

impl SuhjongMiracle {
    fn to_string(self) -> String {
        format!("aite: 得点({})\nmax: 得点({})\n```\n{:?}\n{:?}\n```\nmin: 得点({})\n```\n{:?}\n{:?}\n```\nmean: 得点({:.2}) {:.2}%\n```\n{:?}\n{:?}\n```\nkakuritsu: 得点({:.2}) {:.2}%\n```\n{:?}\n{:?}\n```\nsum: 得点({:.2}) {:.2}%\n```\n{:?}\n{:?}\n```\nmax_gigit: {:.2}% 得点({:.2}) {:.2}%\n```\n{:?}\n{:?}\n```", self.aite_score, self.max_score, self.max_swap, self.max_yaku, self.min_score, self.min_swap, self.min_yaku, self.mean_score, self.mean_k * 100.0, self.mean_swap, self.mean_yaku, self.kakuritsu_score, self.kakuritsu_k * 100.0, self.kakuritsu_swap, self.kakuritsu_yaku, self.sum_score, self.sum_k * 100.0, self.sum_swap, self.sum_yaku, self.max_digit_k_max * 100.0, self.max_digit_mean_score, self.max_digit_k_win * 100.0, self.max_digit_swap, self.max_digit_yaku)
    }
}

struct SuhjongCheck {
    max_yaku1: [u32; 5],
    max_yaku2: [u32; 5],
    max_score1: u32,
    max_score2: u32,
    max_swap: (Vec<u32>, Vec<u32>),
    min_yaku1: [u32; 5],
    min_yaku1_after: [u32; 5],
    min_yaku2: [u32; 5],
    min_score1: u32,
    min_score2: u32,
    min_swap: (Vec<u32>, Vec<u32>),
    max_1: [u32; 5],
    max_1_score: u32,
    max_1_swap: (Vec<u32>, Vec<u32>),
    min_2: [u32; 5],
    min_2_score: u32,
    min_2_swap: (Vec<u32>, Vec<u32>),
    vs_miracle_max_yaku1: [u32; 5],
    vs_miracle_max_yaku2: [u32; 5],
    miracle_max_score1: u32,
    miracle_max_score2: u32,
    miracle_max_swap: (Vec<u32>, Vec<u32>),
    vs_miracle_min_yaku1: [u32; 5],
    vs_miracle_min_yaku2: [u32; 5],
    miracle_min_score1: u32,
    miracle_min_score2: u32,
    miracle_min_swap: (Vec<u32>, Vec<u32>),
    vs_miracle_mean_yaku1: [u32; 5],
    vs_miracle_mean_yaku2: Vec<u32>,
    miracle_mean_score1: u32,
    miracle_mean_score2: f64,
    miracle_mean_k: f64,
    miracle_mean_swap: (Vec<u32>, Vec<u32>),
    vs_miracle_kakuritsu_yaku1: [u32; 5],
    vs_miracle_kakuritsu_yaku2: Vec<u32>,
    miracle_kakuritsu_score1: u32,
    miracle_kakuritsu_score2: f64,
    miracle_kakuritsu_k: f64,
    miracle_kakuritsu_swap: (Vec<u32>, Vec<u32>),
}

impl SuhjongCheck {
    fn to_tuple_string(self) -> (String, String) {
        (
            format!("max(得点{}) {}\n```\n{:?}\n{:?}\n```\n(得点{}) {}\n```\n{:?}\n```\n\nmin(得点{}) {}\n```\n{:?}\n{:?}\n{:?}\n```\n(得点{}) {}\n```\n{:?}\n```\nmax_1(得点{})\n```\n{:?}\n{:?}\n```\n\nmin_2(得点{})\n```\n{:?}\n{:?}\n```", self.max_score1, if self.max_score1 > self.max_score2 {"WIN"} else if self.max_score1 < self.max_score2 {"LOSE"} else {"DRAW"}, self.max_swap, self.max_yaku1, self.max_score2, if self.max_score2 > self.max_score1 {"WIN"} else if self.max_score2 < self.max_score1 {"LOSE"} else {"DRAW"}, self.max_yaku2, self.min_score1, if self.min_score1 > self.min_score2 {"WIN"} else if self.min_score1 < self.min_score2 {"LOSE"} else {"DRAW"}, self.min_swap, self.min_yaku1, self.min_yaku1_after, self.min_score2, if self.min_score2 > self.min_score1 {"WIN"} else if self.min_score2 < self.min_score1 {"LOSE"} else {"DRAW"}, self.min_yaku2, self.max_1_score, self.max_1_swap, self.max_1, self.min_2_score, self.min_2_swap, self.min_2),
            format!("max:\n(得点{}) {}\n```\n{:?}\n{:?}\n```\n(得点{}) {}\n```\n{:?}\n```\n\nmin:\n(得点{}) {}\n```\n{:?}\n{:?}\n```\n(得点{}) {}\n```\n{:?}\n```\n\nmean:\n(得点{}) {}\n```\n{:?}\n{:?}\n```\n(得点{:.2}){:.2}% {}\n```\n{:?}\n```\n\nkakuritsu:\n(得点{}) {}\n```\n{:?}\n{:?}\n```\n(合計{:.2}){:.2}% {}\n```\n{:?}\n```", self.miracle_max_score1, if self.miracle_max_score1 > self.miracle_max_score2 {"WIN"} else if self.miracle_max_score1 < self.miracle_max_score2 {"LOSE"} else {"DRAW"}, self.miracle_max_swap, self.vs_miracle_max_yaku1, self.miracle_max_score2, if self.miracle_max_score2 > self.miracle_max_score1 {"WIN"} else if self.miracle_max_score2 < self.miracle_max_score1 {"LOSE"} else {"DRAW"}, self.vs_miracle_max_yaku2, self.miracle_min_score1, if self.miracle_min_score1 > self.miracle_min_score2 {"WIN"} else if self.miracle_min_score1 < self.miracle_min_score2 {"LOSE"} else {"DRAW"}, self.miracle_min_swap, self.vs_miracle_min_yaku1, self.miracle_min_score2, if self.miracle_min_score2 > self.miracle_min_score1 {"WIN"} else if self.miracle_min_score2 < self.miracle_min_score1 {"LOSE"} else {"DRAW"}, self.vs_miracle_min_yaku2, self.miracle_mean_score1, if self.miracle_mean_score1 as f64 > self.miracle_mean_score2 {"WIN"} else if (self.miracle_mean_score1 as f64) < self.miracle_mean_score2 {"LOSE"} else {"DRAW"}, self.miracle_mean_swap, self.vs_miracle_mean_yaku1, self.miracle_mean_score2, self.miracle_mean_k * 100.0, if self.miracle_mean_score2 > self.miracle_mean_score1 as f64 {"WIN"} else if self.miracle_mean_score2 < self.miracle_mean_score1 as f64 {"LOSE"} else {"DRAW"}, self.vs_miracle_mean_yaku2, self.miracle_kakuritsu_score1, if self.miracle_kakuritsu_score1 as f64 > self.miracle_kakuritsu_score2 {"WIN"} else if (self.miracle_kakuritsu_score1 as f64) < self.miracle_kakuritsu_score2 {"LOSE"} else {"DRAW"}, self.miracle_kakuritsu_swap, self.vs_miracle_kakuritsu_yaku1, self.miracle_kakuritsu_score2, self.miracle_kakuritsu_k * 100.0, if self.miracle_kakuritsu_score2 > self.miracle_kakuritsu_score1 as f64 {"WIN"} else if self.miracle_kakuritsu_score2 < self.miracle_kakuritsu_score1 as f64 {"LOSE"} else {"DRAW"}, self.vs_miracle_kakuritsu_yaku2),
        )
    }
}
