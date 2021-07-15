use std::{collections::HashSet, fs::File, io::BufReader, usize};
use itertools::Itertools;
use std::cmp;


use serenity::async_trait;
use serenity::builder::CreateEmbedAuthor;
use serenity::framework::standard::{
    help_commands,
    macros::{command, group, help},
    Args, CommandError, CommandGroup, CommandResult, HelpOptions,
};
use serenity::framework::StandardFramework;
use serenity::model::{
    channel::GuildChannel, channel::Message, event::TypingStartEvent, gateway::Ready,
    id::ChannelId, id::GuildId, id::UserId,
};
use serenity::prelude::{Client, Context, EventHandler, Mentionable};

use serde::{Deserialize, Serialize};
use serde_json::Result;

use rand::seq::SliceRandom;
use rand::Rng;

use std::time::Duration;

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    async fn channel_create(&self, ctx: Context, channel: &GuildChannel) {
        match channel.guild_id {
            GuildId(739882359649992714u64) => {
                if let Err(why) = channel
                    .say(&ctx.http, format!("{}に一番乗り！", channel))
                    .await
                {
                    println!("Error sending message: {:?}", why);
                }
            }
            _ => (),
        }
    }

    async fn typing_start(&self, ctx: Context, tse: TypingStartEvent) {
        let channel_id = tse.channel_id;
        let author = match tse.user_id.to_user(&ctx.http).await {
            Ok(user) => user,
            Err(why) => {
                println!("Error sending message: {:?}", why);
                return ();
            }
        };
        match channel_id {
            ChannelId(804573278937153596u64) => {
                if let Err(why) = channel_id
                    .say(&ctx.http, format!("{}荒らすな", author))
                    .await
                {
                    println!("Error sending message: {:?}", why);
                }
            }
            _ => (),
        }
    }

    async fn message(&self, ctx: Context, msg: Message) {}
    async fn ready(&self, _: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);
    }
}

#[help] // Helpコマンド
#[individual_command_tip = "これはヘルプコマンド"] // Helpコマンドの説明
#[strikethrough_commands_tip_in_guild = ""] // 使用できないコマンドについての説明を削除
async fn my_help(
    ctx: &Context,
    msg: &Message,
    args: Args,
    help_options: &'static HelpOptions,
    groups: &[&'static CommandGroup],
    owners: HashSet<UserId>,
) -> CommandResult {
    // _ は使用しない返り値を捨てることを明示している
    let _ = help_commands::with_embeds(ctx, msg, args, help_options, groups, owners).await;
    // 空のタプルをreturn（仕様）
    // Rustでは`;`なしの行は値としてreturnすることを表す
    // return Ok(()); と同義
    Ok(())
}

#[group]
#[description("テスト")]
#[summary("テスト")]
#[commands(
    echo,
    echo_ping,
    echo_mention,
    debug,
    first,
    second,
    third,
    parse,
    parse_u64,
    test_embed,
    count20,
)]
struct Test;

#[group]
#[description("ゲーム")]
#[summary("ゲーム")]
#[commands(nkodice)]
struct Game;

#[group]
#[description("パズル")]
#[summary("ソルバー")]
#[commands(solve_hutougou)]
struct Puzzle;

#[command]
#[description = "そのまま返す"]
async fn echo(ctx: &Context, msg: &Message) -> CommandResult {
    let answer = msg.content.chars().skip(7).take(2000).collect::<String>();
    msg.reply(&ctx.http, answer).await?;
    Ok(())
}

#[command]
#[description = "そのまま返す"]
async fn echo_ping(ctx: &Context, msg: &Message) -> CommandResult {
    let answer = msg.content.chars().skip(12).take(2000).collect::<String>();
    msg.reply_ping(&ctx.http, answer).await?;
    Ok(())
}

#[command]
#[description = "そのまま返す"]
async fn echo_mention(ctx: &Context, msg: &Message) -> CommandResult {
    let answer = msg.content.chars().skip(15).take(2000).collect::<String>();
    msg.reply_mention(&ctx.http, answer).await?;
    Ok(())
}

#[command]
#[description = "メッセージの中身を確認する(デバグ)"]
async fn debug(ctx: &Context, msg: &Message) -> CommandResult {
    msg.reply_ping(&ctx.http, format!("{:?}", msg)).await?;
    Ok(())
}

#[command]
#[description = "最初の要素を返す"]
async fn first(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    match args.single::<String>() {
        Ok(x) => msg.reply_ping(&ctx.http, x).await?,
        Err(_) => {
            msg.reply_ping(&ctx.http, "one or more arguments are needed.".to_string())
                .await?
        }
    };
    Ok(())
}

#[command]
#[description = "二番目の要素を返す"]
async fn second(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let _ = args.single::<String>();
    match args.single::<String>() {
        Ok(x) => msg.reply_ping(&ctx.http, x).await?,
        Err(_) => {
            msg.reply_ping(&ctx.http, "two or more arguments are needed.".to_string())
                .await?
        }
    };
    Ok(())
}

#[command]
#[description = "三番目の要素を返す"]
async fn third(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let _ = args.single::<String>();
    let _ = args.single::<String>();
    match args.single::<String>() {
        Ok(x) => msg.reply_ping(&ctx.http, x).await?,
        Err(_) => {
            msg.reply_ping(&ctx.http, "three or more arguments are needed.".to_string())
                .await?
        }
    };
    Ok(())
}

#[command]
#[description = "全ての要素を返す"]
async fn parse(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let s = args
        .iter::<String>()
        .collect::<std::result::Result<Vec<String>, _>>()?
        .join(" ");
    msg.reply_ping(&ctx.http, s).await?;
    Ok(())
}

#[command]
#[description = "全ての数値を返す"]
async fn parse_u64(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    match args
        .iter::<u64>()
        .collect::<std::result::Result<Vec<u64>, _>>()
    {
        Ok(vec) => {
            msg.reply_ping(
                &ctx.http,
                vec.iter()
                    .map(|value| value.to_string())
                    .collect::<Vec<String>>()
                    .join(" "),
            )
            .await?;
            Ok(())
        }
        Err(_) => {
            msg.reply_ping(&ctx.http, "parse error".to_string()).await?;
            Ok(())
        }
    }
}

#[command]
#[description = "embedのテスト"]
async fn test_embed(ctx: &Context, msg: &Message, mut args: Args) -> CommandResult {
    let optional_avatar_url = msg.author.avatar_url();
    let optional_author_name = msg.author_nick(&ctx.http).await;
    let vec = args
        .iter::<String>()
        .collect::<std::result::Result<Vec<String>, _>>()?;
    msg.channel_id
        .send_message(&ctx.http, |m| {
            m.content("test");
            m.reference_message(msg);
            m.tts(true);
            m.embed(|e| {
                e.title("This is an embed");
                e.description("With a description");
                e.author(|author| {
                    if let Some(avatar_url) = optional_avatar_url {
                        author.icon_url(avatar_url);
                    };
                    if let Some(name) = optional_author_name {
                        author.name(name);
                    };
                    author
                });
                for (n, s) in vec.iter().enumerate() {
                    e.field(n, s, true);
                }
                e
            });
            m
        })
        .await?;
    Ok(())
}

#[command]
#[description = "20数える"]
async fn count20(ctx: &Context, msg: &Message) -> CommandResult {
    let mut message = msg.channel_id.say(&ctx.http, 20.to_string()).await?;
    for i in (0..20).rev() {
        tokio::time::sleep(Duration::from_secs(1)).await;
        let _ = message
            .edit(&ctx.http, |m| m.content(i.to_string()))
            .await?;
    }
    Ok(())
}

#[command]
#[description = "nkodice"]
async fn nkodice(ctx: &Context, msg: &Message) -> CommandResult {
    let dice = vec!["う", "ま", "ち", "ん", "こ", "お"]
        .iter()
        .map(|v| v.to_string())
        .collect();
    let mut demes = Vec::new();
    for _ in 1..=5 {
        demes.push(roll(&dice));
    }
    let demes = demes;
    for deme in &demes {
        if let Some(v) = deme {
            tokio::time::sleep(Duration::from_secs(1)).await;
            let _ = msg.channel_id.say(&ctx.http, v).await?;
        }
    }
    let u = demes
        .iter()
        .filter(|deme| **deme == Some("う".to_string()))
        .count();
    let ma = demes
        .iter()
        .filter(|deme| **deme == Some("ま".to_string()))
        .count();
    let chi = demes
        .iter()
        .filter(|deme| **deme == Some("ち".to_string()))
        .count();
    let nn = demes
        .iter()
        .filter(|deme| **deme == Some("ん".to_string()))
        .count();
    let ko = demes
        .iter()
        .filter(|deme| **deme == Some("こ".to_string()))
        .count();
    let o = demes
        .iter()
        .filter(|deme| **deme == Some("お".to_string()))
        .count();
    if u >= 1 && nn >= 1 && chi >= 1 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        let _ = msg
            .channel_id
            .say(&ctx.http, "***UNCHI***".to_string())
            .await?;
    }
    if u >= 1 && nn >= 1 && ko >= 1 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        let _ = msg
            .channel_id
            .say(&ctx.http, "***UNKO***".to_string())
            .await?;
    }
    if ma >= 1 && nn >= 1 && ko >= 1 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        let _ = msg
            .channel_id
            .say(&ctx.http, "***MANKO***".to_string())
            .await?;
    }
    if o >= 1 && ma >= 1 && nn >= 1 && ko >= 1 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        let _ = msg
            .channel_id
            .say(&ctx.http, "***OMANKO***".to_string())
            .await?;
    }
    if chi >= 1 && nn >= 1 && ko >= 1 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        let _ = msg
            .channel_id
            .say(&ctx.http, "***CHINKO***".to_string())
            .await?;
    }
    if chi >= 2 && nn >= 2 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        let _ = msg
            .channel_id
            .say(&ctx.http, "***CHINCHIN***".to_string())
            .await?;
    }
    if o > 1 && chi >= 2 && nn >= 2 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        let _ = msg
            .channel_id
            .say(&ctx.http, "***OCHINCHIN***".to_string())
            .await?;
    }
    Ok(())
}


fn roll(dice: &Vec<String>) -> Option<String> {
    let mut rng = rand::thread_rng();
    let i: i32 = rng.gen();
    if i % 10 != 0 {
        dice.choose(&mut rand::thread_rng()).map(|v| v.clone())
    } else {
        None
    }
}

#[derive(Clone)]
struct HutougouPuzzle {
    size: usize,
    collect_number: usize,
    grid: Vec<Vec<char>>,
    possible_numbers: Vec<Vec<HashSet<u8>>>,
}

impl HutougouPuzzle {
    fn from_grid(grid: &Vec<Vec<char>>) -> Option<Self> {
        let size = (grid.len() + 1) / 2;
        let mut collect_number = 0;
        let mut possible_numbers: Vec<Vec<HashSet<u8>>> = Vec::new();
        for row in 0..size {
            let mut possible_numbers_row: Vec<HashSet<u8>> = Vec::new();
            for col in 0..size {
                let possible_numbers_cell: HashSet<u8> = match grid[row * 2][col * 4] {
                    '?' => (1..=size as u8).into_iter().collect(),
                    digit @ '1' ..= '9' => {collect_number += 1; Some(digit as u8 - 48).into_iter().collect()},
                    _ => return None,
                };
                possible_numbers_row.push(possible_numbers_cell);
            }
            let possible_numbers_row = possible_numbers_row;
            possible_numbers.push(possible_numbers_row);
        }
        let possible_numbers = possible_numbers;
        Some(Self{size, collect_number, grid: grid.clone(), possible_numbers})
    }
    fn get_mut_digit_character(&mut self, row: usize, col: usize) -> &mut char {
        &mut self.grid[row * 2][col * 4]
    }
    fn get_hutougou_row(&self, row: usize, col: usize) -> char {
        self.grid[row * 2][col * 4 + 2]
    }
    fn get_hutougou_col(&self, row: usize, col: usize) -> char {
        self.grid[row * 2 + 1][col * 4]
    }
    fn complete_list_row(&self, row: usize) -> Vec<HashSet<u8>> {
        let mut list = Vec::new();
        let sets: Vec<&HashSet<u8>> = self.possible_numbers[row].iter().collect();
        let kosuu = self.size;
        for i in (1..=kosuu).rev() {
            for set in (0..kosuu).combinations(i).map(|v| v.into_iter().fold(HashSet::new(), |mut u, j| {u.extend(sets[j].clone()); u})).filter(|set| set.len() == i) {
                list.push(set);
            }
        }
        list
    }
    fn complete_list_col(&self, col: usize) -> Vec<HashSet<u8>> {
        let mut list = Vec::new();
        let sets: Vec<&HashSet<u8>> = self.possible_numbers.iter().map(|v| &v[col]).collect();
        let kosuu = self.size;
        for i in (1..=kosuu).rev() {
            for set in (0..kosuu).combinations(i).map(|v| v.into_iter().fold(HashSet::new(), |mut u, j| {u.extend(sets[j].clone()); u})).filter(|set| set.len() == i) {
                list.push(set);
            }
        }
        list
    }
    fn possible_numbers_range(&self, row: usize, col: usize) -> Option<HashSet<u8>> {
        let size = self.size;
        let mut min = 1;
        let mut max = size as u8;
        if row > 0 {
            match self.get_hutougou_col(row - 1, col) {
                '^' => min = cmp::max(min, self.possible_numbers[row - 1][col].iter().min().expect("elements must exist") + 1),
                'v' => max = cmp::min(max, self.possible_numbers[row - 1][col].iter().max().expect("elements must exist") - 1),
                _ => return None,
            }
        }
        if col > 0 {
            match self.get_hutougou_row(row, col - 1) {
                '<' => min = cmp::max(min, self.possible_numbers[row][col - 1].iter().min().expect("elements must exist") + 1),
                '>' => max = cmp::min(max, self.possible_numbers[row][col - 1].iter().max().expect("elements must exist") - 1),
                _ => return None,
            }
        }
        if col < size - 1 {
            match self.get_hutougou_row(row, col) {
                '>' => min = cmp::max(min, self.possible_numbers[row][col + 1].iter().min().expect("elements must exist") + 1),
                '<' => max = cmp::min(max, self.possible_numbers[row][col + 1].iter().max().expect("elements must exist") - 1),
                _ => return None,
            }
        }
        if row < size - 1 {
            match self.get_hutougou_col(row, col) {
                'v' => min = cmp::max(min, self.possible_numbers[row + 1][col].iter().min().expect("elements must exist") + 1),
                '^' => max = cmp::min(max, self.possible_numbers[row + 1][col].iter().max().expect("elements must exist") - 1),
                _ => return None,
            }
        }
        Some((min..=max).collect())
    }
    fn reduce_possible_numbers(&mut self, row: usize, col: usize) -> std::result::Result<bool, &'static str> {
        if self.collect_number == self.size * self.size {
            return Ok(true);
        }
        let kosuu_before = self.possible_numbers[row][col].len();
        let mut changed = self.possible_numbers[row][col].clone();
        let range = match self.possible_numbers_range(row, col) {
            Some(range) => range,
            None => return Err("不等号が適切ではありません"),
        };
        changed = changed.intersection(&range).map(|&value| value).collect();
        let complete_list_row = self.complete_list_row(row);
        let complete_list_col = self.complete_list_col(col);
        for complete_pettern in complete_list_row.into_iter().chain(complete_list_col.into_iter()) {
            let temp: HashSet<u8> = changed.difference(&complete_pettern).map(|&value| value).collect();
            if !temp.is_empty() {
                changed = temp;
            }
        }
        let kosuu_after = changed.len();
        if kosuu_after == 0 {
            return Err("可能性がなくなりました。このプログラムが間違っているか、パズルが間違っています。");
        }
        if kosuu_before != 1 && kosuu_after == 1 {
            *self.get_mut_digit_character(row, col) = (changed.iter().next().expect("element must be one") + 48) as char;
            self.collect_number += 1;
            if self.collect_number == self.size * self.size {
                return Ok(true);
            }
        }
        if kosuu_after < kosuu_before {
            self.possible_numbers[row][col] = changed;
            for r in 0..self.size {
                let b = self.reduce_possible_numbers(r, col)?;
                if b {
                    return Ok(true);
                }
            }
            for c in 0..self.size {
                let b = self.reduce_possible_numbers(row, c)?;
                if b {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }
    fn solve(&mut self) -> std::result::Result<(), &'static str> {
        for row in 0..self.size {
            for col in 0..self.size {
                self.reduce_possible_numbers(row, col)?;
            }
        }
        if self.collect_number != self.size * self.size {
            println!("collect_number: {}, size: {}", self.collect_number, self.size);
            Err("確定しませんでした。このプログラムが間違っているか、パズルが間違っています")
        } else {
            Ok(())
        }
    }
    async fn print_zantei(self, ctx: &Context, msg: &Message) -> CommandResult {
        let mut s = "".to_string();
        for (row, possible_numbers_row) in self.possible_numbers.into_iter().enumerate() {
            for (col, possible_numbers_cell) in possible_numbers_row.into_iter().enumerate() {
                s += &format!("({}, {}): ", row, col);
                s += &possible_numbers_cell.into_iter().map(|value| format!("{}", value)).collect::<Vec<String>>().join(", ");
                s += "\n";
            }
            s += "\n";
        }
        msg.reply_ping(&ctx.http, "```".to_string() + &s + "```").await?;
        Ok(())
    }
    async fn print(self, ctx: &Context, msg: &Message) -> CommandResult {
        let s = self.grid.into_iter().map(|v| v.into_iter().collect::<String>()).collect::<Vec<String>>().join("\n");
        msg.reply_ping(&ctx.http, "```".to_string() + &s + "```").await?;
        Ok(())
    }
}

#[command]
#[description = "不等号パズルソルバー"]
async fn solve_hutougou(ctx: &Context, msg: &Message) -> CommandResult {
    let blocks:Vec<&str> = msg.content.split("```").collect();
    if blocks.len() != 3 {
        msg.reply_ping(&ctx.http, "Code block was not found or too many").await?;
        return Ok(());
    }
    let block = blocks.get(1).expect("blocks must 3 elements").trim();
    let grid: Vec<Vec<char>> = block.split("\n").map(|line| line.chars().collect::<Vec<_>>()).collect();
    let size = grid.len();
    if size < 1 || size > 17 {
        msg.reply_ping(&ctx.http, "puzzle size is illegal").await?;
        return Ok(());
    }
    if grid.iter().any(|v| v.len() != 2 * size - 1) {
        msg.reply_ping(&ctx.http, "Must be square").await?;
        return Ok(());
    }
    let mut puzzle = match HutougouPuzzle::from_grid(&grid) {
        Some(v) => v,
        None => {
            msg.reply_ping(&ctx.http, "puzzle format is illegal").await?;
            return Ok(());
        },
    };
    if let Err(err_msg) = puzzle.solve() {
        msg.reply_ping(&ctx.http, err_msg).await?;
        puzzle.clone().print_zantei(ctx, msg).await?;
        puzzle.print(ctx, msg).await?;
        return Ok(())
    }
    puzzle.print(ctx, msg).await?;
    // msg.reply_ping(&ctx.http, "Has not implemented yet").await?;
    Ok(())
}

#[derive(Serialize, Deserialize)]
struct Token {
    token: String,
}

//{"token": "This_is_Token"} の形のトークンを取り出す関数
fn get_token(file_name: &str) -> Result<String> {
    let file = File::open(file_name).unwrap();
    let reader = BufReader::new(file);
    let t: Token = serde_json::from_reader(reader).unwrap();
    Ok(t.token)
}

#[tokio::main]
async fn main() {
    // Discord Bot Token を設定
    let token = get_token("config.json").expect("Err トークンが見つかりません");
    // コマンド系の設定
    let framework = StandardFramework::new()
        // |c| c はラムダ式
        .configure(|c| c.prefix("L:")) // コマンドプレフィックス
        .help(&MY_HELP) // ヘルプコマンドを追加
        .group(&TEST_GROUP) // general を追加するには,GENERAL_GROUP とグループ名をすべて大文字にする
        .group(&GAME_GROUP)
        .group(&PUZZLE_GROUP);

    // Botのクライアントを作成
    let mut client = Client::builder(&token)
        .event_handler(Handler) // 取得するイベント
        .framework(framework) // コマンドを登録
        .await
        .expect("Err creating client"); // エラーハンドリング

    // メインループ。Botを起動
    if let Err(why) = client.start().await {
        println!("Client error: {:?}", why);
    }
}
