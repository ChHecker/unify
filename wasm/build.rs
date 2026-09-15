use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

use csv::ReaderBuilder;
use phf_codegen::Map;

fn main() {
    println!("cargo:rerun-if-changed=../units/prefixes-en.csv");
    println!("cargo:rerun-if-changed=../units/units-en.csv");
    println!("cargo:rerun-if-changed=../units/postfixes.csv");
    println!("cargo:rerun-if-changed=../units/money.csv");

    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("codegen_units.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    prefixes(&mut file);
    units(&mut file);
    postfixes(&mut file);

    prefixes_short(&mut file);
    units_short(&mut file);
}

fn prefixes(file: &mut BufWriter<File>) {
    let mut rdr = ReaderBuilder::new()
        .has_headers(false)
        .from_path("../units/prefixes-en.csv")
        .unwrap();

    let mut builder = Map::new();
    let mut keys_seen = HashSet::new();

    for result in rdr.records() {
        let record = result.unwrap();
        let long = record[0].trim();
        let symbol = record[2].trim();

        let val_str = format!("{:?}", symbol);

        let long = long.to_string();
        if !keys_seen.contains(&long) {
            builder.entry(long.clone(), &val_str);
            keys_seen.insert(long);
        }
    }

    writeln!(
        file,
        "static PREFIXES: phf::Map<&'static str, &'static str> = {};",
        builder.build()
    )
    .unwrap();
}

fn units(file: &mut BufWriter<File>) {
    let mut rdr = ReaderBuilder::new()
        .has_headers(false)
        .from_path("../units/units-en.csv")
        .unwrap();
    let mut rdr_money = ReaderBuilder::new()
        .has_headers(false)
        .from_path("../units/money.csv")
        .unwrap();

    let mut builder = Map::new();
    let mut keys_seen = HashSet::new();

    for result in rdr.records().chain(rdr_money.records()) {
        let record = result.unwrap();
        let long = record[0].trim();
        let symbol = record[2].trim();
        let space: bool = record[3].trim().parse().unwrap();

        let val_str = format!("UnitSpec {{ symbol: {:?}, space: {} }}", symbol, space);

        let long = long.to_string();
        if !keys_seen.contains(&long) {
            builder.entry(long.clone(), &val_str);
            keys_seen.insert(long);
        }
    }

    writeln!(
        file,
        "static UNITS: phf::Map<&'static str, UnitSpec> = {};",
        builder.build()
    )
    .unwrap();
}

fn postfixes(file: &mut BufWriter<File>) {
    let mut rdr = ReaderBuilder::new()
        .has_headers(false)
        .from_path("../units/postfixes.csv")
        .unwrap();

    let mut builder = Map::new();
    let mut keys_seen = HashSet::new();

    for result in rdr.records() {
        let record = result.unwrap();
        let long = record[0].trim();
        let symbol = record[1].trim();

        let val_str = format!("{:?}", symbol);

        let long = long.to_string();
        if !keys_seen.contains(&long) {
            builder.entry(long.clone(), &val_str);
            keys_seen.insert(long);
        }
    }

    writeln!(
        file,
        "static POSTFIXES: phf::Map<&'static str, &'static str> = {};",
        builder.build()
    )
    .unwrap();
}

fn units_short(file: &mut BufWriter<File>) {
    let mut rdr = ReaderBuilder::new()
        .has_headers(false)
        .from_path("../units/units-en.csv")
        .unwrap();
    let mut rdr_money = ReaderBuilder::new()
        .has_headers(false)
        .from_path("../units/money.csv")
        .unwrap();

    let mut builder = Map::new();
    let mut keys_seen = HashSet::new();

    for result in rdr.records().chain(rdr_money.records()) {
        let record = result.unwrap();
        let short = record[1].trim();
        let symbol = record[2].trim();
        let space: bool = record[3].trim().parse().unwrap();

        let val_str = format!("UnitSpec {{ symbol: {:?}, space: {} }}", symbol, space);

        let short = short.to_string();
        if !keys_seen.contains(&short) {
            builder.entry(short.clone(), &val_str);
            keys_seen.insert(short);
        }
    }

    writeln!(
        file,
        "static UNITS_SHORT: phf::Map<&'static str, UnitSpec> = {};",
        builder.build()
    )
    .unwrap();
}

fn prefixes_short(file: &mut BufWriter<File>) {
    let mut rdr = ReaderBuilder::new()
        .has_headers(false)
        .from_path("../units/prefixes-en.csv")
        .unwrap();

    let mut builder = Map::new();
    let mut keys_seen = HashSet::new();

    for result in rdr.records() {
        let record = result.unwrap();
        let short = record[1].trim();
        let symbol = record[2].trim();

        let val_str = format!("{:?}", symbol);

        let short = short.to_string();
        if !keys_seen.contains(&short) {
            builder.entry(short.clone(), &val_str);
            keys_seen.insert(short);
        }
    }

    writeln!(
        file,
        "static PREFIXES_SHORT: phf::Map<&'static str, &'static str> = {};",
        builder.build()
    )
    .unwrap();
}
