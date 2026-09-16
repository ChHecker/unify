use std::collections::HashSet;
use std::env;
use std::fs::{File, read_dir};
use std::io::{BufWriter, Write};
use std::path::Path;

use csv::ReaderBuilder;
use phf_codegen::Map;

fn main() {
    println!("cargo:rerun-if-changed=../units");

    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("codegen_units.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    prefixes(&mut file);
    units(&mut file);
    postfixes(&mut file);

    prefixes_short(&mut file);
    units_short(&mut file);
}

fn prefixes(file: &mut BufWriter<File>) {
    let mut outer_builder = phf_codegen::Map::new();

    for file in read_dir("../units").expect("Failed to read units directory") {
        let path = file.expect("Invalid directory entry").path();
        let file_name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
        if !(file_name.starts_with("prefixes-") && file_name.ends_with(".csv")) {
            continue;
        }

        // Extract language
        let lang = file_name
            .trim_start_matches("prefixes-")
            .trim_end_matches(".csv")
            .to_lowercase();

        let mut rdr = ReaderBuilder::new()
            .has_headers(false)
            .from_path(path)
            .unwrap();

        let mut inner_builder = Map::new();
        let mut keys_seen = HashSet::new();

        for result in rdr.records() {
            let record = result.unwrap();
            let long = record[0].trim();
            let symbol = record[2].trim();

            let val_str = format!("{:?}", symbol);

            let long = long.to_string();
            if !keys_seen.contains(&long) {
                inner_builder.entry(long.clone(), &val_str);
                keys_seen.insert(long);
            }
        }

        let inner_map_code = inner_builder.build().to_string();
        outer_builder.entry(lang, &inner_map_code);
    }

    writeln!(
        file,
        "static PREFIXES: phf::Map<&'static str, phf::Map<&'static str, &'static str>> = {};",
        outer_builder.build()
    )
    .unwrap();
}

fn units(file: &mut BufWriter<File>) {
    let mut outer_builder = phf_codegen::Map::new();

    for file in read_dir("../units").expect("Failed to read units directory") {
        let path = file.expect("Invalid directory entry").path();
        let file_name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
        if !(file_name.starts_with("units-") && file_name.ends_with(".csv")) {
            continue;
        }

        // Extract language
        let lang = file_name
            .trim_start_matches("units-")
            .trim_end_matches(".csv")
            .to_lowercase();
        let mut rdr = ReaderBuilder::new()
            .has_headers(false)
            .from_path(path)
            .unwrap();
        let mut rdr_money = ReaderBuilder::new()
            .has_headers(false)
            .from_path("../units/money.csv")
            .unwrap();

        let mut inner_builder = Map::new();
        let mut keys_seen = HashSet::new();

        for result in rdr.records().chain(rdr_money.records()) {
            let record = result.unwrap();
            let long = record[0].trim();
            let symbol = record[2].trim();
            let space: bool = record[3].trim().parse().unwrap();

            let val_str = format!("UnitSpec {{ symbol: {:?}, space: {} }}", symbol, space);

            let long = long.to_string();
            if !keys_seen.contains(&long) {
                inner_builder.entry(long.clone(), &val_str);
                keys_seen.insert(long);
            }
        }

        let inner_map_code = inner_builder.build().to_string();
        outer_builder.entry(lang, &inner_map_code);
    }

    writeln!(
        file,
        "static UNITS: phf::Map<&'static str, phf::Map<&'static str, UnitSpec>> = {};",
        outer_builder.build()
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
    let mut outer_builder = phf_codegen::Map::new();

    for file in read_dir("../units").expect("Failed to read units directory") {
        let path = file.expect("Invalid directory entry").path();
        let file_name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
        if !(file_name.starts_with("units-") && file_name.ends_with(".csv")) {
            continue;
        }

        // Extract language
        let lang = file_name
            .trim_start_matches("units-")
            .trim_end_matches(".csv")
            .to_lowercase();
        let mut rdr = ReaderBuilder::new()
            .has_headers(false)
            .from_path(path)
            .unwrap();
        let mut rdr_money = ReaderBuilder::new()
            .has_headers(false)
            .from_path("../units/money.csv")
            .unwrap();

        let mut inner_builder = Map::new();
        let mut keys_seen = HashSet::new();

        for result in rdr.records().chain(rdr_money.records()) {
            let record = result.unwrap();
            let short = record[1].trim();
            let symbol = record[2].trim();
            let space: bool = record[3].trim().parse().unwrap();

            let val_str = format!("UnitSpec {{ symbol: {:?}, space: {} }}", symbol, space);

            let short = short.to_string();
            if !keys_seen.contains(&short) {
                inner_builder.entry(short.clone(), &val_str);
                keys_seen.insert(short);
            }
        }
        let inner_map_code = inner_builder.build().to_string();
        outer_builder.entry(lang, &inner_map_code);
    }

    writeln!(
        file,
        "static UNITS_SHORT: phf::Map<&'static str, phf::Map<&'static str, UnitSpec>> = {};",
        outer_builder.build()
    )
    .unwrap();
}

fn prefixes_short(file: &mut BufWriter<File>) {
    let mut outer_builder = phf_codegen::Map::new();

    for file in read_dir("../units").expect("Failed to read units directory") {
        let path = file.expect("Invalid directory entry").path();
        let file_name = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
        if !(file_name.starts_with("prefixes-") && file_name.ends_with(".csv")) {
            continue;
        }

        // Extract language
        let lang = file_name
            .trim_start_matches("prefixes-")
            .trim_end_matches(".csv")
            .to_lowercase();

        let mut rdr = ReaderBuilder::new()
            .has_headers(false)
            .from_path(path)
            .unwrap();

        let mut inner_builder = Map::new();
        let mut keys_seen = HashSet::new();
        for result in rdr.records() {
            let record = result.unwrap();
            let short = record[1].trim();
            let symbol = record[2].trim();

            let val_str = format!("{:?}", symbol);

            let short = short.to_string();
            if !keys_seen.contains(&short) {
                inner_builder.entry(short.clone(), &val_str);
                keys_seen.insert(short);
            }
        }
        let inner_map_code = inner_builder.build().to_string();
        outer_builder.entry(lang, &inner_map_code);
    }

    writeln!(
        file,
        "static PREFIXES_SHORT: phf::Map<&'static str, phf::Map<&'static str, &'static str>> = {};",
        outer_builder.build()
    )
    .unwrap();
}
