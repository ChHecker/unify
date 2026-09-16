cd wasm
cargo build --release --target wasm32-unknown-unknown
cp target/wasm32-unknown-unknown/release/unify.wasm ../format.wasm
cd ..