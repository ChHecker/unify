cd wasm

echo "Building crate..."
echo
cargo build --release --target wasm32-unknown-unknown
echo

echo "Copying WASM file..."
cp target/wasm32-unknown-unknown/release/unify.wasm ../format.wasm

cd ..
echo "Done."