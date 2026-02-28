//! Simple benchmarks for SharedString serialization.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use holo_base::shared_string::SharedString;
use speedy::{Readable, Writable};

fn create_test_data(size: usize) -> Vec<SharedString> {
    let mut data = Vec::with_capacity(size);
    for i in 0..size {
        let string = format!("test_string_{}_with_some_content", i);
        data.push(SharedString::new(string));
    }
    data
}

fn bench_serde_json(c: &mut Criterion) {
    let data = create_test_data(1000);

    c.bench_function("serde_json_roundtrip", |b| {
        b.iter(|| {
            let serialized = serde_json::to_string(&black_box(&data)).unwrap();
            let deserialized: Vec<SharedString> =
                serde_json::from_str(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
}

fn bench_speedy(c: &mut Criterion) {
    let data = create_test_data(1000);

    c.bench_function("speedy_roundtrip", |b| {
        b.iter(|| {
            let serialized = black_box(&data).write_to_vec().unwrap();
            let deserialized =
                Vec::<SharedString>::read_from_buffer(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
}

fn bench_bitcode(c: &mut Criterion) {
    let data = create_test_data(1000);

    c.bench_function("bitcode_roundtrip", |b| {
        b.iter(|| {
            // Convert to Vec<String> for bitcode operations
            let string_data: Vec<String> = black_box(&data)
                .iter()
                .map(|s| s.as_str().to_string())
                .collect();
            let serialized = bitcode::encode(&string_data);
            let deserialized: Vec<String> = bitcode::decode(&serialized).unwrap();
            // Convert back to SharedString for fair comparison
            let shared_strings: Vec<SharedString> =
                deserialized.into_iter().map(SharedString::new).collect();
            black_box(shared_strings)
        })
    });
}

fn bench_size_comparison(c: &mut Criterion) {
    let data = create_test_data(1000);

    let serde_size = serde_json::to_string(&data).unwrap().len();
    let speedy_size = data.write_to_vec().unwrap().len();
    // Convert to Vec<String> for bitcode size comparison
    let string_data: Vec<String> = data.iter().map(|s| s.as_str().to_string()).collect();
    let bitcode_size = bitcode::encode(&string_data).len();

    println!("Serialization size comparison for 1000 SharedStrings:");
    println!("serde_json: {} bytes", serde_size);
    println!("speedy: {} bytes", speedy_size);
    println!("bitcode: {} bytes", bitcode_size);
    println!(
        "speedy compression ratio: {:.2}x vs serde_json",
        serde_size as f64 / speedy_size as f64
    );
    println!(
        "bitcode compression ratio: {:.2}x vs serde_json",
        serde_size as f64 / bitcode_size as f64
    );

    c.bench_function("size_info", |b| {
        b.iter(|| black_box((serde_size, speedy_size, bitcode_size)))
    });
}

criterion_group!(
    benches,
    bench_serde_json,
    bench_speedy,
    bench_bitcode,
    bench_size_comparison
);
criterion_main!(benches);
