//! Comprehensive benchmarks for SharedString and FilePath serialization.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use holo_base::file_path::FilePath;
use holo_base::shared_string::SharedString;
use speedy::{Readable, Writable};

fn create_shared_strings(size: usize) -> Vec<SharedString> {
    let mut data = Vec::with_capacity(size);
    for i in 0..size {
        let string = format!(
            "test_string_{}_with_some_longer_content_to_make_it_more_realistic_{}",
            i,
            i * 2
        );
        data.push(SharedString::new(string));
    }
    data
}

fn create_file_paths(size: usize) -> Vec<FilePath> {
    let mut data = Vec::with_capacity(size);
    for i in 0..size {
        let path = format!("src/components/module_{}/submodule/file_{}.rs", i % 10, i);
        data.push(FilePath::new(path));
    }
    data
}

// SharedString benchmarks
fn bench_shared_string_serde(c: &mut Criterion) {
    let data = create_shared_strings(1000);

    c.bench_function("shared_string_serde_serialize", |b| {
        b.iter(|| {
            let serialized = serde_json::to_string(&black_box(&data)).unwrap();
            black_box(serialized)
        })
    });

    c.bench_function("shared_string_serde_deserialize", |b| {
        let serialized = serde_json::to_string(&data).unwrap();
        b.iter(|| {
            let deserialized: Vec<SharedString> =
                serde_json::from_str(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });

    c.bench_function("shared_string_serde_roundtrip", |b| {
        b.iter(|| {
            let serialized = serde_json::to_string(&black_box(&data)).unwrap();
            let deserialized: Vec<SharedString> =
                serde_json::from_str(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
}

fn bench_shared_string_speedy(c: &mut Criterion) {
    let data = create_shared_strings(1000);

    c.bench_function("shared_string_speedy_serialize", |b| {
        b.iter(|| {
            let serialized = black_box(&data).write_to_vec().unwrap();
            black_box(serialized)
        })
    });

    c.bench_function("shared_string_speedy_deserialize", |b| {
        let serialized = data.write_to_vec().unwrap();
        b.iter(|| {
            let deserialized =
                Vec::<SharedString>::read_from_buffer(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });

    c.bench_function("shared_string_speedy_roundtrip", |b| {
        b.iter(|| {
            let serialized = black_box(&data).write_to_vec().unwrap();
            let deserialized =
                Vec::<SharedString>::read_from_buffer(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
}

fn bench_shared_string_bitcode(c: &mut Criterion) {
    let data = create_shared_strings(1000);

    c.bench_function("shared_string_bitcode_serialize", |b| {
        b.iter(|| {
            // Convert to Vec<String> for bitcode serialization
            let string_data: Vec<String> = black_box(&data)
                .iter()
                .map(|s| s.as_str().to_string())
                .collect();
            let serialized = bitcode::encode(&string_data);
            black_box(serialized)
        })
    });

    c.bench_function("shared_string_bitcode_deserialize", |b| {
        // Convert to Vec<String> and encode first
        let string_data: Vec<String> = data.iter().map(|s| s.as_str().to_string()).collect();
        let serialized = bitcode::encode(&string_data);

        b.iter(|| {
            let deserialized: Vec<String> = bitcode::decode(black_box(&serialized)).unwrap();
            // Convert back to SharedString for fair comparison
            let shared_strings: Vec<SharedString> =
                deserialized.into_iter().map(SharedString::new).collect();
            black_box(shared_strings)
        })
    });

    c.bench_function("shared_string_bitcode_roundtrip", |b| {
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

// FilePath benchmarks
fn bench_file_path_serde(c: &mut Criterion) {
    let data = create_file_paths(1000);

    c.bench_function("file_path_serde_serialize", |b| {
        b.iter(|| {
            let serialized = serde_json::to_string(&black_box(&data)).unwrap();
            black_box(serialized)
        })
    });

    c.bench_function("file_path_serde_deserialize", |b| {
        let serialized = serde_json::to_string(&data).unwrap();
        b.iter(|| {
            let deserialized: Vec<FilePath> = serde_json::from_str(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });

    c.bench_function("file_path_serde_roundtrip", |b| {
        b.iter(|| {
            let serialized = serde_json::to_string(&black_box(&data)).unwrap();
            let deserialized: Vec<FilePath> = serde_json::from_str(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
}

fn bench_file_path_speedy(c: &mut Criterion) {
    let data = create_file_paths(1000);

    c.bench_function("file_path_speedy_serialize", |b| {
        b.iter(|| {
            let serialized = black_box(&data).write_to_vec().unwrap();
            black_box(serialized)
        })
    });

    c.bench_function("file_path_speedy_deserialize", |b| {
        let serialized = data.write_to_vec().unwrap();
        b.iter(|| {
            let deserialized = Vec::<FilePath>::read_from_buffer(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });

    c.bench_function("file_path_speedy_roundtrip", |b| {
        b.iter(|| {
            let serialized = black_box(&data).write_to_vec().unwrap();
            let deserialized = Vec::<FilePath>::read_from_buffer(black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
}

fn bench_file_path_bitcode(c: &mut Criterion) {
    let data = create_file_paths(1000);

    c.bench_function("file_path_bitcode_serialize", |b| {
        b.iter(|| {
            // Convert to Vec<String> for bitcode serialization
            let string_data: Vec<String> = black_box(&data)
                .iter()
                .map(|p| p.as_str().to_string())
                .collect();
            let serialized = bitcode::encode(&string_data);
            black_box(serialized)
        })
    });

    c.bench_function("file_path_bitcode_deserialize", |b| {
        // Convert to Vec<String> and encode first
        let string_data: Vec<String> = data.iter().map(|p| p.as_str().to_string()).collect();
        let serialized = bitcode::encode(&string_data);

        b.iter(|| {
            let deserialized: Vec<String> = bitcode::decode(black_box(&serialized)).unwrap();
            // Convert back to FilePath for fair comparison
            let file_paths: Vec<FilePath> = deserialized.into_iter().map(FilePath::new).collect();
            black_box(file_paths)
        })
    });

    c.bench_function("file_path_bitcode_roundtrip", |b| {
        b.iter(|| {
            // Convert to Vec<String> for bitcode operations
            let string_data: Vec<String> = black_box(&data)
                .iter()
                .map(|p| p.as_str().to_string())
                .collect();
            let serialized = bitcode::encode(&string_data);
            let deserialized: Vec<String> = bitcode::decode(&serialized).unwrap();
            // Convert back to FilePath for fair comparison
            let file_paths: Vec<FilePath> = deserialized.into_iter().map(FilePath::new).collect();
            black_box(file_paths)
        })
    });
}

// Size comparison benchmarks
fn bench_size_comparison(c: &mut Criterion) {
    let shared_strings = create_shared_strings(1000);
    let file_paths = create_file_paths(1000);

    // SharedString sizes
    let ss_serde_size = serde_json::to_string(&shared_strings).unwrap().len();
    let ss_speedy_size = shared_strings.write_to_vec().unwrap().len();
    // Convert to Vec<String> for bitcode size comparison
    let ss_string_data: Vec<String> = shared_strings
        .iter()
        .map(|s| s.as_str().to_string())
        .collect();
    let ss_bitcode_size = bitcode::encode(&ss_string_data).len();

    // FilePath sizes
    let fp_serde_size = serde_json::to_string(&file_paths).unwrap().len();
    let fp_speedy_size = file_paths.write_to_vec().unwrap().len();
    // Convert to Vec<String> for bitcode size comparison
    let fp_string_data: Vec<String> = file_paths.iter().map(|p| p.as_str().to_string()).collect();
    let fp_bitcode_size = bitcode::encode(&fp_string_data).len();

    println!("\n=== Serialization Size Comparison for 1000 items ===");
    println!("SharedString:");
    println!("  serde_json: {} bytes", ss_serde_size);
    println!("  speedy: {} bytes", ss_speedy_size);
    println!("  bitcode: {} bytes", ss_bitcode_size);
    println!(
        "  speedy compression ratio: {:.2}x vs serde_json",
        ss_serde_size as f64 / ss_speedy_size as f64
    );
    println!(
        "  bitcode compression ratio: {:.2}x vs serde_json",
        ss_serde_size as f64 / ss_bitcode_size as f64
    );

    println!("FilePath:");
    println!("  serde_json: {} bytes", fp_serde_size);
    println!("  speedy: {} bytes", fp_speedy_size);
    println!("  bitcode: {} bytes", fp_bitcode_size);
    println!(
        "  speedy compression ratio: {:.2}x vs serde_json",
        fp_serde_size as f64 / fp_speedy_size as f64
    );
    println!(
        "  bitcode compression ratio: {:.2}x vs serde_json",
        fp_serde_size as f64 / fp_bitcode_size as f64
    );

    c.bench_function("size_info_shared_string", |b| {
        b.iter(|| black_box((ss_serde_size, ss_speedy_size, ss_bitcode_size)))
    });

    c.bench_function("size_info_file_path", |b| {
        b.iter(|| black_box((fp_serde_size, fp_speedy_size, fp_bitcode_size)))
    });
}

criterion_group!(
    benches,
    bench_shared_string_serde,
    bench_shared_string_speedy,
    bench_shared_string_bitcode,
    bench_file_path_serde,
    bench_file_path_speedy,
    bench_file_path_bitcode,
    bench_size_comparison
);
criterion_main!(benches);
