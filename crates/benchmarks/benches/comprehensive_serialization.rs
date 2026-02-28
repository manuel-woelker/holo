//! Comprehensive benchmarks for SharedString and FilePath serialization.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use holo_base::file_path::FilePath;
use holo_base::shared_string::SharedString;
use serde_json;
use speedy::{Readable, Writable};

fn create_shared_strings(size: usize) -> Vec<SharedString> {
    let mut data = Vec::with_capacity(size);
    for i in 0..size {
        let string = format!("test_string_{}_with_some_longer_content_to_make_it_more_realistic_{}", i, i * 2);
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
            let deserialized: Vec<SharedString> = serde_json::from_str(&black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
    
    c.bench_function("shared_string_serde_roundtrip", |b| {
        b.iter(|| {
            let serialized = serde_json::to_string(&black_box(&data)).unwrap();
            let deserialized: Vec<SharedString> = serde_json::from_str(&black_box(&serialized)).unwrap();
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
            let deserialized = Vec::<SharedString>::read_from_buffer(&black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
    
    c.bench_function("shared_string_speedy_roundtrip", |b| {
        b.iter(|| {
            let serialized = black_box(&data).write_to_vec().unwrap();
            let deserialized = Vec::<SharedString>::read_from_buffer(&black_box(&serialized)).unwrap();
            black_box(deserialized)
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
            let deserialized: Vec<FilePath> = serde_json::from_str(&black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
    
    c.bench_function("file_path_serde_roundtrip", |b| {
        b.iter(|| {
            let serialized = serde_json::to_string(&black_box(&data)).unwrap();
            let deserialized: Vec<FilePath> = serde_json::from_str(&black_box(&serialized)).unwrap();
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
            let deserialized = Vec::<FilePath>::read_from_buffer(&black_box(&serialized)).unwrap();
            black_box(deserialized)
        })
    });
    
    c.bench_function("file_path_speedy_roundtrip", |b| {
        b.iter(|| {
            let serialized = black_box(&data).write_to_vec().unwrap();
            let deserialized = Vec::<FilePath>::read_from_buffer(&black_box(&serialized)).unwrap();
            black_box(deserialized)
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
    
    // FilePath sizes
    let fp_serde_size = serde_json::to_string(&file_paths).unwrap().len();
    let fp_speedy_size = file_paths.write_to_vec().unwrap().len();
    
    println!("\n=== Serialization Size Comparison for 1000 items ===");
    println!("SharedString:");
    println!("  serde_json: {} bytes", ss_serde_size);
    println!("  speedy: {} bytes", ss_speedy_size);
    println!("  speedy compression ratio: {:.2}x vs serde_json", ss_serde_size as f64 / ss_speedy_size as f64);
    
    println!("FilePath:");
    println!("  serde_json: {} bytes", fp_serde_size);
    println!("  speedy: {} bytes", fp_speedy_size);
    println!("  speedy compression ratio: {:.2}x vs serde_json", fp_serde_size as f64 / fp_speedy_size as f64);
    
    c.bench_function("size_info_shared_string", |b| {
        b.iter(|| {
            black_box((ss_serde_size, ss_speedy_size))
        })
    });
    
    c.bench_function("size_info_file_path", |b| {
        b.iter(|| {
            black_box((fp_serde_size, fp_speedy_size))
        })
    });
}

criterion_group!(
    benches,
    bench_shared_string_serde,
    bench_shared_string_speedy,
    bench_file_path_serde,
    bench_file_path_speedy,
    bench_size_comparison
);
criterion_main!(benches);
