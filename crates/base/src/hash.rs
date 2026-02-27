/// Hashes a string using the rapidhash algorithm.
pub fn hash_string(s: &str) -> u64 {
    rapidhash::v3::rapidhash_v3(s.as_bytes())
}
