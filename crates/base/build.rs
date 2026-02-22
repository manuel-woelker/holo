use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    let manifest_dir = PathBuf::from(
        std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR should be set"),
    );
    let repo_root = find_repo_root(&manifest_dir).unwrap_or(manifest_dir);

    emit_rerun_hints(&repo_root);
    let revision = build_revision_string(&repo_root);
    println!("cargo:rustc-env=HOLO_PROJECT_REVISION={revision}");
}

fn find_repo_root(start: &Path) -> Option<PathBuf> {
    for candidate in start.ancestors() {
        if candidate.join(".git").exists() {
            return Some(candidate.to_path_buf());
        }
    }
    None
}

fn emit_rerun_hints(repo_root: &Path) {
    let git_dir = repo_root.join(".git");
    println!("cargo:rerun-if-changed={}", git_dir.join("HEAD").display());
    println!("cargo:rerun-if-changed={}", git_dir.join("index").display());
    println!(
        "cargo:rerun-if-changed={}",
        git_dir.join("packed-refs").display()
    );
    println!(
        "cargo:rerun-if-changed={}",
        git_dir.join("refs").join("tags").display()
    );

    if let Ok(head) = std::fs::read_to_string(git_dir.join("HEAD")) {
        if let Some(reference) = head.strip_prefix("ref: ") {
            let reference = reference.trim();
            println!(
                "cargo:rerun-if-changed={}",
                git_dir.join(reference).display()
            );
        }
    }
}

fn build_revision_string(repo_root: &Path) -> String {
    let latest_tag = git(
        repo_root,
        &["describe", "--tags", "--abbrev=0", "--match", "v*"],
    )
    .unwrap_or_else(|| "v0.0.0".to_owned());

    let commits_since_tag = git(
        repo_root,
        &["rev-list", "--count", &format!("{latest_tag}..HEAD")],
    )
    .unwrap_or_else(|| "0".to_owned());

    let last_commit_date =
        git(repo_root, &["log", "-1", "--format=%cs"]).unwrap_or_else(|| "unknown-date".to_owned());
    let commit_id =
        git(repo_root, &["rev-parse", "--short", "HEAD"]).unwrap_or_else(|| "unknown".to_owned());

    let dirty_suffix = if is_dirty(repo_root) { "-dev" } else { "" };
    format!("{latest_tag}.{commits_since_tag}-{last_commit_date}-{commit_id}{dirty_suffix}")
}

fn is_dirty(repo_root: &Path) -> bool {
    match Command::new("git")
        .arg("-C")
        .arg(repo_root)
        .arg("status")
        .arg("--porcelain")
        .output()
    {
        Ok(output) => !String::from_utf8_lossy(&output.stdout).trim().is_empty(),
        Err(_) => false,
    }
}

fn git(repo_root: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo_root)
        .args(args)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let value = String::from_utf8(output.stdout).ok()?;
    Some(value.trim().to_owned())
}
