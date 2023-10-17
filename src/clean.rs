const PATTERNS: [&str; 4] = ["<>", "><", "+-", "-+"];

pub fn clean(code: &mut String) {

    for pattern in PATTERNS {
        while let Some(idx) = code.rfind(pattern) {
            for _ in 0..pattern.len() {
                code.remove(idx);
            }
        }
    }
}