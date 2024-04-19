const PATTERNS: [&str; 4] = ["<>", "><", "+-", "-+"];
const REPLACINGS: [(&str, &str); 1] = [("][-]", "]")];

pub fn clean(code: &mut String) {

    for pattern in PATTERNS {
        while let Some(idx) = code.rfind(pattern) {
            for _ in 0..pattern.len() {
                code.remove(idx);
            }
        }
    }

    while code.ends_with('>') || code.ends_with('<') {
        code.pop();
    }
}