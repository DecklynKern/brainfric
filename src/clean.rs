pub fn clean(code: &mut String) {

    while let Some(idx) = code.rfind("+-") {
        code.remove(idx);
        code.remove(idx);
    }

    while let Some(idx) = code.rfind("-+") {
        code.remove(idx);
        code.remove(idx);
    }
}