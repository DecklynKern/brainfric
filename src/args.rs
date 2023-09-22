macro_rules! create_flags {
    ($($flag_name: ident, $flag_arg: literal, $flag_func_name: ident, $negate: literal),*) => {

        $(static mut $flag_name: bool = $negate;)*

        pub fn parse_args() {

            let args: Vec<_> = std::env::args().collect();

            unsafe {
                $(
                    $flag_name = $negate ^ args.contains(&String::from($flag_arg));
                )*
            }
        }

        $(
            pub fn $flag_func_name() -> bool {
                unsafe {
                    $flag_name
                }
            }
        )*
    };
}

create_flags!(
    DO_OPTIMIZATION,            "-doopt",           arg_do_optimization,            true,
    ALLOW_DELETE_VARIABLES,     "-allowdelvars",    arg_allow_delete_variables,     false,
    SHOW_LEX,                   "-showlex",         arg_show_lex,                   false,
    SHOW_PARSE,                 "-showparse",       arg_show_parse,                 false,
    SHOW_IR,                    "-showir",          arg_show_ir,                    false,
    SHOW_OPTIMIZATION_STEPS,    "-showoptsteps",    arg_show_optimization_steps,    false,
    SHOW_OPTIMIZATION,          "-showopt",         arg_show_optimization,          false,
    SHOW_LOWERED,               "-showlower",       arg_show_lowered,               false,
    SHOW_CLEANED,               "-showclean",       arg_show_cleaned,               false
);