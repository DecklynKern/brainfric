macro_rules! create_flags {
    ($($flag_name: ident, $flag_arg: literal, $flag_func_name: ident, $default: literal),*) => {

        $(static mut $flag_name: bool = $default;)*

        pub fn parse_args() {

            let args: Box<[String]> = std::env::args().collect();
            
            if args.len() < 2 {
                panic!("No input file provided");
            }

            unsafe {

                INPUT_FILE = args[1].clone();
                OUTPUT_FILE = match args.iter().position(|arg| arg == "-out") {
                    Some(position) => args[position + 1].clone(),
                    None => "out.bf".to_string()
                };

                $(
                    $flag_name = $default ^ args.contains(&String::from($flag_arg));
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
    DO_OPTIMIZATION,            "-noopt",           arg_do_optimization,            true,
    ALLOW_DELETE_VARIABLES,     "-allowdelvars",    arg_allow_delete_variables,     false,
    SHOW_LEX,                   "-showlex",         arg_show_lex,                   false,
    SHOW_PARSE,                 "-showparse",       arg_show_parse,                 false,
    SHOW_ELABORATION,           "-showelaboration",     arg_show_elaborated_program,    false,
    SHOW_IR,                    "-showir",          arg_show_ir,                    false,
    SHOW_OPTIMIZATION_STEPS,    "-showoptsteps",    arg_show_optimization_steps,    false,
    SHOW_OPTIMIZATION,          "-showopt",         arg_show_optimization,          false,
    SHOW_LOWERED,               "-showlower",       arg_show_lowered,               false,
    SHOW_CLEANED,               "-showclean",       arg_show_cleaned,               false
);

static mut INPUT_FILE: String = String::new();
static mut OUTPUT_FILE: String = String::new();

pub fn arg_input_file() -> &'static str {
    unsafe {
        &INPUT_FILE
    }
}

pub fn arg_output_file() -> &'static str {
    unsafe {
        &OUTPUT_FILE
    }
}