pub static mut DO_OPTIMIZATION: bool = true;
pub static mut PRINT_OPTIMIZATION_STEPS: bool = false;

macro_rules! parse_arg {
    ($var: ident, $flag: literal, $args: ident) => {
        $var = $args.contains(&String::from($flag));
    };
}

macro_rules! parse_arg_neg {
    ($var: ident, $flag: literal, $args: ident) => {
        parse_arg!($var, $flag, $args);
        $var = !$var;
    };
}

pub fn parse_args() {

    let args: Vec<_> = std::env::args().collect();

    unsafe {
        parse_arg_neg!(DO_OPTIMIZATION, "-noopt", args);
        parse_arg!(PRINT_OPTIMIZATION_STEPS, "-printopt", args);
    }
}

pub fn arg_do_optimization() -> bool {
    unsafe {
        DO_OPTIMIZATION
    }
}

pub fn arg_show_optimization_steps() -> bool {
    unsafe {
        PRINT_OPTIMIZATION_STEPS
    }
}