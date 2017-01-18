extern crate gamma_lut;

fn main() {
    let contrast = 0.0;
    let gamma = 0.0;

    let table = gamma_lut::PreblendLut::new_default_color(contrast, gamma, gamma);
    table.print_table();
}
