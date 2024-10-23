fn main() {
	lalrpop::Configuration::default()
		// .generate_in_source_tree()
		.use_cargo_dir_conventions()
		// .process_file("src/nonsembly/grammar.lalrpop")
		.process()
		.expect("Could not generate parser");
}
