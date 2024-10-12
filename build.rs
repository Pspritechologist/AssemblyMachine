fn main() {
	lalrpop::Configuration::default()
		.generate_in_source_tree()
		.process_file("src/nonsembly/grammar.lalrpop")
		.expect("Could not generate parser");
}
