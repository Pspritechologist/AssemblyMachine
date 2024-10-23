fn main() {
	// divan::main();
}

// #[divan::bench(args = [1000, 10_000, 100_000], sample_size = 10, sample_count = 10)]
// fn assembly(n: i64) -> Result<(), ()> {
// 	use assembly_machine::Operation::*;

// 	let vm = assembly_machine::VM::<2048>::new(&const { [
// 		Push { dst: 0, src: 0 },
// 		Push { dst: 1, src: 1 },
// 		Push { dst: 7, src: 2 },
// 		Push { dst: 8, src: 3 },
// 		Push { dst: 9, src: 4 },

// 		// Rand { dst: 2 },
// 		// Mod { dst: 2, a: 2, b: 0 },
// 		RandR { dst: 2, max: 0, min: 9 },
// 		// Rand { dst: 3 },
// 		// Mod { dst: 3, a: 3, b: 1 },
// 		RandR { dst: 3, max: 1, min: 9 },
// 		Add { dst: 2, a: 2, b: 3 },
// 		Print { src: 2 },

// 		Sub { dst: 7, a: 7, b: 8 },

// 		TBJmp { tst: 7, off: 5 },
// 	] }, vec![ 50.into(), 27.into(), n.into(), 1.into(), 0.into() ]);

// 	vm.run()
// }

// // #[divan::bench(args = [1000, 10_000, 100_000, 1_000_000, 100_000_000], sample_size = 10, sample_count = 10)]
// // fn lua_test(n: i64) -> Result<std::process::Output, std::io::Error> {
// // 	std::process::Command::new("lua")
// // 		.arg("-e")
// // 		.arg(format!(
// // 			r"
// // 				-- Loop 'n' times.
// // 				for i = 1, {} do
// // 					n = math.random(0, 50) + math.random(0, 27)
// // 				end
// // 			",
// // 			n
// // 		))
// // 		.output()
// // }
