use bevy::app::AppExit;
use sond_has_engine::testing::visual::run_review_visual_tests;

pub fn main() -> AppExit {
	if std::env::var_os("VIS_TEST_REVIEW").is_none() {
		// `cargo test` runs every test target. Don't open windows unless the
		// review mode was explicitly requested.
		return AppExit::Success;
	}

	run_review_visual_tests()
}
