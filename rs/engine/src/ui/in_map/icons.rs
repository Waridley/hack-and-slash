use crate::ui::{
	a11y::AKNode,
	in_map::{icons::kenney::generic_base_dir, GamepadSeries},
	widgets::{Font3d, TextBuilder},
	TextMeshCache, GLOBAL_UI_RENDER_LAYERS,
};
use bevy::{
	a11y::accesskit::{NodeBuilder, Role},
	asset::AssetPath,
	input::keyboard::Key,
	prelude::*,
	render::view::RenderLayers,
};
use bevy_svg::prelude::{Origin, Svg3dBundle};
use kenney::{base_dir, kb_mouse_base_dir};
use leafwing_input_manager::{
	axislike::{AxisType, AxisType::Gamepad, MouseMotionAxisType},
	buttonlike::{MouseMotionDirection, MouseWheelDirection},
	prelude::{Modifier as ModifierKey, MouseWheelAxisType, UserInput},
	user_input::InputKind,
};
use smol_str::SmolStr;
use std::path::PathBuf;

pub fn default_icon() -> AssetPath<'static> {
	base_dir().join("Flairs/Vector/flair_disabled.svg").into()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
#[reflect(no_field_bounds)]
pub enum UserInputIcons {
	Single(InputIcons),
	Chord(Vec<InputIcons>),
	// Variants are too large without indirection, but `Box` won't derive Reflect
	// See https://github.com/bevyengine/bevy/issues/10393
	VirtualDPad(std::sync::Arc<VirtualDPadIcons>),
	VirtualAxis(std::sync::Arc<VirtualAxisIcons>),
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, Reflect)]
pub struct VirtualDPadIcons {
	pub up: InputIcons,
	pub down: InputIcons,
	pub left: InputIcons,
	pub right: InputIcons,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, Reflect)]
pub struct VirtualAxisIcons {
	pub positive: InputIcons,
	pub negative: InputIcons,
}

impl UserInputIcons {
	pub fn from_user_input(input: UserInput, gp: Option<GamepadSeries>) -> UserInputIcons {
		match input {
			UserInput::Single(kind) => {
				UserInputIcons::Single(InputIcons::from_input_kind(kind, gp).unwrap_or_default())
			}
			UserInput::Chord(kinds) => UserInputIcons::Chord(
				kinds
					.into_iter()
					.map(|kind| InputIcons::from_input_kind(kind, gp).unwrap_or_default())
					.collect(),
			),
			UserInput::VirtualDPad(dpad) => UserInputIcons::VirtualDPad(
				VirtualDPadIcons {
					up: InputIcons::from_input_kind(dpad.up, gp).unwrap_or_default(),
					down: InputIcons::from_input_kind(dpad.down, gp).unwrap_or_default(),
					left: InputIcons::from_input_kind(dpad.left, gp).unwrap_or_default(),
					right: InputIcons::from_input_kind(dpad.right, gp).unwrap_or_default(),
				}
				.into(),
			),
			UserInput::VirtualAxis(axis) => UserInputIcons::VirtualAxis(
				VirtualAxisIcons {
					positive: InputIcons::from_input_kind(axis.positive, gp).unwrap_or_default(),
					negative: InputIcons::from_input_kind(axis.negative, gp).unwrap_or_default(),
				}
				.into(),
			),
		}
	}
}

impl Default for UserInputIcons {
	fn default() -> Self {
		Self::Single(default())
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
pub enum InputIcons {
	Single(Icon),
	DualAxis { horizontal: Icon, vertical: Icon },
}

impl InputIcons {
	pub fn from_input_kind(kind: InputKind, gp: Option<GamepadSeries>) -> Option<Self> {
		use InputKind::*;
		let icons = match kind {
			GamepadButton(btn) => {
				Self::Single(Icon::from_path(gp.unwrap_or_default().button(btn)?))
			}
			SingleAxis(axis) => Self::Single(Icon::from_axis_type(axis.axis_type, gp)),
			DualAxis(axes) => Self::from_dual_axis_types(axes.x.axis_type, axes.y.axis_type, gp),
			PhysicalKey(k) => Self::Single(key_code(k)?),
			Modifier(modifier) => Self::Single(
				key(match modifier {
					ModifierKey::Alt => &Key::Alt,
					ModifierKey::Control => &Key::Control,
					ModifierKey::Shift => &Key::Shift,
					ModifierKey::Super => &Key::Super,
				})
				.expect("icons exist for all modifiers"),
			),
			Mouse(btn) => Self::Single(Icon::from_path(mouse_button(btn))),
			MouseWheel(dir) => Self::Single(Icon::from_path(scroll(dir))),
			MouseMotion(dir) => Self::Single(Icon::from_path(mouse_direction(dir))),
			other => {
				error!("unknown InputKind: {other:?}");
				return None;
			}
		};
		Some(icons)
	}

	pub fn from_dual_axis_types(x: AxisType, y: AxisType, gp: Option<GamepadSeries>) -> Self {
		use AxisType::*;
		use GamepadAxisType::*;
		match (x, y) {
			(Gamepad(LeftStickX), Gamepad(LeftStickY))
			| (Gamepad(LeftStickY), Gamepad(LeftStickX)) => Self::gamepad_left_stick(gp.unwrap_or_default()),
			(Gamepad(RightStickX), Gamepad(RightStickY))
			| (Gamepad(RightStickY), Gamepad(RightStickX)) => {
				Self::gamepad_right_stick(gp.unwrap_or_default())
			}
			(MouseWheel(_), MouseWheel(_)) => Self::mouse_wheel(),
			(MouseMotion(_), MouseMotion(_)) => Self::mouse_motion(),
			(x, y) => Self::DualAxis {
				horizontal: Icon::from_axis_type(x, gp),
				vertical: Icon::from_axis_type(y, gp),
			},
		}
	}

	pub fn gamepad_left_stick(series: GamepadSeries) -> Self {
		Self::Single(Icon::from_path(series.left_stick()))
	}

	pub fn gamepad_right_stick(series: GamepadSeries) -> Self {
		Self::Single(Icon::from_path(series.right_stick()))
	}

	pub fn mouse_wheel() -> Self {
		Self::Single(Icon::from_path(kenney::mouse_wheel()))
	}

	pub fn mouse_motion() -> Self {
		Self::Single(Icon::from_path(kenney::mouse_motion()))
	}
}

impl Default for InputIcons {
	fn default() -> Self {
		Self::Single(default())
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
pub struct Icon {
	pub image: AssetPath<'static>,
	pub text: Option<SmolStr>,
}

impl Icon {
	pub fn from_path(path: impl Into<AssetPath<'static>>) -> Self {
		Self {
			image: path.into(),
			text: None,
		}
	}

	pub fn from_axis_type(axis: AxisType, gp_name: Option<GamepadSeries>) -> Self {
		match axis {
			Gamepad(gp) => Self::gamepad(gp, gp_name.unwrap_or_default()),
			AxisType::MouseWheel(wheel) => Self::mouse_wheel(wheel),
			AxisType::MouseMotion(motion) => Self::mouse_motion(motion),
		}
	}

	pub fn gamepad(gp: GamepadAxisType, series: GamepadSeries) -> Self {
		Self::from_path(series.axis(gp).unwrap_or_else(default_icon))
	}

	pub fn mouse_wheel(wheel: MouseWheelAxisType) -> Self {
		Self::from_path(match wheel {
			MouseWheelAxisType::X => kenney::mouse_wheel_horizontal(),
			MouseWheelAxisType::Y => kenney::mouse_wheel_vertical(),
		})
	}

	pub fn mouse_motion(motion: MouseMotionAxisType) -> Self {
		Self::from_path(match motion {
			MouseMotionAxisType::X => kenney::mouse_motion_horizontal(),
			MouseMotionAxisType::Y => kenney::mouse_motion_vertical(),
		})
	}
}

impl Default for Icon {
	fn default() -> Self {
		Self {
			image: default_icon(),
			text: None,
		}
	}
}

#[derive(Debug, Clone)]
pub struct IconBundleBuilder {
	pub icon: Icon,
	pub font: Handle<Font3d>,
	pub origin: Origin,
	pub size: Vec3,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub layers: RenderLayers,
}

impl Default for IconBundleBuilder {
	fn default() -> Self {
		Self {
			icon: default(),
			font: default(),
			origin: default(),
			size: Vec3::ONE,
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}

#[derive(Bundle)]
pub struct IconBundle {
	pub svg: Svg3dBundle,
	pub layers: RenderLayers,
	pub role: AKNode,
}

impl IconBundleBuilder {
	pub fn build(
		self,
		asset_server: &AssetServer,
		meshes: Mut<Assets<Mesh>>,
		cache: Mut<TextMeshCache>,
		fonts: Mut<Assets<Font3d>>,
	) -> (IconBundle, Option<impl Bundle>) {
		let Self {
			icon: Icon { image, text },
			font,
			origin,
			size,
			mut transform,
			mut global_transform,
			visibility,
			inherited_visibility,
			layers,
		} = self;
		// Compensate for lack of z-up convertibility in bevy_svg
		let rot = Quat::from_rotation_x(std::f32::consts::FRAC_PI_2);
		transform.rotation *= rot;
		global_transform = global_transform * Transform::from_rotation(rot);
		let svg = Svg3dBundle {
			svg: asset_server.load(image),
			origin,
			transform,
			global_transform,
			visibility,
			inherited_visibility,
			..default()
		};

		let text = text.and_then(|text| {
			TextBuilder {
				text: text.to_string().into(),
				font,
				vertex_translation: Vec3::new(-8.0, -40.0, -32.0),
				vertex_scale: Vec3::new(size.x * 12.0, size.y * 12.0, 0.0),
				// Compensate for lack of z-up convertibility in bevy_svg
				vertex_rotation: Quat::default(),
				..default()
			}
			.build(meshes, cache, fonts)
		});

		let role = NodeBuilder::new(Role::Image).into();
		(IconBundle { svg, layers, role }, text)
	}
}

pub fn key(key: &Key) -> Option<Icon> {
	let Some(kenney_name) = kenney::key_name(key) else {
		return if let Key::Character(c) = key {
			Some(Icon {
				image: generic_base_dir()
					.join("generic_button_square_outline.svg")
					.into(),
				text: Some(c.clone()),
			})
		} else {
			None
		};
	};
	Some(Icon::from_path(
		kb_mouse_base_dir().join(format!("keyboard_{}_outline.svg", kenney_name)),
	))
}

pub fn key_code(key: KeyCode) -> Option<Icon> {
	let kenney_name = kenney::key_code_name(key)?;
	Some(Icon::from_path(
		kb_mouse_base_dir().join(format!("keyboard_{}_outline.svg", kenney_name)),
	))
}

pub fn mouse_button(btn: MouseButton) -> AssetPath<'static> {
	let s = match btn {
		MouseButton::Left => "mouse_left_outline.svg",
		MouseButton::Right => "mouse_right_outline.svg",
		MouseButton::Middle => "mouse_scroll_outline.svg",
		MouseButton::Back | // TODO: I might need to make these myself
		MouseButton::Forward |
		MouseButton::Other(_) => "mouse_outline.svg"
	};
	kb_mouse_base_dir().join(s).into()
}

pub fn mouse_direction(motion: MouseMotionDirection) -> AssetPath<'static> {
	let s = match motion {
		MouseMotionDirection::Up | MouseMotionDirection::Down => "mouse_vertical.svg",
		MouseMotionDirection::Right | MouseMotionDirection::Left => "mouse_horizontal.svg",
	};
	kb_mouse_base_dir().join(s).into()
}

pub fn mouse_motion() -> AssetPath<'static> {
	kb_mouse_base_dir().join("mouse_move.svg").into()
}

pub fn scroll(direction: MouseWheelDirection) -> AssetPath<'static> {
	let s = match direction {
		MouseWheelDirection::Up => "mouse_scroll_up_outline.svg",
		MouseWheelDirection::Down => "mouse_scroll_down_outline.svg",
		MouseWheelDirection::Right => "mouse_scroll_right_outline.svg",
		MouseWheelDirection::Left => "mouse_scroll_left_outline.svg",
	};
	kb_mouse_base_dir().join(s).into()
}

pub fn scroll_direction(axis: MouseWheelAxisType) -> AssetPath<'static> {
	let s = match axis {
		MouseWheelAxisType::X => "mouse_scroll_horizontal_outline.svg",
		MouseWheelAxisType::Y => "mouse_scroll_vertical_outline.svg",
	};
	kb_mouse_base_dir().join(s).into()
}

impl GamepadSeries {
	pub fn dir_name(self) -> &'static str {
		match self {
			GamepadSeries::Generic => "Generic",
			GamepadSeries::Nintendo => "Nintendo Switch",
			GamepadSeries::PlayStation => "PlayStation Series",
			GamepadSeries::SteamController => "Steam Controller",
			GamepadSeries::SteamDeck => "Steam Deck",
			GamepadSeries::Xbox => "Xbox Series",
		}
	}

	pub fn dir(self) -> PathBuf {
		base_dir().join(self.dir_name()).join("Vector")
	}

	pub fn button(self, btn: GamepadButtonType) -> Option<AssetPath<'static>> {
		match self {
			Self::Generic => kenney::generic_gamepad_button(btn),
			Self::Nintendo => kenney::nintendo_button(btn),
			Self::PlayStation => kenney::ps_button(btn),
			Self::Xbox => kenney::xb_button(btn),
			// TODO
			Self::SteamController => kenney::generic_gamepad_button(btn),
			Self::SteamDeck => kenney::generic_gamepad_button(btn),
		}
	}

	pub fn axis(self, axis: GamepadAxisType) -> Option<AssetPath<'static>> {
		match self {
			Self::Generic | Self::Nintendo => kenney::nintendo_axis(axis),
			Self::PlayStation => kenney::ps_axis(axis),
			Self::Xbox => kenney::xb_axis(axis),
			// TODO
			Self::SteamController => kenney::nintendo_axis(axis),
			Self::SteamDeck => kenney::nintendo_axis(axis),
		}
	}

	pub fn left_stick(self) -> AssetPath<'static> {
		match self {
			Self::Generic | Self::Nintendo => {
				Self::Nintendo.dir().join("nintendo_stick_l.svg").into()
			}
			Self::PlayStation => Self::PlayStation
				.dir()
				.join("playstation_stick_l.svg")
				.into(),
			Self::SteamController => Self::SteamController.dir().join("steam_stick_l.svg").into(),
			Self::SteamDeck => Self::SteamDeck.dir().join("steamdeck_stick_l.svg").into(),
			Self::Xbox => Self::Xbox.dir().join("xbox_stick_l.svg").into(),
		}
	}

	pub fn right_stick(self) -> AssetPath<'static> {
		match self {
			Self::Generic | Self::Nintendo => {
				Self::Nintendo.dir().join("nintendo_stick_r.svg").into()
			}
			Self::PlayStation => Self::PlayStation
				.dir()
				.join("playstation_stick_r.svg")
				.into(),
			Self::SteamController => Self::SteamController.dir().join("steam_stick_r.svg").into(),
			Self::SteamDeck => Self::SteamDeck.dir().join("steamdeck_stick_r.svg").into(),
			Self::Xbox => Self::Xbox.dir().join("xbox_stick_r.svg").into(),
		}
	}
}

pub mod kenney {
	use super::super::GamepadSeries;
	use crate::util;
	use bevy::{
		asset::AssetPath,
		input::keyboard::Key,
		log::error,
		prelude::{GamepadAxisType, GamepadButtonType, KeyCode},
	};
	use smol_str::SmolStr;
	use std::path::PathBuf;

	pub fn base_dir() -> PathBuf {
		PathBuf::from("ui/Kenney/input-prompts")
	}

	pub fn kb_mouse_base_dir() -> PathBuf {
		base_dir().join("Keyboard & Mouse/Vector")
	}

	pub fn generic_base_dir() -> PathBuf {
		base_dir().join("Generic/Vector")
	}

	pub fn key_name(key: &Key) -> Option<SmolStr> {
		use Key::*;
		let s: &'static str = match key {
			Character(c) => return rename_char(c.clone()),
			Dead(c) => return c.map(std::iter::once).map(SmolStr::from_iter),
			Alt | AltGraph => "alt",
			CapsLock => "capslock",
			Control => "ctrl",
			Fn | FnLock => "function",
			NumLock => "numlock",
			Shift => "shift_icon",
			Meta | Hyper | Super => platform_meta()?,
			Enter => "return", // icon version of "enter"
			Tab => "tab_icon_alternative",
			Space => "space",
			ArrowDown => "arrow_down",
			ArrowLeft => "arrow_left",
			ArrowRight => "arrow_right",
			ArrowUp => "arrow_up",
			End => "end",
			Home => "home",
			PageDown => "page_down",
			PageUp => "page_up",
			Backspace => {
				if util::host_is_mac() {
					"backspace_icon_alternative"
				} else {
					"backspace_icon"
				}
			}
			Delete => "delete",
			Insert => "insert",
			Escape => "escape",
			PrintScreen => "printscreen",
			AllCandidates => "any", // Does this make sense??
			NextCandidate => "tab_icon_alternative",
			F1 => "f1",
			F2 => "f2",
			F3 => "f3",
			F4 => "f4",
			F5 => "f5",
			F6 => "f6",
			F7 => "f7",
			F8 => "f8",
			F9 => "f9",
			F10 => "f10",
			F11 => "f11",
			F12 => "f12",
			_ => return None,
		};
		Some(s.into())
	}

	pub fn key_code_name(key: KeyCode) -> Option<&'static str> {
		let s: &'static str = match key {
			KeyCode::Backquote => "tilde",
			KeyCode::Backslash => "slash_back",
			KeyCode::BracketLeft => "bracket_open",
			KeyCode::BracketRight => "bracket_close",
			KeyCode::Comma => "comma",
			KeyCode::Digit0 => "0",
			KeyCode::Digit1 => "1",
			KeyCode::Digit2 => "2",
			KeyCode::Digit3 => "3",
			KeyCode::Digit4 => "4",
			KeyCode::Digit5 => "5",
			KeyCode::Digit6 => "6",
			KeyCode::Digit7 => "7",
			KeyCode::Digit8 => "8",
			KeyCode::Digit9 => "9",
			KeyCode::Equal => "equals",
			KeyCode::IntlBackslash | KeyCode::IntlRo | KeyCode::IntlYen => "slash_back", // I guess use the Russian one
			KeyCode::KeyA => "a",
			KeyCode::KeyB => "b",
			KeyCode::KeyC => "c",
			KeyCode::KeyD => "d",
			KeyCode::KeyE => "e",
			KeyCode::KeyF => "f",
			KeyCode::KeyG => "g",
			KeyCode::KeyH => "h",
			KeyCode::KeyI => "i",
			KeyCode::KeyJ => "j",
			KeyCode::KeyK => "k",
			KeyCode::KeyL => "l",
			KeyCode::KeyM => "m",
			KeyCode::KeyN => "n",
			KeyCode::KeyO => "o",
			KeyCode::KeyP => "p",
			KeyCode::KeyQ => "q",
			KeyCode::KeyR => "r",
			KeyCode::KeyS => "s",
			KeyCode::KeyT => "t",
			KeyCode::KeyU => "u",
			KeyCode::KeyV => "v",
			KeyCode::KeyW => "w",
			KeyCode::KeyX => "x",
			KeyCode::KeyY => "y",
			KeyCode::KeyZ => "z",
			KeyCode::Minus => "minus",
			KeyCode::Period => "period",
			KeyCode::Quote => "apostrophe",
			KeyCode::Semicolon => "semicolon",
			KeyCode::Slash => "slash_forward",
			KeyCode::AltLeft | KeyCode::AltRight => platform_alt(),
			KeyCode::Backspace => platform_backspace(),
			KeyCode::CapsLock => "capslock",
			KeyCode::ControlLeft | KeyCode::ControlRight => "ctrl",
			KeyCode::Enter => "return",
			KeyCode::SuperLeft | KeyCode::SuperRight => return platform_meta(),
			KeyCode::ShiftLeft | KeyCode::ShiftRight => "shift_icon",
			KeyCode::Space => "space_icon",
			KeyCode::Tab => "tab_icon_alternative",
			KeyCode::Delete => "delete",
			KeyCode::End => "end",
			KeyCode::Home => "home",
			KeyCode::Insert => "insert",
			KeyCode::PageDown => "page_down",
			KeyCode::PageUp => "page_up",
			KeyCode::ArrowDown => "arrow_down",
			KeyCode::ArrowLeft => "arrow_left",
			KeyCode::ArrowRight => "arrow_right",
			KeyCode::ArrowUp => "arrow_up",
			KeyCode::NumLock => "numlock",
			KeyCode::Numpad0 => "0",
			KeyCode::Numpad1 => "1",
			KeyCode::Numpad2 => "2",
			KeyCode::Numpad3 => "3",
			KeyCode::Numpad4 => "4",
			KeyCode::Numpad5 => "5",
			KeyCode::Numpad6 => "6",
			KeyCode::Numpad7 => "7",
			KeyCode::Numpad8 => "8",
			KeyCode::Numpad9 => "9",
			KeyCode::NumpadAdd => "numpad_plus",
			KeyCode::NumpadBackspace => platform_backspace(),
			KeyCode::NumpadClear => "a", // Slightly hacky but better than nothing
			KeyCode::NumpadClearEntry => "c",
			KeyCode::NumpadComma => "comma",
			KeyCode::NumpadDecimal => "period",
			KeyCode::NumpadDivide => "slash_forward",
			KeyCode::NumpadEnter => "numpad_enter",
			KeyCode::NumpadEqual => "equals",
			KeyCode::Escape => "escape",
			KeyCode::Fn => "function",
			KeyCode::PrintScreen => "printscreen",
			KeyCode::Meta => return platform_meta(),
			KeyCode::F1 => "f1",
			KeyCode::F2 => "f2",
			KeyCode::F3 => "f3",
			KeyCode::F4 => "f4",
			KeyCode::F5 => "f5",
			KeyCode::F6 => "f6",
			KeyCode::F7 => "f7",
			KeyCode::F8 => "f8",
			KeyCode::F9 => "f9",
			KeyCode::F10 => "f10",
			KeyCode::F11 => "f11",
			KeyCode::F12 => "f12",
			other => {
				error!("No icon for {other:?}");
				return None;
			}
		};
		Some(s)
	}

	pub fn rename_char(winit_char: SmolStr) -> Option<SmolStr> {
		let s: &'static str = match &*winit_char {
			"a" | "A" => "a",
			"b" | "B" => "b",
			"c" | "C" => "c",
			"d" | "D" => "d",
			"e" | "E" => "e",
			"f" | "F" => "f",
			"g" | "G" => "g",
			"h" | "H" => "h",
			"i" | "I" => "i",
			"j" | "J" => "j",
			"k" | "K" => "k",
			"l" | "L" => "l",
			"m" | "M" => "m",
			"n" | "N" => "n",
			"o" | "O" => "o",
			"p" | "P" => "p",
			"q" | "Q" => "q",
			"r" | "R" => "r",
			"s" | "S" => "s",
			"t" | "T" => "t",
			"u" | "U" => "u",
			"v" | "V" => "v",
			"w" | "W" => "w",
			"x" | "X" => "x",
			"y" | "Y" => "y",
			"z" | "Z" => "z",
			"0" => "0",
			"1" => "1",
			"2" => "2",
			"3" => "3",
			"4" => "4",
			"5" => "5",
			"6" => "6",
			"7" => "7",
			"8" => "8",
			"9" => "9",
			"'" => "apostrophe",
			"*" => "asterisk",
			"[" => "bracket_open",
			"]" => "bracket_close",
			">" => "bracket_greater",
			"<" => "bracket_less",
			"^" => "caret",
			":" => "colon",
			"," => "comma",
			"=" => "equals",
			"!" => "exclamation",
			"-" => "minus",
			"." => "period",
			"+" => "plus",
			"?" => "question",
			"\"" => "quote",
			";" => "semicolon",
			"\\" => "slash_back",
			"/" => "slash_forward",
			"`" | // Kenney has no backtick icon
			"~" => "tilde",

			// I think these are separate `Key`s, but just in case
			" " => "space",
			"\t" => "tab_icon_alternative",

			_ => return None,
		};
		Some(SmolStr::new_static(s))
	}

	pub fn platform_alt() -> &'static str {
		if util::host_is_mac() {
			"option"
		} else {
			"alt"
		}
	}

	pub fn platform_meta() -> Option<&'static str> {
		if cfg!(target_os = "windows") {
			Some("win")
		} else if util::host_is_mac() {
			Some("command")
		} else {
			// TODO: Find a "Super" icon for Linux?
			None
		}
	}

	pub fn platform_backspace() -> &'static str {
		if util::host_is_mac() {
			"backspace_icon_alternative"
		} else {
			"backspace_icon"
		}
	}

	pub fn mouse_motion() -> AssetPath<'static> {
		kb_mouse_base_dir().join("mouse_move.svg").into() // TODO: Outline version
	}

	pub fn mouse_motion_horizontal() -> AssetPath<'static> {
		kb_mouse_base_dir().join("mouse_horizontal.svg").into() // TODO: Outline version
	}

	pub fn mouse_motion_vertical() -> AssetPath<'static> {
		kb_mouse_base_dir().join("mouse_vertical.svg").into() // TODO: Outline version
	}

	pub fn mouse_wheel() -> AssetPath<'static> {
		kb_mouse_base_dir().join("mouse_scroll_outline.svg").into()
	}

	pub fn mouse_wheel_horizontal() -> AssetPath<'static> {
		// TODO: Horizontal wheel icons
		kb_mouse_base_dir().join("mouse_scroll.svg").into()
	}

	pub fn mouse_wheel_vertical() -> AssetPath<'static> {
		kb_mouse_base_dir()
			.join("mouse_scroll_vertical_outline.svg")
			.into()
	}

	pub fn mouse_wheel_up() -> AssetPath<'static> {
		kb_mouse_base_dir()
			.join("mouse_scroll_up_outline.svg")
			.into()
	}

	pub fn mouse_wheel_down() -> AssetPath<'static> {
		kb_mouse_base_dir()
			.join("mouse_scroll_down_outline.svg")
			.into()
	}

	pub fn generic_gamepad_button(btn: GamepadButtonType) -> Option<AssetPath<'static>> {
		use GamepadButtonType::*;
		let dir = GamepadSeries::Generic.dir();
		let switch_dir = GamepadSeries::Nintendo.dir();
		let ps_dir = GamepadSeries::PlayStation.dir();
		let xb_dir = GamepadSeries::Xbox.dir();
		let path = match btn {
			South => switch_dir.join("switch_buttons_down_outline.svg"),
			East => switch_dir.join("switch_buttons_right_outline.svg"),
			North => switch_dir.join("switch_buttons_up_outline.svg"),
			West => switch_dir.join("switch_buttons_left_outline.svg"),
			C => dir.join("generic_button_trigger_c_outline.svg"), // Kinda looks like the C button
			Z => kb_mouse_base_dir().join("keyboard_z.svg"),
			LeftTrigger => ps_dir.join("playstation_trigger_l1_outline.svg"),
			LeftTrigger2 => ps_dir.join("playstation_trigger_l2_outline.svg"),
			RightTrigger => ps_dir.join("playstation_trigger_r1_outline.svg"),
			RightTrigger2 => ps_dir.join("playstation_trigger_r2_outline.svg"),
			Select => xb_dir.join("xbox_button_view_outline.svg"),
			Start => xb_dir.join("xbox_button_start_outline.svg"),
			Mode => dir.join("generic_button_circle_outline.svg"),
			LeftThumb => switch_dir.join("switch_stick_side_l.svg"),
			RightThumb => switch_dir.join("switch_stick_side_r.svg"),
			DPadUp => switch_dir.join("switch_dpad_up_outline.svg"),
			DPadDown => switch_dir.join("switch_dpad_down_outline.svg"),
			DPadLeft => switch_dir.join("switch_dpad_left_outline.svg"),
			DPadRight => switch_dir.join("switch_dpad_right_outline.svg"),
			Other(_) => return None,
		};
		Some(path.into())
	}

	pub fn nintendo_button(btn: GamepadButtonType) -> Option<AssetPath<'static>> {
		use GamepadButtonType::*;
		let dir = GamepadSeries::Nintendo.dir();
		let file = match btn {
			South => "switch_button_b_outline.svg",
			East => "switch_button_a_outline.svg",
			North => "switch_button_x_outline.svg",
			West => "switch_button_y_outline.svg",
			LeftTrigger => "switch_button_l_outline.svg",
			LeftTrigger2 => "switch_button_zl_outline.svg",
			RightTrigger => "switch_button_r_outline.svg",
			RightTrigger2 => "switch_button_zr_outline.svg",
			Select => "switch_button_minus_outline.svg",
			Start => "switch_button_plus_outline.svg",
			LeftThumb => "switch_stick_side_l.svg",
			RightThumb => "switch_stick_side_r.svg",
			DPadUp => "switch_dpad_up_outline.svg",
			DPadDown => "switch_dpad_down_outline.svg",
			DPadLeft => "switch_dpad_left_outline.svg",
			DPadRight => "switch_dpad_right_outline.svg",
			_ => return None,
		};
		Some(dir.join(file).into())
	}

	pub fn nintendo_axis(axis: GamepadAxisType) -> Option<AssetPath<'static>> {
		Some(
			GamepadSeries::Nintendo
				.dir()
				.join(match axis {
					GamepadAxisType::LeftStickX => "switch_stick_l_horizontal.svg",
					GamepadAxisType::LeftStickY => "switch_stick_l_vertical.svg",
					GamepadAxisType::LeftZ => "switch_sick_side_l.svg",
					GamepadAxisType::RightStickX => "switch_stick_r_horizontal.svg",
					GamepadAxisType::RightStickY => "switch_stick_r_vertical.svg",
					GamepadAxisType::RightZ => "switch_stick_side_r.svg",
					// TODO: Motion
					GamepadAxisType::Other(_) => return None,
				})
				.into(),
		)
	}

	pub fn ps_button(btn: GamepadButtonType) -> Option<AssetPath<'static>> {
		use GamepadButtonType::*;
		let dir = GamepadSeries::PlayStation.dir();
		let file = match btn {
			South => "playstation_button_color_cross_outline.svg",
			East => "playstation_button_color_circle_outline.svg",
			North => "playstation_button_color_triangle_outline.svg",
			West => "playstation_button_color_square_outline.svg",
			LeftTrigger => "playstation_trigger_l1_outline.svg",
			LeftTrigger2 => "playstation_trigger_l2_outline.svg",
			RightTrigger => "playstation_trigger_r1_outline.svg",
			RightTrigger2 => "playstation_trigger_r2_outline.svg",
			Select => "playstation5_button_create_outline.svg",
			Start => "playstation5_button_options_outline.svg",
			LeftThumb => "playstation_stick_side_l.svg",
			RightThumb => "playstation_stick_side_r.svg",
			DPadUp => "playstation_dpad_up_outline.svg",
			DPadDown => "playstation_dpad_down_outline.svg",
			DPadLeft => "playstation_dpad_left_outline.svg",
			DPadRight => "playstation_dpad_right_outline.svg",
			// TODO: touchpad, Mode, etc.
			_ => return None,
		};
		Some(dir.join(file).into())
	}

	pub fn ps_axis(axis: GamepadAxisType) -> Option<AssetPath<'static>> {
		use GamepadAxisType::*;
		let dir = GamepadSeries::PlayStation.dir();
		let file = match axis {
			LeftStickX => "playstation_stick_l_horizontal.svg",
			LeftStickY => "playstation_stick_l_vertical.svg",
			LeftZ => "playstation_stick_side_l.svg",
			RightStickX => "playstation_stick_r_horizontal.svg",
			RightStickY => "playstation_stick_r_vertical.svg",
			RightZ => "playstation_stick_side_r.svg",
			// TODO: touchpad, motion, etc.
			Other(_) => return None,
		};
		Some(dir.join(file).into())
	}

	pub fn xb_button(btn: GamepadButtonType) -> Option<AssetPath<'static>> {
		use GamepadButtonType::*;
		let dir = GamepadSeries::Xbox.dir();
		let file = match btn {
			South => "xbox_button_color_a_outline.svg",
			East => "xbox_button_color_b_outline.svg",
			North => "xbox_button_color_y_outline.svg",
			West => "xbox_button_color_x_outline.svg",
			LeftTrigger => "xbox_lb_outline.svg",
			LeftTrigger2 => "xbox_lt_outline.svg",
			RightTrigger => "xbox_rb_outline.svg",
			RightTrigger2 => "xbox_rt_outline.svg",
			Select => "xbox_button_view_outline.svg",
			Start => "xbox_button_menu_outline.svg",
			Mode => "xbox_guide.svg",
			LeftThumb => "xbox_stick_side_l.svg",
			RightThumb => "xbox_stick_side_r.svg",
			DPadUp => "xbox_dpad_up_outline.svg",
			DPadDown => "xbox_dpad_down_outline.svg",
			DPadLeft => "xbox_dpad_left_outline.svg",
			DPadRight => "xbox_dpad_right_outline.svg",
			_ => return None,
		};
		Some(dir.join(file).into())
	}

	pub fn xb_axis(axis: GamepadAxisType) -> Option<AssetPath<'static>> {
		use GamepadAxisType::*;
		let dir = GamepadSeries::Xbox.dir();
		let file = match axis {
			LeftStickX => "xbox_stick_l_horizontal.svg",
			LeftStickY => "xbox_stick_l_vertical.svg",
			LeftZ => "xbox_stick_side_l.svg",
			RightStickX => "xbox_stick_r_horizontal.svg",
			RightStickY => "xbox_stick_r_vertical.svg",
			RightZ => "xbox_stick_side_r.svg",
			// TODO: touchpad, motion, etc.
			Other(_) => return None,
		};
		Some(dir.join(file).into())
	}
}
