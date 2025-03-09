use crate::input::map::{icons::kenney::generic_base_dir, Platform};
use bevy::{asset::AssetPath, input::keyboard::Key, prelude::*, utils::HashMap};
use leafwing_input_manager::{clashing_inputs::BasicInputs, prelude::*};
use serde::{Deserialize, Serialize};
use smallvec::{smallvec, SmallVec};
use smol_str::{SmolStr, ToSmolStr};
use std::{hash::Hash, ops::Index, path::PathBuf};
use std::any::Any;
use std::sync::Arc;
use bevy::utils::HashSet;

pub fn default_icon() -> AssetPath<'static> {
	PathBuf::from("ui/Kenney/input-prompts/Flairs/Vector/flair_circle_red_8.svg").into()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
#[reflect(no_field_bounds)]
pub enum ButtonIcons {
	Simple(Icon),
	Composite(Vec<ButtonIcons>),
	Chord(Vec<ButtonIcons>),
}

impl Default for ButtonIcons {
	fn default() -> Self {
		Self::Simple(default())
	}
}

impl ButtonIcons {
	pub fn from_button(
		btn: Box<dyn Buttonlike>,
		platform: Option<Platform>,
		map: &InputIconFileMap,
	) -> Option<Self> {
		if let Some(modifier) = <dyn Any>::downcast_ref::<ModifierKey>(&*btn) {
			// Avoid decomposing into [left + right]
			return Some(Self::Simple(Icon::from_path(map.get_button_icon(&(Box::new(match modifier {
				ModifierKey::Alt => KeyCode::AltLeft,
				ModifierKey::Control => KeyCode::ControlLeft,
				ModifierKey::Shift => KeyCode::ShiftLeft,
				ModifierKey::Super => KeyCode::SuperLeft,
			}) as Box<dyn Buttonlike>).into())?)))
		}
		
		if let Some(mouse_btn) = <dyn Any>::downcast_ref::<MouseButton>(&*btn) {
			if let MouseButton::Other(i) = mouse_btn {
				// TODO: Device icon map (this is a hack using the fact that I mapped `Back` to the mouse outline)
				if let Some(path) = map.get_button_icon(&IconPathKey(Box::new(MouseButton::Back), None)) {
					return Some(Self::Simple(Icon {
						image: path.clone(),
						text: Some(i.to_smolstr()),
					}));
				}
			}
		}
		
		Some(match btn.decompose() {
			BasicInputs::None => return None,
			BasicInputs::Simple(btn) => Self::Simple(Icon::from_path(
				map.get_button_icon(&(btn, platform).into())?,
			)),
			BasicInputs::Composite(btns) => Self::Composite(
				btns.into_iter()
					.map(|btn| Self::from_button(btn, platform, map).unwrap_or_default())
					.collect(),
			),
			BasicInputs::Chord(btns) => {
				let mut collected_modifiers = HashSet::new();
				Self::Chord(
					btns.into_iter()
						.filter_map(|btn| {
							if let Some(modifier) = <dyn Any>::downcast_ref::<KeyCode>(&*btn).map(|key| {
								match key {
									KeyCode::AltLeft | KeyCode::AltRight => Some(ModifierKey::Alt),
									KeyCode::ControlLeft | KeyCode::ControlRight => Some(ModifierKey::Control),
									KeyCode::ShiftLeft | KeyCode::ShiftRight => Some(ModifierKey::Shift),
									KeyCode::SuperLeft | KeyCode::SuperRight => Some(ModifierKey::Super),
									_ => None,
								}
							}) {
								if collected_modifiers.contains(&modifier) {
									return None
								}
								collected_modifiers.insert(modifier);
							}
							Some(Self::from_button(btn, platform, map).unwrap_or_default())
						})
						.collect(),
				)
			},
		})
	}
}

impl IntoIterator for ButtonIcons {
	type Item = Icon;
	type IntoIter = smallvec::IntoIter<[Self::Item; 1]>;
	fn into_iter(self) -> Self::IntoIter {
		match self {
			ButtonIcons::Simple(icon) => smallvec![icon].into_iter(),
			ButtonIcons::Composite(icons) | ButtonIcons::Chord(icons) => icons
				.into_iter()
				.flatten()
				.collect::<SmallVec<_>>()
				.into_iter(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
pub enum AxisIcons {
	Single(Icon),
	Composite {
		positive: ButtonIcons,
		negative: ButtonIcons,
	},
	Chord {
		button: ButtonIcons,
		axis: Arc<AxisIcons>,
	}
}

impl Default for AxisIcons {
	fn default() -> Self {
		Self::Single(default())
	}
}

impl AxisIcons {
	pub fn from_axis(
		axis: Box<dyn Axislike>,
		platform: Option<Platform>,
		map: &InputIconFileMap,
	) -> Option<Self> {
		Some(
			if let Some(path) = map.get_axis_icon(&(axis.clone(), platform).into()) {
				Self::Single(Icon::from_path(path))
			} else if let Some(chord) = <dyn Any>::downcast_ref::<AxislikeChord>(&*axis) {
				Self::Chord {
					button: ButtonIcons::from_button(chord.button.clone(), platform, map).unwrap_or_default(),
					axis: Arc::new(Self::from_axis(chord.axis.clone(), platform, map).unwrap_or_default()),
				}
			} else {
				match axis.decompose() {
					BasicInputs::None => return None,
					BasicInputs::Simple(btn) => {
						warn!("Expected {btn:?} to have AxisIcons path");
						let Some(basic_icons) =
							ButtonIcons::from_button(btn.clone(), platform, map)
						else {
							error!("Missing icon for {btn:?}");
							return None;
						};
						Self::Composite {
							positive: basic_icons.clone(),
							negative: basic_icons.clone(),
						}
					}
					BasicInputs::Composite(btns) => {
						let [positive, negative] = match btns.len() {
							0 => {
								error!(?axis, "BasicInputs::Composite contains no buttons");
								return None;
							}
							1 => {
								warn!(?axis, "BasicInputs::Composite contains only one button");
								[&btns[0], &btns[0]]
							}
							2 => [&btns[0], &btns[1]],
							_ => {
								error!(?axis, "BasicInputs::Composite contains too many buttons");
								[&btns[0], &btns[1]]
							}
						};
						Self::Composite {
							positive: ButtonIcons::from_button(positive.clone(), platform, map)
								.unwrap_or_default(),
							negative: ButtonIcons::from_button(negative.clone(), platform, map)
								.unwrap_or_default(),
						}
					}
					BasicInputs::Chord(btns) => {
						error!(?axis, ?btns, "should be an AxislikeChord, not a BasicInputs::Chord");
						return None;
					}
				}
			},
		)
	}
}

impl IntoIterator for AxisIcons {
	type Item = Icon;
	type IntoIter = smallvec::IntoIter<[Self::Item; 2]>;
	fn into_iter(self) -> Self::IntoIter {
		match self {
			AxisIcons::Single(icon) => smallvec![icon].into_iter(),
			AxisIcons::Composite { positive, negative } => [positive, negative]
				.into_iter()
				.flatten()
				.collect::<SmallVec<_>>()
				.into_iter(),
			AxisIcons::Chord { button, axis } => button.into_iter()
				.chain((*axis).clone().into_iter())
				.collect::<SmallVec<_>>()
				.into_iter()
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
pub enum DualAxisIcons {
	Single(Icon),
	Composite {
		horizontal: AxisIcons,
		vertical: AxisIcons,
	},
	Chord {
		button: ButtonIcons,
		dual_axis: Arc<DualAxisIcons>,
	}
}

impl Default for DualAxisIcons {
	fn default() -> Self {
		Self::Single(default())
	}
}

impl DualAxisIcons {
	pub fn from_dual_axis(
		dual_axis: Box<dyn DualAxislike>,
		platform: Option<Platform>,
		map: &InputIconFileMap,
	) -> Option<Self> {
		Some(
			if let Some(path) = map.get_dual_axis_icon(&(dual_axis.clone(), platform).into()) {
				Self::Single(Icon::from_path(path))
			} else if let Some(chord) = <dyn Any>::downcast_ref::<DualAxislikeChord>(&*dual_axis) {
				Self::Chord {
					button: ButtonIcons::from_button(chord.button.clone(), platform, map).unwrap_or_default(),
					dual_axis: Arc::new(Self::from_dual_axis(chord.dual_axis.clone(), platform, map).unwrap_or_default()),
				}
			} else {
				match dual_axis.decompose() {
					BasicInputs::None => return None,
					BasicInputs::Simple(btn) => {
						warn!("Expected {btn:?} to have AxisIcons path");
						let Some(basic_icons) =
							ButtonIcons::from_button(btn.clone(), platform, map)
						else {
							error!("Missing icon for {btn:?}");
							return None;
						};
						let axis_icons = AxisIcons::Composite {
							positive: basic_icons.clone(),
							negative: basic_icons.clone(),
						};
						Self::Composite {
							horizontal: axis_icons.clone(),
							vertical: axis_icons.clone(),
						}
					}
					BasicInputs::Composite(btns) => {
						// TODO: Try to recognize AxisIcons::Single for each axis
						let [vp, vn, hn, hp] = match btns.len() {
							0 => {
								error!(?dual_axis, "BasicInputs::Composite contains no buttons");
								return None;
							}
							1..=3 => {
								error!(
									?dual_axis,
									"BasicInputs::Composite does not contain enough buttons"
								);
								[
									&btns[0],
									&btns[1.min(btns.len() - 1)],
									&btns[2.min(btns.len() - 1)],
									&btns[3.min(btns.len() - 1)],
								]
							}
							4 => [&btns[0], &btns[1], &btns[2], &btns[3]],
							_ => {
								error!("BasicInputs::Composite contains too many buttons");
								[&btns[0], &btns[1], &btns[2], &btns[3]]
							}
						};
						Self::Composite {
							horizontal: AxisIcons::Composite {
								positive: ButtonIcons::from_button(hp.clone(), platform, map)
									.unwrap_or_default(),
								negative: ButtonIcons::from_button(hn.clone(), platform, map)
									.unwrap_or_default(),
							},
							vertical: AxisIcons::Composite {
								positive: ButtonIcons::from_button(vp.clone(), platform, map)
									.unwrap_or_default(),
								negative: ButtonIcons::from_button(vn.clone(), platform, map)
									.unwrap_or_default(),
							},
						}
					}
					BasicInputs::Chord(btns) => {
						error!(?dual_axis, ?btns, "DualAxisIcons should be a Composite of 2 Simple or Chord inputs, not a Chord itself");
						return None;
					}
				}
			},
		)
	}
}

impl IntoIterator for DualAxisIcons {
	type Item = Icon;
	type IntoIter = smallvec::IntoIter<[Self::Item; 4]>;
	fn into_iter(self) -> Self::IntoIter {
		match self {
			DualAxisIcons::Single(icon) => smallvec![icon].into_iter(),
			DualAxisIcons::Composite {
				horizontal,
				vertical,
			} => [horizontal, vertical]
				.into_iter()
				.flatten()
				.collect::<SmallVec<_>>()
				.into_iter(),
			DualAxisIcons::Chord { button, dual_axis } => button
				.into_iter()
				.chain((*dual_axis).clone().into_iter())
				.collect::<SmallVec<_>>()
				.into_iter(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
pub enum TripleAxisIcons {
	Single(Icon),
	Composite {
		x: AxisIcons,
		y: AxisIcons,
		z: AxisIcons,
	},
}

impl Default for TripleAxisIcons {
	fn default() -> Self {
		Self::Single(default())
	}
}

impl TripleAxisIcons {
	pub fn from_triple_axis(
		_triple_axis: Box<dyn TripleAxislike>,
		_platform: Option<Platform>,
		_map: &InputIconFileMap,
	) -> Option<Self> {
		todo!()
	}
}

impl IntoIterator for TripleAxisIcons {
	type Item = Icon;
	type IntoIter = smallvec::IntoIter<[Self::Item; 6]>;
	fn into_iter(self) -> Self::IntoIter {
		match self {
			TripleAxisIcons::Single(icon) => smallvec![icon].into_iter(),
			TripleAxisIcons::Composite { x, y, z } => [x, y, z]
				.into_iter()
				.flatten()
				.collect::<SmallVec<_>>()
				.into_iter(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect)]
#[reflect(no_field_bounds)]
pub enum UserInputIcons {
	Button(ButtonIcons),
	Axis(AxisIcons),
	DualAxis(DualAxisIcons),
	TripleAxis(TripleAxisIcons),
}

impl UserInputIcons {
	pub fn simple(icon: Icon) -> Self {
		Self::Button(ButtonIcons::Simple(icon))
	}

	pub fn from_user_input(
		input: UserInputWrapper,
		platform: Option<Platform>,
		map: &InputIconFileMap,
	) -> UserInputIcons {
		match input {
			UserInputWrapper::Button(btn) => {
				Self::Button(ButtonIcons::from_button(btn, platform, map).unwrap_or_default())
			}
			UserInputWrapper::Axis(axis) => {
				Self::Axis(AxisIcons::from_axis(axis, platform, map).unwrap_or_default())
			}
			UserInputWrapper::DualAxis(da) => {
				Self::DualAxis(DualAxisIcons::from_dual_axis(da, platform, map).unwrap_or_default())
			}
			UserInputWrapper::TripleAxis(ta) => Self::TripleAxis(
				TripleAxisIcons::from_triple_axis(ta, platform, map).unwrap_or_default(),
			),
		}
	}
}

impl IntoIterator for UserInputIcons {
	type Item = Icon;
	type IntoIter = smallvec::IntoIter<[Self::Item; 6]>;
	fn into_iter(self) -> Self::IntoIter {
		match self {
			UserInputIcons::Button(icons) => icons.into_iter().collect::<SmallVec<_>>().into_iter(),
			UserInputIcons::Axis(icons) => icons.into_iter().collect::<SmallVec<_>>().into_iter(),
			UserInputIcons::DualAxis(icons) => {
				icons.into_iter().collect::<SmallVec<_>>().into_iter()
			}
			UserInputIcons::TripleAxis(icons) => {
				icons.into_iter().collect::<SmallVec<_>>().into_iter()
			}
		}
	}
}

impl Default for UserInputIcons {
	fn default() -> Self {
		Self::Button(default())
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
}

impl Default for Icon {
	fn default() -> Self {
		Self {
			image: default_icon(),
			text: Some("�".into()),
		}
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
		PathBuf::from("ui/Kenney/input-prompts/Keyboard & Mouse/Vector")
			.join(format!("keyboard_{}_outline.svg", kenney_name)),
	))
}

pub mod kenney {

	use crate::util;
	use bevy::input::keyboard::Key;
	use smol_str::SmolStr;
	use std::path::PathBuf;

	pub fn base_dir() -> PathBuf {
		PathBuf::from("ui/Kenney/input-prompts")
	}

	pub fn kb_mouse_base_dir() -> PathBuf {
		PathBuf::from("ui/Kenney/input-prompts/Keyboard & Mouse/Vector")
	}

	pub fn generic_base_dir() -> PathBuf {
		PathBuf::from("ui/Kenney/input-prompts/Generic/Vector")
	}

	pub fn key_name(key: &Key) -> Option<SmolStr> {
		use Key::*;
		let s: &'static str = match key {
			Character(c) => return rename_char(c.clone()),
			Dead(c) => return c.map(std::iter::once).map(SmolStr::from_iter),
			Alt | AltGraph => platform_alt(),
			CapsLock => "capslock",
			Control => "ctrl",
			Fn | FnLock => "function",
			NumLock => "numlock",
			Shift => "shift_icon",
			Meta | Hyper | Super => platform_meta(),
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
			Backspace => platform_backspace(),
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

	pub fn platform_meta() -> &'static str {
		if util::host_is_mac() {
			"command"
		} else {
			// TODO: Find a "Super" icon for Linux?
			"win"
		}
	}

	pub fn platform_backspace() -> &'static str {
		if util::host_is_mac() {
			"backspace_icon_alternative"
		} else {
			"backspace_icon"
		}
	}
}

#[derive(Resource, Debug, Clone, Reflect, Serialize, Deserialize)]
#[reflect(Resource, Serialize, Deserialize)]
pub struct InputIconFileMap {
	button_map: HashMap<IconPathKey<Box<dyn Buttonlike>>, AssetPath<'static>>,
	axis_map: HashMap<IconPathKey<Box<dyn Axislike>>, AssetPath<'static>>,
	dual_axis_map: HashMap<IconPathKey<Box<dyn DualAxislike>>, AssetPath<'static>>,
	triple_axis_map: HashMap<IconPathKey<Box<dyn TripleAxislike>>, AssetPath<'static>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect, Serialize, Deserialize)]
#[reflect(PartialEq, Hash, Serialize, Deserialize, where T: PartialEq + Hash + Serialize + for<'de> Deserialize<'de>)]
pub struct IconPathKey<T>(
	T,
	#[serde(default, skip_serializing_if = "Option::is_none")] Option<Platform>,
);

impl<T: ?Sized> From<Box<T>> for IconPathKey<Box<T>> {
	fn from(value: Box<T>) -> Self {
		Self(value, None)
	}
}

impl<T: ?Sized> From<(Box<T>, Platform)> for IconPathKey<Box<T>> {
	fn from(value: (Box<T>, Platform)) -> Self {
		Self(value.0, Some(value.1))
	}
}

impl<T: ?Sized> From<(Box<T>, Option<Platform>)> for IconPathKey<Box<T>> {
	fn from(value: (Box<T>, Option<Platform>)) -> Self {
		Self(value.0, value.1)
	}
}

macro_rules! unprocessed {
	($key:ident $map:expr => $T:ident $Trait:ident $fallback:ident) => {
		if let Some(val) = <dyn Any>::downcast_ref::<$T>(&*$key.0) {
			let key = IconPathKey(
				Box::new($T { processors: Vec::new(), ..val.clone() }) as Box<dyn $Trait>,
				$key.1,
			);
			return $map.get(&key).or_else(|| $fallback(key.0.clone()))
		}
	}
}

impl InputIconFileMap {
	pub fn insert(
		&mut self,
		input: impl Into<UserInputWrapper>,
		platform: Option<Platform>,
		path: AssetPath<'static>,
	) -> Option<AssetPath<'static>> {
		match input.into() {
			UserInputWrapper::Button(btn) => {
				self.button_map.insert(IconPathKey(btn, platform), path)
			}
			UserInputWrapper::Axis(axis) => self.axis_map.insert(IconPathKey(axis, platform), path),
			UserInputWrapper::DualAxis(da) => {
				self.dual_axis_map.insert(IconPathKey(da, platform), path)
			}
			UserInputWrapper::TripleAxis(ta) => {
				self.triple_axis_map.insert(IconPathKey(ta, platform), path)
			}
		}
	}

	pub fn insert_button(
		&mut self,
		input: impl Buttonlike,
		platform: Option<Platform>,
		path: AssetPath<'static>,
	) -> Option<AssetPath<'static>> {
		self.button_map
			.insert(IconPathKey(Box::new(input), platform), path)
	}

	pub fn insert_axis(
		&mut self,
		input: impl Axislike,
		platform: Option<Platform>,
		path: AssetPath<'static>,
	) -> Option<AssetPath<'static>> {
		self.axis_map
			.insert(IconPathKey(Box::new(input), platform), path)
	}

	pub fn insert_dual_axis(
		&mut self,
		input: impl DualAxislike,
		platform: Option<Platform>,
		path: AssetPath<'static>,
	) -> Option<AssetPath<'static>> {
		self.dual_axis_map
			.insert(IconPathKey(Box::new(input), platform), path)
	}

	pub fn insert_triple_axis(
		&mut self,
		input: impl TripleAxislike,
		platform: Option<Platform>,
		path: AssetPath<'static>,
	) -> Option<AssetPath<'static>> {
		self.triple_axis_map
			.insert(IconPathKey(Box::new(input), platform), path)
	}

	pub fn get_button_icon(
		&self,
		button: &IconPathKey<Box<dyn Buttonlike>>,
	) -> Option<&AssetPath<'static>> {
		let fallback = || self.button_map.get(&IconPathKey(button.0.clone(), None));
		macro_rules! zero_threshold {
			($T:ident) => {
				if let Some(val) = <dyn Any>::downcast_ref::<$T>(&*button) {
					return self.button_map.get(&IconPathKey(
						Box::new($T { threshold: 0.0, ..val.clone() }) as Box<dyn Buttonlike>,
						button.1,
					)).or_else(fallback)
				}
			}
		}
		zero_threshold!(MouseMoveDirection);
		zero_threshold!(MouseScrollDirection);
		zero_threshold!(GamepadControlDirection);
		
		if let Some(val) = <dyn Any>::downcast_ref::<SpecificGamepadButton>(&*button) {
			return self.get_button_icon(&IconPathKey(
				Box::new(val.button) as Box<dyn Buttonlike>,
				button.1,
			))
		}
		
		self.button_map.get(button).or_else(fallback)
	}

	pub fn get_axis_icon(
		&self,
		axis: &IconPathKey<Box<dyn Axislike>>,
	) -> Option<&AssetPath<'static>> {
		let fallback = |input| self.axis_map.get(&IconPathKey(input, None));
		
		unprocessed!(axis self.axis_map => MouseMoveAxis Axislike fallback);
		unprocessed!(axis self.axis_map => MouseScrollAxis Axislike fallback);
		unprocessed!(axis self.axis_map => VirtualAxis Axislike fallback);
		
		if let Some(val) = <dyn Any>::downcast_ref::<GamepadControlAxis>(&*axis.0) {
			let key = IconPathKey(
				Box::new(val.axis) as Box<dyn Axislike>,
				axis.1,
			);
			return self.get_axis_icon(&key).or_else(|| fallback(key.0.clone()))
		}
		
		if let Some(val) = <dyn Any>::downcast_ref::<SpecificGamepadAxis>(&*axis.0) {
			let key = IconPathKey(
				Box::new(val.axis) as Box<dyn Axislike>,
				axis.1,
			);
			return self.get_axis_icon(&key).or_else(|| fallback(key.0.clone()))
		}
		
		self.axis_map.get(axis).or_else(|| fallback(axis.0.clone()))
	}

	pub fn get_dual_axis_icon(
		&self,
		da: &IconPathKey<Box<dyn DualAxislike>>,
	) -> Option<&AssetPath<'static>> {
		let fallback = |input| self.dual_axis_map.get(&IconPathKey(input, None));
		
		unprocessed!(da self.dual_axis_map => GamepadStick DualAxislike fallback);
		unprocessed!(da self.dual_axis_map => MouseMove DualAxislike fallback);
		unprocessed!(da self.dual_axis_map => MouseScroll DualAxislike fallback);
		unprocessed!(da self.dual_axis_map => VirtualDPad DualAxislike fallback);
		
		self.dual_axis_map.get(da).or_else(|| fallback(da.0.clone()))
	}

	pub fn get_triple_axis_icon(
		&self,
		ta: &IconPathKey<Box<dyn TripleAxislike>>,
	) -> Option<&AssetPath<'static>> {
		self.triple_axis_map
			.get(ta)
			.or_else(|| self.triple_axis_map.get(&IconPathKey(ta.0.clone(), None)))
	}
}

impl Index<&IconPathKey<Box<dyn Buttonlike>>> for InputIconFileMap {
	type Output = AssetPath<'static>;
	fn index(&self, index: &IconPathKey<Box<dyn Buttonlike>>) -> &Self::Output {
		self.get_button_icon(index).unwrap()
	}
}

impl Index<&IconPathKey<Box<dyn Axislike>>> for InputIconFileMap {
	type Output = AssetPath<'static>;
	fn index(&self, index: &IconPathKey<Box<dyn Axislike>>) -> &Self::Output {
		self.get_axis_icon(index).unwrap()
	}
}

impl Index<&IconPathKey<Box<dyn DualAxislike>>> for InputIconFileMap {
	type Output = AssetPath<'static>;
	fn index(&self, index: &IconPathKey<Box<dyn DualAxislike>>) -> &Self::Output {
		self.get_dual_axis_icon(index).unwrap()
	}
}

impl Index<&IconPathKey<Box<dyn TripleAxislike>>> for InputIconFileMap {
	type Output = AssetPath<'static>;
	fn index(&self, index: &IconPathKey<Box<dyn TripleAxislike>>) -> &Self::Output {
		self.get_triple_axis_icon(index).unwrap()
	}
}
