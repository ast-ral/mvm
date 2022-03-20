#[non_exhaustive]
#[derive(Debug)]
pub enum Fault {
	MemoryError,
	ExplicitTrap,
	InvalidInstruction,
	ExecutionError,
}

pub trait Memory {
	fn read(&mut self, address: u64) -> Option<u8>;
	fn write(&mut self, address: u64, value: u8) -> Option<()>;
}

pub struct MVM<M> {
	memory: M,
	ip: u64,
	registers: [u64; 15],
	flags: u8,
}

fn split(x: u8) -> (u8, u8) {
	(x & 0x0f, x >> 4)
}

trait Flaggable: Copy {
	fn zero(self) -> bool;
	fn sign(self) -> bool;
}

impl Flaggable for u8 {
	fn zero(self) -> bool {
		self == 0
	}

	fn sign(self) -> bool {
		self & 0x80 != 0
	}
}

impl Flaggable for u16 {
	fn zero(self) -> bool {
		self == 0
	}

	fn sign(self) -> bool {
		self & 0x8000 != 0
	}
}

impl Flaggable for u32 {
	fn zero(self) -> bool {
		self == 0
	}

	fn sign(self) -> bool {
		self & 0x8000_0000 != 0
	}
}

impl Flaggable for u64 {
	fn zero(self) -> bool {
		self == 0
	}

	fn sign(self) -> bool {
		self & 0x8000_0000_0000_0000 != 0
	}
}

trait LowHighMul {
	fn low_high_mul(self, other: Self) -> (Self, Self) where Self: Sized;
	fn signed_low_high_mul(self, other: Self) -> (Self, Self) where Self: Sized;
}

impl LowHighMul for u8 {
	fn low_high_mul(self, other: Self) -> (Self, Self) {
		let out = self as u16 * other as u16;
		(out as u8, (out >> 8) as u8)
	}

	fn signed_low_high_mul(self, other: Self) -> (Self, Self) {
		let out = self as i16 * other as i16;
		(out as u8, (out >> 8) as u8)
	}
}

impl LowHighMul for u16 {
	fn low_high_mul(self, other: Self) -> (Self, Self) {
		let out = self as u32 * other as u32;
		(out as u16, (out >> 16) as u16)
	}

	fn signed_low_high_mul(self, other: Self) -> (Self, Self) {
		let out = self as i32 * other as i32;
		(out as u16, (out >> 16) as u16)
	}
}

impl LowHighMul for u32 {
	fn low_high_mul(self, other: Self) -> (Self, Self) {
		let out = self as u64 * other as u64;
		(out as u32, (out >> 32) as u32)
	}

	fn signed_low_high_mul(self, other: Self) -> (Self, Self) {
		let out = self as i64 * other as i64;
		(out as u32, (out >> 32) as u32)
	}
}

impl LowHighMul for u64 {
	fn low_high_mul(self, other: Self) -> (Self, Self) {
		let out = self as u128 * other as u128;
		(out as u64, (out >> 64) as u64)
	}

	fn signed_low_high_mul(self, other: Self) -> (Self, Self) {
		let out = self as i128 * other as i128;
		(out as u64, (out >> 64) as u64)
	}
}

trait ToSignedType {
	type Signed;
}

impl ToSignedType for u8 {
	type Signed = i8;
}

impl ToSignedType for u16 {
	type Signed = i8;
}

impl ToSignedType for u32 {
	type Signed = i8;
}

impl ToSignedType for u64 {
	type Signed = i8;
}

impl<M: Memory> MVM<M> {
	pub fn step(&mut self) -> Result<bool, Fault> {
		macro_rules! binary_subsidiaries {
			($op_core:ident, $nimm:ident, $imm:ident) => {
				macro_rules! $nimm {
					($t:ty) => {{
						let (dst, src_a) = split(self.read_ip_u8()?);
						let (src_b, _) = split(self.read_ip_u8()?);

						let val_a = self.get_register(src_a) as $t;
						let val_b = self.get_register(src_b) as $t;
						$op_core!($t, dst, val_a, val_b);
					}}
				}

				macro_rules! $imm {
					($t:ty) => {{
						let (dst, src) = split(self.read_ip_u8()?);
						let val_b = self.read_ip_and_process(<$t>::from_le_bytes)?;

						let val_a = self.get_register(src) as $t;
						$op_core!($t, dst, val_a, val_b);
					}}
				}
			}
		}

		macro_rules! ternary_subsidiaries {
			($op_core:ident, $nimm:ident, $imm:ident) => {
				macro_rules! $nimm {
					($t:ty) => {{
						let (dst_a, dst_b) = split(self.read_ip_u8()?);
						let (src_a, src_b) = split(self.read_ip_u8()?);

						let val_a = self.get_register(src_a) as $t;
						let val_b = self.get_register(src_b) as $t;
						$op_core!($t, dst_a, dst_b, val_a, val_b);
					}}
				}

				macro_rules! $imm {
					($t:ty) => {{
						let (dst_a, dst_b) = split(self.read_ip_u8()?);
						let (src, _) = split(self.read_ip_u8()?);
						let val_b = self.read_ip_and_process(<$t>::from_le_bytes)?;

						let val_a = self.get_register(src) as $t;
						$op_core!($t, dst_a, dst_b, val_a, val_b);
					}}
				}
			}
		}

		macro_rules! define_jumps {
			($condition:tt, $jal:ident, $jalr: ident) => {
				macro_rules! $jal {
					() => {{
						let (dst, _) = split(self.read_ip_u8()?);
						let address = self.read_ip_ptrsize()?;

						if $condition {
							self.set_register(dst, self.ip);
							self.ip = address;
						}
					}}
				}

				macro_rules! $jalr {
					() => {{
						let (dst, src) = split(self.read_ip_u8()?);
						let address = self.get_register(src);

						if $condition {
							self.set_register(dst, self.ip);
							self.ip = address;
						}
					}}
				}
			}
		}

		macro_rules! add_core {
			($t:ty, $dst:ident, $val_a:ident, $val_b:ident) => {
				let (out, carry) = $val_a.overflowing_add($val_b);

				self.set_flags_for_val(out);
				self.set_carry(carry);
				self.set_register($dst, u64::from(out));
			}
		}

		macro_rules! adc_core {
			($t:ty, $dst:ident, $val_a:ident, $val_b:ident) => {
				let (val_a, carry_a) = $val_a.overflowing_add(1);
				let (out, carry_b) = val_a.overflowing_add($val_b);

				self.set_flags_for_val(out);
				self.set_carry(carry_a | carry_b);
				self.set_register($dst, u64::from(out));
			}
		}

		macro_rules! sub_core {
			($t:ty, $dst:ident, $val_a:ident, $val_b:ident) => {
				let (out, carry) = $val_a.overflowing_sub($val_b);

				self.set_flags_for_val(out);
				self.set_carry(carry);
				self.set_register($dst, u64::from(out));
			}
		}

		macro_rules! sbb_core {
			($t:ty, $dst:ident, $val_a:ident, $val_b:ident) => {
				let (val_a, carry_a) = $val_a.overflowing_sub(1);
				let (out, carry_b) = val_a.overflowing_sub($val_b);

				self.set_flags_for_val(out);
				self.set_carry(carry_a | carry_b);
				self.set_register($dst, u64::from(out));
			}
		}

		macro_rules! and_core {
			($t:ty, $dst:ident, $val_a:ident, $val_b:ident) => {
				let out = $val_a & $val_b;

				self.set_flags_for_val(out);
				self.set_register($dst, u64::from(out));
			}
		}

		macro_rules! or_core {
			($t:ty, $dst:ident, $val_a:ident, $val_b:ident) => {
				let out = $val_a | $val_b;

				self.set_flags_for_val(out);
				self.set_register($dst, u64::from(out));
			}
		}

		macro_rules! xor_core {
			($t:ty, $dst:ident, $val_a:ident, $val_b:ident) => {
				let out = $val_a ^ $val_b;

				self.set_flags_for_val(out);
				self.set_register($dst, u64::from(out));
			}
		}

		macro_rules! mul_core {
			($t:ty, $dst_a:ident, $dst_b:ident, $val_a:ident, $val_b:ident) => {
				let (out_a, out_b) = $val_a.low_high_mul($val_b);
				self.set_register($dst_a, out_a as u64);
				self.set_register($dst_b, out_b as u64);
			}
		}

		macro_rules! imul_core {
			($t:ty, $dst_a:ident, $dst_b:ident, $val_a:ident, $val_b:ident) => {
				let (out_a, out_b) = $val_a.signed_low_high_mul($val_b);
				self.set_register($dst_a, out_a as u64);
				self.set_register($dst_b, out_b as u64);
			}
		}

		macro_rules! div_core {
			($t:ty, $dst_a:ident, $dst_b:ident, $val_a:ident, $val_b:ident) => {
				let out_a = $val_a.checked_div_euclid($val_b).ok_or(Fault::ExecutionError)?;
				let out_b = $val_a.checked_rem_euclid($val_b).ok_or(Fault::ExecutionError)?;

				self.set_register($dst_a, out_a as u64);
				self.set_register($dst_b, out_b as u64);
			}
		}

		macro_rules! idiv_core {
			($t:ty, $dst_a:ident, $dst_b:ident, $val_a:ident, $val_b:ident) => {
				let val_a = $val_a as <$t as ToSignedType>::Signed;
				let val_b = $val_b as <$t as ToSignedType>::Signed;

				let out_a = val_a.checked_div_euclid(val_b).ok_or(Fault::ExecutionError)?;
				let out_b = val_a.checked_rem_euclid(val_b).ok_or(Fault::ExecutionError)?;

				self.set_register($dst_a, out_a as u64);
				self.set_register($dst_b, out_b as u64);
			}
		}

		macro_rules! not {
			($t:ty) => {{
				let (dst, src) = split(self.read_ip_u8()?);
				let data = self.get_register(src) as $t;
				self.set_register(dst, !data as u64);
			}}
		}

		macro_rules! se {
			($t:ty) => {{
				let (dst, src) = split(self.read_ip_u8()?);
				let data = self.get_register(src) as $t;
				self.set_register(dst, data as i64 as u64);
			}}
		}

		macro_rules! sei {
			($t:ty) => {{
				let (dst, _) = split(self.read_ip_u8()?);
				let data = self.read_ip_and_process(<$t>::from_le_bytes)?;
				self.set_register(dst, data as i64 as u64);
			}}
		}

		macro_rules! load {
			($t:ty) => {{
				let (dst, src) = split(self.read_ip_u8()?);
				let address = self.get_register(src);

				let val = self.read_address_and_process(address, <$t>::from_le_bytes)?;
				self.set_register(dst, u64::from(val));
			}}
		}

		macro_rules! store {
			($t:ty) => {{
				let (dst, src) = split(self.read_ip_u8()?);
				let address = self.get_register(dst);
				let val = self.get_register(src) as $t;

				self.write_address(address, val.to_le_bytes())?;
			}}
		}

		binary_subsidiaries!(add_core, add, addi);
		binary_subsidiaries!(adc_core, adc, adci);
		binary_subsidiaries!(sub_core, sub, subi);
		binary_subsidiaries!(sbb_core, sbb, sbbi);
		binary_subsidiaries!(and_core, and, andi);
		binary_subsidiaries!(or_core, or, ori);
		binary_subsidiaries!(xor_core, xor, xori);

		ternary_subsidiaries!(mul_core, mul, muli);
		ternary_subsidiaries!(imul_core, imul, imuli);
		ternary_subsidiaries!(div_core, div, divi);
		ternary_subsidiaries!(idiv_core, idiv, idivi);

		define_jumps!({true}, jal, jalr);
		define_jumps!({self.get_zero()}, jalz, jalrz);
		define_jumps!({!self.get_zero()}, jalnz, jalrnz);
		define_jumps!({self.get_sign()}, jals, jalrs);
		define_jumps!({!self.get_sign()}, jalns, jalrns);
		define_jumps!({self.get_carry()}, jalc, jalrc);
		define_jumps!({!self.get_carry()}, jalnc, jalrnc);
		define_jumps!({self.get_zero() | self.get_sign()}, jalzs, jalrzs);
		define_jumps!({!self.get_zero() & !self.get_sign()}, jalnzs, jalrnzs);
		define_jumps!({self.get_zero() | self.get_carry()}, jalzc, jalrzc);
		define_jumps!({!self.get_zero() & !self.get_carry()}, jalnzc, jalrnzc);

		let opcode = self.read_ip_u8()?;

		match opcode {
			0x00 => add!(u8),
			0x01 => add!(u16),
			0x02 => add!(u32),
			0x03 => add!(u64),
			0x04 => addi!(u8),
			0x05 => addi!(u16),
			0x06 => addi!(u32),
			0x07 => addi!(u64),

			0x08 => adc!(u8),
			0x09 => adc!(u16),
			0x0a => adc!(u32),
			0x0b => adc!(u64),
			0x0c => adci!(u8),
			0x0d => adci!(u16),
			0x0e => adci!(u32),
			0x0f => adci!(u64),

			0x10 => sub!(u8),
			0x11 => sub!(u16),
			0x12 => sub!(u32),
			0x13 => sub!(u64),
			0x14 => subi!(u8),
			0x15 => subi!(u16),
			0x16 => subi!(u32),
			0x17 => subi!(u64),

			0x18 => sbb!(u8),
			0x19 => sbb!(u16),
			0x1a => sbb!(u32),
			0x1b => sbb!(u64),
			0x1c => sbbi!(u8),
			0x1d => sbbi!(u16),
			0x1e => sbbi!(u32),
			0x1f => sbbi!(u64),

			0x20 => and!(u8),
			0x21 => and!(u16),
			0x22 => and!(u32),
			0x23 => and!(u64),
			0x24 => andi!(u8),
			0x25 => andi!(u16),
			0x26 => andi!(u32),
			0x27 => andi!(u64),

			0x28 => or!(u8),
			0x29 => or!(u16),
			0x2a => or!(u32),
			0x2b => or!(u64),
			0x2c => ori!(u8),
			0x2d => ori!(u16),
			0x2e => ori!(u32),
			0x2f => ori!(u64),

			0x30 => xor!(u8),
			0x31 => xor!(u16),
			0x32 => xor!(u32),
			0x33 => xor!(u64),
			0x34 => xori!(u8),
			0x35 => xori!(u16),
			0x36 => xori!(u32),
			0x37 => xori!(u64),

			0x38 => mul!(u8),
			0x39 => mul!(u16),
			0x3a => mul!(u32),
			0x3b => mul!(u64),
			0x3c => muli!(u8),
			0x3d => muli!(u16),
			0x3e => muli!(u32),
			0x3f => muli!(u64),

			0x40 => imul!(u8),
			0x41 => imul!(u16),
			0x42 => imul!(u32),
			0x43 => imul!(u64),
			0x44 => imuli!(u8),
			0x45 => imuli!(u16),
			0x46 => imuli!(u32),
			0x47 => imuli!(u64),

			0x48 => div!(u8),
			0x49 => div!(u16),
			0x4a => div!(u32),
			0x4b => div!(u64),
			0x4c => divi!(u8),
			0x4d => divi!(u16),
			0x4e => divi!(u32),
			0x4f => divi!(u64),

			0x50 => idiv!(u8),
			0x51 => idiv!(u16),
			0x52 => idiv!(u32),
			0x53 => idiv!(u64),
			0x54 => idivi!(u8),
			0x55 => idivi!(u16),
			0x56 => idivi!(u32),
			0x57 => idivi!(u64),

			0xc0 => not!(u8),
			0xc1 => not!(u16),
			0xc2 => not!(u32),
			0xc3 => not!(u64),

			0xd0 => jal!(),
			0xd1 => jalr!(),

			0xd2 => jalz!(),
			0xd3 => jalrz!(),

			0xd4 => jalnz!(),
			0xd5 => jalrnz!(),

			0xd6 => jals!(),
			0xd7 => jalrs!(),

			0xd8 => jalns!(),
			0xd9 => jalrns!(),

			0xda => jalc!(),
			0xdb => jalrc!(),

			0xdc => jalnc!(),
			0xdd => jalrnc!(),

			0xde => jalzs!(),
			0xdf => jalrzs!(),

			0xe0 => jalnzs!(),
			0xe1 => jalrnzs!(),

			0xe2 => jalzc!(),
			0xe3 => jalrzc!(),

			0xe4 => jalnzc!(),
			0xe5 => jalrnzc!(),

			0xf0 => load!(u8),
			0xf1 => load!(u16),
			0xf2 => load!(u32),
			0xf3 => load!(u64),

			0xf4 => store!(u8),
			0xf5 => store!(u16),
			0xf6 => store!(u32),
			0xf7 => store!(u64),

			0xf8 => se!(u8),
			0xf9 => sei!(u8),
			0xfa => se!(u16),
			0xfb => sei!(u16),
			0xfc => se!(u32),
			0xfd => sei!(u32),

			0xfe => return Ok(true),                 // halt
			0xff => return Err(Fault::ExplicitTrap), // trap

			_ => return Err(Fault::InvalidInstruction),
		}

		Ok(false)
	}

	fn get_register(&self, register: u8) -> u64 {
		assert!(register < 16);

		match register {
			0 => 0,
			x => self.registers[usize::from(x - 1)],
		}
	}

	fn set_register(&mut self, register: u8, value: u64) {
		assert!(register < 16);

		match register {
			0 => {}
			x => self.registers[usize::from(x - 1)] = value,
		}
	}

	fn read_ip_u8(&mut self) -> Result<u8, Fault> {
		let out = self.memory.read(self.ip).ok_or(Fault::MemoryError)?;
		self.ip = self.ip.wrapping_add(1);
		Ok(out)
	}

	fn read_ip_and_process<const N: usize, T>(
		&mut self,
		process: impl FnOnce([u8; N]) -> T,
	) -> Result<T, Fault> {
		let mut out = [0; N];

		for i in 0 .. N {
			out[i] = self.read_ip_u8()?;
		}

		Ok(process(out))
	}

	fn read_address_and_process<const N: usize, T>(
		&mut self,
		address: u64,
		process: impl FnOnce([u8; N]) -> T,
	) -> Result<T, Fault> {
		let mut out = [0; N];

		for i in 0 .. N {
			out[i] = self.memory.read(address.wrapping_add(i as u64)).ok_or(Fault::MemoryError)?;
		}

		Ok(process(out))
	}

	fn write_address<const N: usize>(
		&mut self,
		address: u64,
		val: [u8; N],
	) -> Result<(), Fault> {
		for i in 0 .. N {
			self.memory.write(address.wrapping_add(i as u64), val[i]).ok_or(Fault::MemoryError)?;
		}

		Ok(())
	}

	fn read_ip_ptrsize(&mut self) -> Result<u64, Fault> {
		self.read_ip_and_process(u64::from_le_bytes)
	}

	fn set_flags_for_val(&mut self, val: impl Flaggable) {
		self.flags &= !0x03;
		self.flags |= u8::from(val.zero()) << 0;
		self.flags |= u8::from(val.sign()) << 1;
	}

	fn set_carry(&mut self, carry: bool) {
		self.flags &= !0x04;
		self.flags |= u8::from(carry) << 2;
	}

	fn get_zero(&mut self) -> bool {
		self.flags & 0x01 != 0
	}

	fn get_sign(&mut self) -> bool {
		self.flags & 0x02 != 0
	}

	fn get_carry(&mut self) -> bool {
		self.flags & 0x04 != 0
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	impl Memory for Vec<u8> {
		fn read(&mut self, address: u64) -> Option<u8> {
			let address: usize = address.try_into().ok()?;
			self.get(address).map(|&x| x)
		}

		fn write(&mut self, address: u64, value: u8) -> Option<()> {
			let address: usize = address.try_into().ok()?;
			*self.get_mut(address)? = value;
			Some(())
		}
	}

	const ARBITRARY_INIT: u64 = 0x0123456789abcdef;

	#[test]
	fn test_addition_loop() {
		// quick program to essentially check (0 .. 23).sum() == 253

		let src = vec![
			0x04, 0x01, 0x00, // addi r1 r0 0
			0x04, 0x02, 0x00, // addi r2 r0 0

			// loop_start:
			0x14, 0x10, 0x17, // sub r0 r1 23
			0xd2, 0x00, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // jalz r0 loop_end

			0x00, 0x22, 0x01, // add r2 r2 r1
			0x04, 0x11, 0x01, // addi r1 r1 1
			0xd0, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // jmp loop_start

			// loop_end:
			0x14, 0x20, 0xfc, // sub r0 r2 253
			0xd0, 0x00, 0x31, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // jalz r0 success

			0xff, // trap

			// success:
			0xfe, // halt
		];

		let mut mvm = MVM {
			memory: src,
			ip: 0,
			registers: [ARBITRARY_INIT; 15],
			flags: 0,
		};

		while !mvm.step().unwrap() {}
	}
}
