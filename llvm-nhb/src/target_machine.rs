use super::{llvm_c::{LLVMCreateTargetDataLayout, LLVMTargetMachineRef}, target_data::TargetData, traits::WrappedReference};

#[repr(transparent)]
pub struct TargetMachine {
	machine_ref: LLVMTargetMachineRef,
}

impl TargetMachine {
	pub fn get_target_data<'a>(&'a self) -> TargetData<'a> {
		unsafe { TargetData::from_ref(LLVMCreateTargetDataLayout(self.machine_ref)) }
	}
}

unsafe impl WrappedReference for TargetMachine {
	type RefType = LLVMTargetMachineRef;
}