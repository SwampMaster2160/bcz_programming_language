#[repr(C)]
pub enum Linkage {
	External = 0,
	DLLImport = 10,
}

#[repr(C)]
pub enum CallingConvention {
	Win64 = 79,
}

#[repr(C)]
pub enum CodegenFileType {
	Object = 1,
}

#[repr(C)]
pub enum CodegenOptLevel {
	Default = 2,
}

#[repr(C)]
pub enum RealocMode {
	Default = 0,
}

#[repr(C)]
pub enum CodeModel {
	Default = 0,
}