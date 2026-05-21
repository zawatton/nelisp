use std::alloc::{self, Layout};
fn nl_layout(size: i64, align: i64) -> Option<Layout> { (size > 0 && align > 0).then(|| Layout::from_size_align(size as usize, align as usize).ok()).flatten() }
#[no_mangle] pub unsafe extern "C" fn nl_alloc_bytes(size: i64, align: i64) -> *mut u8 { nl_layout(size, align).map_or(std::ptr::null_mut(), |l| unsafe { alloc::alloc(l) }) }
#[no_mangle] pub unsafe extern "C" fn nl_dealloc_bytes(ptr: *mut u8, size: i64, align: i64) { if !ptr.is_null() { if let Some(l) = nl_layout(size, align) { unsafe { alloc::dealloc(ptr, l) }; } } }
#[cfg(test)]
mod tests {
    use super::*;
    #[test] fn alloc_dealloc_round_trip() { let (s,a)=(64i64,8i64); let p=unsafe{nl_alloc_bytes(s,a)}; assert!(!p.is_null()); unsafe{nl_dealloc_bytes(p,s,a)} }
    #[test] fn alloc_rejects_bad() { for(s,a) in [(32,3),(0,8),(-1,8),(32,0)] { assert!(unsafe{nl_alloc_bytes(s,a)}.is_null()); } }
    #[test] fn dealloc_null_no_op() { unsafe{nl_dealloc_bytes(std::ptr::null_mut(),32,8)} }
    #[test] fn alloc_16_aligned() { let p=unsafe{nl_alloc_bytes(128,16)}; assert!(!p.is_null()&&(p as usize)&0xF==0); unsafe{nl_dealloc_bytes(p,128,16)} }
}
