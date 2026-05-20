use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};

static QUIT_FLAG: AtomicI64 = AtomicI64::new(0);
#[cfg(unix)]
static SIGINT_INSTALLED: AtomicBool = AtomicBool::new(false);

#[no_mangle]
pub extern "C" fn nl_quit_flag_ptr() -> *mut i64 { QUIT_FLAG.as_ptr() }
pub fn take_quit_flag() -> bool { QUIT_FLAG.swap(0, Ordering::SeqCst) != 0 }

#[cfg(unix)]
pub fn install_sigint_handler() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| unsafe {
        extern "C" fn handler(_: libc::c_int) { QUIT_FLAG.store(1, Ordering::SeqCst); }
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = handler as *const () as usize; libc::sigemptyset(&mut sa.sa_mask); sa.sa_flags = libc::SA_RESTART;
        libc::sigaction(libc::SIGINT, &sa, std::ptr::null_mut());
        SIGINT_INSTALLED.store(true, Ordering::SeqCst);
    });
}
#[cfg(not(unix))] pub fn install_sigint_handler() {}
#[cfg(unix)] pub fn sigint_handler_installed_p() -> bool { SIGINT_INSTALLED.load(Ordering::SeqCst) }
#[cfg(not(unix))] pub fn sigint_handler_installed_p() -> bool { false }
