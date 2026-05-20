mod error;
pub use error::{ReadError, SourcePos};
pub use crate::eval::sexp::{fmt_sexp, Sexp};

use crate::elisp_cc_spike;

const PARSER_POOL_SIZE: usize = 3 + 4 * 1024;

const SCRATCH_CAP: i64 = 64;

const NYI_MSG: &str =
    "feature unsupported by elisp Reader (record `#s(..)', byte-code `#[..]', or syntax error)";

const START_POS: SourcePos = SourcePos { line: 1, col: 1 };

pub fn read_str(input: &str) -> Result<Sexp, ReadError> {
    match try_elisp_read_str(input) {
        Some(result) => result,
        None => Err(ReadError::not_yet_implemented(NYI_MSG, START_POS)),
    }
}

pub fn read_all(input: &str) -> Result<Vec<Sexp>, ReadError> {
    match try_elisp_read_all(input) {
        Some(result) => result,
        None => Err(ReadError::not_yet_implemented(NYI_MSG, START_POS)),
    }
}

fn try_elisp_read_str(input: &str) -> Option<Result<Sexp, ReadError>> {
    let mut state = ElispReadState::new(input);
    match state.parse_one_form()? {
        ElispParseOutcome::Form(form) => {
            if state.has_trailing_token() {
                return Some(Err(ReadError::parse(
                    "trailing token after first form".to_string(),
                    START_POS,
                )));
            }
            Some(Ok(form))
        }
        ElispParseOutcome::Empty => Some(Err(ReadError::unexpected_eof("empty input", START_POS))),
    }
}

fn try_elisp_read_all(input: &str) -> Option<Result<Vec<Sexp>, ReadError>> {
    let mut state = ElispReadState::new(input);
    let mut forms = Vec::new();
    loop {
        match state.parse_one_form()? {
            ElispParseOutcome::Form(form) => forms.push(form),
            ElispParseOutcome::Empty => break,
        }
    }
    Some(Ok(forms))
}

enum ElispParseOutcome {
    Form(Sexp),
    Empty,
}

struct ElispReadState {
    src: Sexp,
    cursor_slot: Sexp,
    result_slot: Sexp,
    pool_slot: Sexp,
}

impl ElispReadState {
    fn new(input: &str) -> Self {
        let src = Sexp::Str(input.to_string());
        let cursor_slot = Sexp::Int(0);
        let result_slot = Sexp::Nil;
        let mut slots = vec![Sexp::Nil; PARSER_POOL_SIZE];
        slots[0] = Sexp::mut_str(String::with_capacity(SCRATCH_CAP as usize));
        let pool_slot = Sexp::vector(slots);
        ElispReadState {
            src,
            cursor_slot,
            result_slot,
            pool_slot,
        }
    }

    fn cursor(&self) -> i64 {
        match self.cursor_slot {
            Sexp::Int(n) => n,
            _ => 0,
        }
    }

    fn lex_peek_advancing(&mut self) -> i64 {
        unsafe {
            let pool_ptr = &self.pool_slot as *const Sexp;
            let scratch_ptr = vector_slot_ptr(pool_ptr, 0) as *mut Sexp;
            elisp_cc_spike::mut_str_make_empty(scratch_ptr, SCRATCH_CAP);
        }
        let cursor = self.cursor();
        let payload_ptr;
        let scratch_ptr;
        let cursor_out_ptr;
        unsafe {
            let pool_ptr = &self.pool_slot as *const Sexp;
            payload_ptr = vector_slot_ptr(pool_ptr, 1) as *mut Sexp;
            scratch_ptr = vector_slot_ptr(pool_ptr, 0) as *mut Sexp;
            cursor_out_ptr = &mut self.cursor_slot as *mut Sexp;
        }
        unsafe {
            elisp_cc_spike::reader_lex_one(
                &self.src as *const Sexp,
                cursor,
                payload_ptr,
                cursor_out_ptr,
                scratch_ptr,
            )
        }
    }

    fn at_eof_after_skip(&mut self) -> bool {
        let saved = self.cursor();
        let kind = self.lex_peek_advancing();
        if kind == 0 {
            true
        } else {
            self.cursor_slot = Sexp::Int(saved);
            false
        }
    }

    fn parse_one_form(&mut self) -> Option<ElispParseOutcome> {
        if self.at_eof_after_skip() {
            return Some(ElispParseOutcome::Empty);
        }
        // Drop the previous result normally — cons-make-with-clone in the .o
        // bumped inner-box refcounts so Drop decrements them correctly.
        self.result_slot = Sexp::Nil;
        let status = unsafe {
            elisp_cc_spike::reader_parse_one(
                &self.src as *const Sexp,
                &mut self.cursor_slot as *mut Sexp,
                &mut self.result_slot as *mut Sexp,
                &self.pool_slot as *const Sexp,
                0,
            )
        };
        if status != 1 {
            return None;
        }
        let result = std::mem::replace(&mut self.result_slot, Sexp::Nil);
        Some(ElispParseOutcome::Form(result))
    }

    fn has_trailing_token(&mut self) -> bool {
        let saved = self.cursor();
        let kind = self.lex_peek_advancing();
        let trailing = kind != 0;
        self.cursor_slot = Sexp::Int(saved);
        trailing
    }
}

unsafe fn vector_slot_ptr(pool_slot: *const Sexp, index: usize) -> *const Sexp {
    match &*pool_slot {
        Sexp::Vector(v) => {
            let slice: &[Sexp] = v.value.as_slice();
            &slice[index] as *const Sexp
        }
        _ => unreachable!("pool_slot must be a Sexp::Vector"),
    }
}
