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
    let mut state = ElispReadState::new(input);
    let form = match state.parse_one_form() {
        Some(ElispParseOutcome::Form(f)) => f,
        Some(ElispParseOutcome::Empty) => return Err(ReadError::unexpected_eof("empty input", START_POS)),
        None => return Err(ReadError::not_yet_implemented(NYI_MSG, START_POS)),
    };
    if state.has_trailing_token() {
        return Err(ReadError::parse("trailing token after first form".to_string(), START_POS));
    }
    Ok(form)
}

pub fn read_all(input: &str) -> Result<Vec<Sexp>, ReadError> {
    let mut state = ElispReadState::new(input);
    let mut forms = Vec::new();
    loop {
        match state.parse_one_form() {
            Some(ElispParseOutcome::Form(form)) => forms.push(form),
            Some(ElispParseOutcome::Empty) => return Ok(forms),
            None => return Err(ReadError::not_yet_implemented(NYI_MSG, START_POS)),
        }
    }
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
        let mut slots = vec![Sexp::Nil; PARSER_POOL_SIZE];
        slots[0] = Sexp::mut_str(String::with_capacity(SCRATCH_CAP as usize));
        ElispReadState {
            src: Sexp::Str(input.to_string()),
            cursor_slot: Sexp::Int(0),
            result_slot: Sexp::Nil,
            pool_slot: Sexp::vector(slots),
        }
    }

    fn cursor(&self) -> i64 {
        match self.cursor_slot {
            Sexp::Int(n) => n,
            _ => 0,
        }
    }

    fn lex_peek_advancing(&mut self) -> i64 {
        let cursor = self.cursor();
        unsafe {
            let pool_ptr = &self.pool_slot as *const Sexp;
            let scratch_ptr = vector_slot_ptr(pool_ptr, 0) as *mut Sexp;
            let payload_ptr = vector_slot_ptr(pool_ptr, 1) as *mut Sexp;
            let cursor_out_ptr = &mut self.cursor_slot as *mut Sexp;
            elisp_cc_spike::mut_str_make_empty(scratch_ptr, SCRATCH_CAP);
            elisp_cc_spike::reader_lex_one(&self.src as *const Sexp, cursor, payload_ptr, cursor_out_ptr, scratch_ptr)
        }
    }

    fn peek_token_kind(&mut self) -> i64 {
        let saved = self.cursor();
        let kind = self.lex_peek_advancing();
        self.cursor_slot = Sexp::Int(saved);
        kind
    }

    fn parse_one_form(&mut self) -> Option<ElispParseOutcome> {
        // Advance past whitespace/comments; if EOF return Empty.
        let saved = self.cursor();
        let kind = self.lex_peek_advancing();
        if kind == 0 { return Some(ElispParseOutcome::Empty); }
        self.cursor_slot = Sexp::Int(saved); // restore — parser re-lexes from here
        self.result_slot = Sexp::Nil;
        let status = unsafe {
            elisp_cc_spike::reader_parse_one(
                &self.src as *const Sexp, &mut self.cursor_slot as *mut Sexp,
                &mut self.result_slot as *mut Sexp, &self.pool_slot as *const Sexp, 0,
            )
        };
        if status != 1 { return None; }
        Some(ElispParseOutcome::Form(std::mem::replace(&mut self.result_slot, Sexp::Nil)))
    }

    fn has_trailing_token(&mut self) -> bool { self.peek_token_kind() != 0 }
}

unsafe fn vector_slot_ptr(pool_slot: *const Sexp, index: usize) -> *const Sexp {
    let Sexp::Vector(v) = &*pool_slot else { unreachable!("pool_slot must be a Sexp::Vector") };
    &v.value.as_slice()[index] as *const Sexp
}
