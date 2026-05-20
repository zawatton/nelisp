mod error;
pub use error::{ReadError, SourcePos};
pub use crate::eval::sexp::{fmt_sexp, Sexp};
use crate::elisp_cc_spike;

const PARSER_POOL_SIZE: usize = 3 + 4 * 1024;
const SCRATCH_CAP: i64 = 64;
const NYI_MSG: &str = "feature unsupported by elisp Reader (record `#s(..)', byte-code `#[..]', or syntax error)";
const START_POS: SourcePos = SourcePos { line: 1, col: 1 };

pub fn read_str(input: &str) -> Result<Sexp, ReadError> {
    let mut state = ElispReadState::new(input);
    let form = match state.parse_one_form() {
        Some(ElispParseOutcome::Form(f)) => f,
        Some(ElispParseOutcome::Empty) => return Err(ReadError::unexpected_eof("empty input", START_POS)),
        None => return Err(ReadError::not_yet_implemented(NYI_MSG, START_POS)),
    };
    if state.has_trailing_token() { return Err(ReadError::parse("trailing token after first form".to_string(), START_POS)); }
    Ok(form)
}
pub fn read_all(input: &str) -> Result<Vec<Sexp>, ReadError> {
    let mut state = ElispReadState::new(input); let mut forms = Vec::new();
    loop {
        match state.parse_one_form() {
            Some(ElispParseOutcome::Form(form)) => forms.push(form),
            Some(ElispParseOutcome::Empty) => return Ok(forms),
            None => return Err(ReadError::not_yet_implemented(NYI_MSG, START_POS)),
        }
    }
}

enum ElispParseOutcome { Form(Sexp), Empty }
struct ElispReadState { src: Sexp, cursor_slot: Sexp, result_slot: Sexp, pool_slot: Sexp }

impl ElispReadState {
    fn new(input: &str) -> Self {
        let mut slots = vec![Sexp::Nil; PARSER_POOL_SIZE];
        slots[0] = Sexp::mut_str(String::with_capacity(SCRATCH_CAP as usize));
        ElispReadState { src: Sexp::Str(input.to_string()), cursor_slot: Sexp::Int(0), result_slot: Sexp::Nil, pool_slot: Sexp::vector(slots) }
    }
    fn cursor(&self) -> i64 { match self.cursor_slot { Sexp::Int(n) => n, _ => 0 } }
    fn lex_peek_advancing(&mut self) -> i64 {
        let cursor = self.cursor();
        unsafe {
            let Sexp::Vector(v) = &self.pool_slot else { unreachable!() };
            let sl = v.value.as_slice();
            let scratch_ptr = &sl[0] as *const Sexp as *mut Sexp;
            let payload_ptr = &sl[1] as *const Sexp as *mut Sexp;
            elisp_cc_spike::mut_str_make_empty(scratch_ptr, SCRATCH_CAP);
            elisp_cc_spike::reader_lex_one(&self.src as *const Sexp, cursor, payload_ptr, &mut self.cursor_slot as *mut Sexp, scratch_ptr)
        }
    }
    fn peek_token_kind(&mut self) -> i64 { let saved = self.cursor(); let kind = self.lex_peek_advancing(); self.cursor_slot = Sexp::Int(saved); kind }
    fn parse_one_form(&mut self) -> Option<ElispParseOutcome> {
        let saved = self.cursor(); let kind = self.lex_peek_advancing();
        if kind == 0 { return Some(ElispParseOutcome::Empty); }
        self.cursor_slot = Sexp::Int(saved); self.result_slot = Sexp::Nil;
        let status = unsafe { elisp_cc_spike::reader_parse_one(&self.src as *const Sexp, &mut self.cursor_slot as *mut Sexp, &mut self.result_slot as *mut Sexp, &self.pool_slot as *const Sexp, 0) };
        if status != 1 { return None; }
        Some(ElispParseOutcome::Form(std::mem::replace(&mut self.result_slot, Sexp::Nil)))
    }
    fn has_trailing_token(&mut self) -> bool { self.peek_token_kind() != 0 }
}
