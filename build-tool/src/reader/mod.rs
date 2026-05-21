mod error;
pub use error::{ReadError, SourcePos};
pub use crate::eval::sexp::{fmt_sexp, Sexp};
use crate::elisp_cc_spike;
const PARSER_POOL_SIZE: usize = 3 + 4 * 1024;
const SCRATCH_CAP: i64 = 64;
const NYI_MSG: &str = "feature unsupported by elisp Reader (record `#s(..)', byte-code `#[..]', or syntax error)";
const START_POS: SourcePos = SourcePos { line: 1, col: 1 };
pub fn read_str(input: &str) -> Result<Sexp, ReadError> { let mut s = ElispReadState::new(input); let f = match s.parse_one_form() { Some(ElispParseOutcome::Form(f)) => f, Some(ElispParseOutcome::Empty) => return Err(ReadError::UnexpectedEof("empty input".into(), START_POS)), None => return Err(ReadError::NotYetImplemented(NYI_MSG.into(), START_POS)) }; if s.has_trailing_token() { return Err(ReadError::Parse("trailing token after first form".into(), START_POS)); } Ok(f) }
pub fn read_all(input: &str) -> Result<Vec<Sexp>, ReadError> { let mut s = ElispReadState::new(input); let mut forms = Vec::new(); loop { match s.parse_one_form() { Some(ElispParseOutcome::Form(f)) => forms.push(f), Some(ElispParseOutcome::Empty) => return Ok(forms), None => return Err(ReadError::NotYetImplemented(NYI_MSG.into(), START_POS)) } } }
enum ElispParseOutcome { Form(Sexp), Empty }
struct ElispReadState { src: Sexp, cursor_slot: Sexp, result_slot: Sexp, pool_slot: Sexp }
impl ElispReadState {
    fn new(input: &str) -> Self { let mut slots = vec![Sexp::Nil; PARSER_POOL_SIZE]; slots[0] = Sexp::mut_str(String::with_capacity(SCRATCH_CAP as usize)); ElispReadState { src: Sexp::Str(input.to_string()), cursor_slot: Sexp::Int(0), result_slot: Sexp::Nil, pool_slot: Sexp::vector(slots) } }
    fn cursor(&self) -> i64 { match self.cursor_slot { Sexp::Int(n) => n, _ => 0 } }
    fn lex_advancing(&mut self) -> i64 { let cur = self.cursor(); unsafe { let Sexp::Vector(v) = &self.pool_slot else { unreachable!() }; let sl = v.value.as_slice(); let sp = &sl[0] as *const Sexp as *mut Sexp; let pp = &sl[1] as *const Sexp as *mut Sexp; elisp_cc_spike::mut_str_make_empty(sp, SCRATCH_CAP); elisp_cc_spike::reader_lex_one(&self.src, cur, pp, &mut self.cursor_slot, sp) } }
    fn peek_kind(&mut self) -> i64 { let s = self.cursor(); let k = self.lex_advancing(); self.cursor_slot = Sexp::Int(s); k }
    fn parse_one_form(&mut self) -> Option<ElispParseOutcome> { let saved = self.cursor(); if self.lex_advancing() == 0 { return Some(ElispParseOutcome::Empty); } self.cursor_slot = Sexp::Int(saved); self.result_slot = Sexp::Nil; let ok = unsafe { elisp_cc_spike::reader_parse_one(&self.src, &mut self.cursor_slot, &mut self.result_slot, &self.pool_slot, 0) } == 1; ok.then(|| ElispParseOutcome::Form(std::mem::replace(&mut self.result_slot, Sexp::Nil))) }
    fn has_trailing_token(&mut self) -> bool { self.peek_kind() != 0 }
}
