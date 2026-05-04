//! Phase 8.x image-format walking skeleton.
//!
//! This is intentionally small: an image is a header plus a vector of
//! real evaluator [`Sexp`] values.  Loading an image returns the same
//! value universe the Rust Elisp evaluator already executes, so the
//! smoke path proves `bootstrap.el -> image -> eval` without inventing
//! a second object model.

use crate::eval::{self, Env, EvalError};
use crate::reader::{self, ReadError, Sexp};
use std::fmt;

pub const IMAGE_MAGIC: &[u8; 8] = b"NELIMG\0\x01";
pub const IMAGE_ABI_VERSION: u32 = 1;

const TAG_NIL: u8 = 0x00;
const TAG_T: u8 = 0x01;
const TAG_INT: u8 = 0x02;
const TAG_FLOAT: u8 = 0x03;
const TAG_SYMBOL: u8 = 0x04;
const TAG_STRING: u8 = 0x05;
const TAG_CONS: u8 = 0x06;
const TAG_VECTOR: u8 = 0x07;

#[derive(Debug)]
pub enum ImageError {
    BadMagic,
    UnsupportedVersion(u32),
    UnexpectedEof(&'static str),
    InvalidUtf8(String),
    UnknownTag(u8),
    TrailingBytes(usize),
    LengthOverflow,
    Read(ReadError),
    Eval(EvalError),
}

impl fmt::Display for ImageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImageError::BadMagic => f.write_str("bad NeLisp image magic"),
            ImageError::UnsupportedVersion(v) => write!(f, "unsupported image ABI version {}", v),
            ImageError::UnexpectedEof(ctx) => write!(f, "truncated image while reading {}", ctx),
            ImageError::InvalidUtf8(ctx) => write!(f, "invalid UTF-8 in {}", ctx),
            ImageError::UnknownTag(tag) => write!(f, "unknown image value tag 0x{tag:02x}"),
            ImageError::TrailingBytes(n) => write!(f, "image has {} trailing bytes", n),
            ImageError::LengthOverflow => f.write_str("image length exceeds ABI u32 field"),
            ImageError::Read(e) => write!(f, "reader error: {}", e),
            ImageError::Eval(e) => write!(f, "eval error: {}", e),
        }
    }
}

impl std::error::Error for ImageError {}

impl From<ReadError> for ImageError {
    fn from(value: ReadError) -> Self {
        ImageError::Read(value)
    }
}

impl From<EvalError> for ImageError {
    fn from(value: EvalError) -> Self {
        ImageError::Eval(value)
    }
}

pub fn compile_elisp_to_image(source: &str) -> Result<Vec<u8>, ImageError> {
    let forms = reader::read_all(source)?;
    encode_image(&forms)
}

pub fn encode_image(forms: &[Sexp]) -> Result<Vec<u8>, ImageError> {
    let mut out = Vec::new();
    out.extend_from_slice(IMAGE_MAGIC);
    out.extend_from_slice(&IMAGE_ABI_VERSION.to_le_bytes());
    write_len(&mut out, forms.len())?;
    for form in forms {
        encode_value(&mut out, form)?;
    }
    Ok(out)
}

pub fn decode_image(bytes: &[u8]) -> Result<Vec<Sexp>, ImageError> {
    let mut rd = Reader { bytes, pos: 0 };
    let magic = rd.read_exact(IMAGE_MAGIC.len(), "magic")?;
    if magic != IMAGE_MAGIC {
        return Err(ImageError::BadMagic);
    }
    let version = rd.read_u32("version")?;
    if version != IMAGE_ABI_VERSION {
        return Err(ImageError::UnsupportedVersion(version));
    }
    let count = rd.read_u32("form count")? as usize;
    let mut forms = Vec::with_capacity(count);
    for _ in 0..count {
        forms.push(decode_value(&mut rd)?);
    }
    if rd.pos != bytes.len() {
        return Err(ImageError::TrailingBytes(bytes.len() - rd.pos));
    }
    Ok(forms)
}

pub fn eval_image(bytes: &[u8]) -> Result<Sexp, ImageError> {
    let forms = decode_image(bytes)?;
    eval_forms(&forms)
}

pub fn eval_forms(forms: &[Sexp]) -> Result<Sexp, ImageError> {
    let mut env = Env::new_global();
    let mut last = Sexp::Nil;
    for form in forms {
        last = eval::eval(form, &mut env)?;
    }
    Ok(last)
}

fn encode_value(out: &mut Vec<u8>, value: &Sexp) -> Result<(), ImageError> {
    match value {
        Sexp::Nil => out.push(TAG_NIL),
        Sexp::T => out.push(TAG_T),
        Sexp::Int(n) => {
            out.push(TAG_INT);
            out.extend_from_slice(&n.to_le_bytes());
        }
        Sexp::Float(x) => {
            out.push(TAG_FLOAT);
            out.extend_from_slice(&x.to_bits().to_le_bytes());
        }
        Sexp::Symbol(name) => {
            out.push(TAG_SYMBOL);
            write_string(out, name)?;
        }
        Sexp::Str(text) => {
            out.push(TAG_STRING);
            write_string(out, text)?;
        }
        Sexp::MutStr(rc) => {
            // Image format flattens MutStr into the same TAG_STRING
            // payload — round-trip drops the mutable identity but keeps
            // the textual content.  Substrate use cases for
            // `compile-image' / `eval-image' do not depend on
            // post-load aset behavior.
            out.push(TAG_STRING);
            let text = rc.borrow();
            write_string(out, &text)?;
        }
        Sexp::Cons(car, cdr) => {
            out.push(TAG_CONS);
            encode_value(out, &car.borrow())?;
            encode_value(out, &cdr.borrow())?;
        }
        Sexp::Vector(items) => {
            out.push(TAG_VECTOR);
            let borrowed = items.borrow();
            write_len(out, borrowed.len())?;
            for item in borrowed.iter() {
                encode_value(out, item)?;
            }
        }
        Sexp::HashTable(_) => {
            // Hash-tables are runtime-only objects in our minimal
            // image format (= they cannot survive `compile-image' /
            // `eval-image' round-trips).  Surface as the closest
            // existing variant so users see a clear failure.
            return Err(ImageError::Eval(EvalError::NotImplemented(
                "hash-table values are not yet supported by image encoding".into(),
            )));
        }
        Sexp::CharTable(_) => {
            return Err(ImageError::Eval(EvalError::NotImplemented(
                "char-table values are not yet supported by image encoding".into(),
            )));
        }
        Sexp::BoolVector(_) => {
            return Err(ImageError::Eval(EvalError::NotImplemented(
                "bool-vector values are not yet supported by image encoding".into(),
            )));
        }
    }
    Ok(())
}

fn decode_value(rd: &mut Reader<'_>) -> Result<Sexp, ImageError> {
    let tag = rd.read_u8("value tag")?;
    match tag {
        TAG_NIL => Ok(Sexp::Nil),
        TAG_T => Ok(Sexp::T),
        TAG_INT => Ok(Sexp::Int(rd.read_i64("integer")?)),
        TAG_FLOAT => Ok(Sexp::Float(f64::from_bits(rd.read_u64("float")?))),
        TAG_SYMBOL => Ok(Sexp::Symbol(rd.read_string("symbol")?)),
        TAG_STRING => Ok(Sexp::Str(rd.read_string("string")?)),
        TAG_CONS => {
            let car = decode_value(rd)?;
            let cdr = decode_value(rd)?;
            Ok(Sexp::cons(car, cdr))
        }
        TAG_VECTOR => {
            let len = rd.read_u32("vector length")? as usize;
            let mut items = Vec::with_capacity(len);
            for _ in 0..len {
                items.push(decode_value(rd)?);
            }
            Ok(Sexp::vector(items))
        }
        other => Err(ImageError::UnknownTag(other)),
    }
}

fn write_string(out: &mut Vec<u8>, value: &str) -> Result<(), ImageError> {
    write_len(out, value.len())?;
    out.extend_from_slice(value.as_bytes());
    Ok(())
}

fn write_len(out: &mut Vec<u8>, len: usize) -> Result<(), ImageError> {
    let len = u32::try_from(len).map_err(|_| ImageError::LengthOverflow)?;
    out.extend_from_slice(&len.to_le_bytes());
    Ok(())
}

struct Reader<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn read_exact(&mut self, len: usize, ctx: &'static str) -> Result<&'a [u8], ImageError> {
        let end = self
            .pos
            .checked_add(len)
            .ok_or(ImageError::UnexpectedEof(ctx))?;
        if end > self.bytes.len() {
            return Err(ImageError::UnexpectedEof(ctx));
        }
        let slice = &self.bytes[self.pos..end];
        self.pos = end;
        Ok(slice)
    }

    fn read_u8(&mut self, ctx: &'static str) -> Result<u8, ImageError> {
        Ok(self.read_exact(1, ctx)?[0])
    }

    fn read_u32(&mut self, ctx: &'static str) -> Result<u32, ImageError> {
        let mut buf = [0u8; 4];
        buf.copy_from_slice(self.read_exact(4, ctx)?);
        Ok(u32::from_le_bytes(buf))
    }

    fn read_i64(&mut self, ctx: &'static str) -> Result<i64, ImageError> {
        let mut buf = [0u8; 8];
        buf.copy_from_slice(self.read_exact(8, ctx)?);
        Ok(i64::from_le_bytes(buf))
    }

    fn read_u64(&mut self, ctx: &'static str) -> Result<u64, ImageError> {
        let mut buf = [0u8; 8];
        buf.copy_from_slice(self.read_exact(8, ctx)?);
        Ok(u64::from_le_bytes(buf))
    }

    fn read_string(&mut self, ctx: &'static str) -> Result<String, ImageError> {
        let len = self.read_u32("string length")? as usize;
        let bytes = self.read_exact(len, ctx)?;
        String::from_utf8(bytes.to_vec()).map_err(|_| ImageError::InvalidUtf8(ctx.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reader::fmt_sexp;

    #[test]
    fn phase4_6a_header_and_version_are_checked() {
        assert!(matches!(decode_image(b"not-image"), Err(ImageError::BadMagic)));

        let mut image = encode_image(&[Sexp::Int(1)]).unwrap();
        image[8..12].copy_from_slice(&999u32.to_le_bytes());
        assert!(matches!(
            decode_image(&image),
            Err(ImageError::UnsupportedVersion(999))
        ));
    }

    #[test]
    fn phase4_6b_atoms_round_trip_through_image_abi() {
        let forms = vec![
            Sexp::Nil,
            Sexp::T,
            Sexp::Int(-42),
            Sexp::Float(3.5),
            Sexp::Symbol("answer".into()),
            Sexp::Str("hello\nimage".into()),
        ];
        let image = encode_image(&forms).unwrap();
        assert_eq!(decode_image(&image).unwrap(), forms);
    }

    #[test]
    fn phase4_6c_composite_values_reload_as_mutable_elisp_values() {
        let forms = vec![
            Sexp::list_from(&[Sexp::Symbol("a".into()), Sexp::Int(1)]),
            Sexp::cons(Sexp::Int(7), Sexp::Symbol("tail".into())),
            Sexp::vector(vec![Sexp::Str("slot".into()), Sexp::Int(2)]),
        ];
        let image = encode_image(&forms).unwrap();
        let loaded = decode_image(&image).unwrap();
        assert_eq!(loaded, forms);

        match &loaded[0] {
            Sexp::Cons(car, _) => *car.borrow_mut() = Sexp::Symbol("changed".into()),
            other => panic!("expected cons, got {:?}", other),
        }
        assert_eq!(fmt_sexp(&loaded[0]), "(changed 1)");
    }

    #[test]
    fn phase4_6d_bootstrap_el_compiles_to_image_and_evaluates() {
        let bootstrap = r#"
          (defun bootstrap-add (x) (+ x 1))
          (defun bootstrap-main ()
            (let ((cell (cons 1 2))
                  (vec (vector "image" 41)))
              (setcar cell (bootstrap-add (car cell)))
              (list (car cell) (aref vec 0) (bootstrap-add (aref vec 1)))))
          (bootstrap-main)
        "#;
        let image = compile_elisp_to_image(bootstrap).unwrap();
        let value = eval_image(&image).unwrap();
        assert_eq!(fmt_sexp(&value), "(2 \"image\" 42)");
    }

    #[test]
    fn phase4_6e_image_forms_are_real_elisp_values_before_eval() {
        let image = compile_elisp_to_image("(quote (1 . [2 3]))").unwrap();
        let forms = decode_image(&image).unwrap();
        assert_eq!(fmt_sexp(&forms[0]), "'(1 . [2 3])");
        assert_eq!(fmt_sexp(&eval_forms(&forms).unwrap()), "(1 . [2 3])");
    }
}
