use escape8259::unescape;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{digit0, digit1, multispace0, multispace1, one_of},
    combinator::{all_consuming, map, opt, recognize, value},
    error::{ErrorKind, ParseError},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, separated_pair, tuple},
    IResult,
};

#[derive(PartialEq, Debug, Clone)]
pub enum Node {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    Str(String),
    Array(Vec<Node>),
    Object(Vec<(String, Node)>),
}

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum JSONParseError {
    #[error("Bad integer")]
    BadInt,
    #[error("Bad float")]
    BadFloat,
    #[error("Bad escape sequence")]
    BadEscape,
    #[error("Unknown parser error")]
    Unparseable,
}

impl<I> ParseError<I> for JSONParseError {
    fn from_error_kind(_input: I, _kind: ErrorKind) -> Self {
        JSONParseError::Unparseable
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

// STRING

fn is_nonescaped_string_char(c: char) -> bool {
    let cv = c as u32;
    (cv > 0x20) && (cv != 0x22) && (cv != 0x5C)
}

fn nonescaped_string(input: &str) -> IResult<&str, &str, JSONParseError> {
    take_while1(
        is_nonescaped_string_char
    )
    (input)
}

fn escape_code(input: &str) -> IResult<&str, &str, JSONParseError> {
    recognize(
        pair(
            tag("\\"),
            alt((
                tag("\""),
                tag("\\"),
                tag("/"),
                tag("b"),
                tag("f"),
                tag("n"),
                tag("r"),
                tag("t"),
                tag("u"),
            ))
        )
    )
    (input)
}

fn string_body(input: &str) -> IResult<&str, &str, JSONParseError> {
    recognize(
        many0(
            alt((
                nonescaped_string, 
                escape_code, 
                multispace1
            ))
        )
    )
    (input)
}

fn string_literal(input: &str) -> IResult<&str, String, JSONParseError> {
    let (remain, raw_str) = delimited(
        tag("\""), 
        string_body, 
        tag("\"")
    )
    (input)?;

    match unescape(raw_str) {
        Ok(s) => Ok((remain, s)),
        Err(_) => Err(nom::Err::Failure(JSONParseError::BadEscape))
    }
}

fn json_string(input: &str) -> IResult<&str, Node, JSONParseError> {
    map(string_literal, |s| 
        Node::Str(s)
    )
    (input)
}

// NUMBER

fn digit1to9(input: &str) -> IResult<&str, char, JSONParseError> {
    one_of("123456789")
    (input)
}

fn uint(input: &str) -> IResult<&str, &str, JSONParseError> {
    alt((
        tag("0"), 
        recognize(
            pair(
                digit1to9, 
                digit0
            )
        )
    ))
    (input)
}

//FLOAT

fn frac(input: &str) -> IResult<&str, &str, JSONParseError> {
    recognize(
        pair(
            tag("."), 
            digit1
        )
    )
    (input)
}

fn exp(input: &str) -> IResult<&str, &str, JSONParseError> {
    recognize(
        tuple((
            tag("e"), 
            opt(
                alt((
                    tag("-"), 
                    tag("+"))
                )
            ), 
            digit1
        ))
    )
    (input)
}

fn float_body(input: &str) -> IResult<&str, &str, JSONParseError> {
    recognize(
        tuple((
            opt(tag("-")),
            uint,
            alt((
                recognize(pair(
                    frac,
                    opt(exp)
                )),
                exp
            )),
        ))
    )
    (input)
}

fn json_float(input: &str) -> IResult<&str, Node, JSONParseError> {
    let (remain, raw_float) = float_body(input)?;
    match raw_float.parse::<f64>() {
        Ok(f) => Ok((remain, Node::Float(f))),
        Err(_) => Err(nom::Err::Failure(JSONParseError::BadFloat)),
    }
}

// INTEGER

fn integer_body(input: &str) -> IResult<&str, &str, JSONParseError> {
    recognize(
        pair(
            opt(tag("-")),
            uint
        )
    )
    (input)
}

fn json_integer(input: &str) -> IResult<&str, Node, JSONParseError> {
    let (remain, raw_int) = integer_body(input)?;
    match raw_int.parse::<i64>() {
        Ok(i) => Ok((remain, Node::Integer(i))),
        Err(_) => Err(nom::Err::Failure(JSONParseError::BadInt))
    }
}

//BOOL

fn json_bool(input: &str) -> IResult<&str, Node, JSONParseError> {
    alt((
        value(Node::Bool(false), tag("false")),
        value(Node::Bool(true), tag("true")),
    ))
    (input)
}

// NULL

fn json_null(input: &str) -> IResult<&str, Node, JSONParseError> {
    value(
        Node::Null {}, 
        tag("null")
    )
    (input)
}

// ARRAY

fn json_array(input: &str) -> IResult<&str, Node, JSONParseError> {
    let parser = delimited(
        handle_whitespace(tag("[")),
        separated_list0(handle_whitespace(tag(",")), json_value),
        handle_whitespace(tag("]")),
    );
    map(parser, |s| Node::Array(s))(input)
}

// OBJECT

fn object_member(input: &str) -> IResult<&str, (String, Node), JSONParseError> {
    separated_pair(string_literal, handle_whitespace(tag(":")), json_value)
    (input)
}

fn json_object(input: &str) -> IResult<&str, Node, JSONParseError> {
    let parser = delimited(
        handle_whitespace(tag("{")),
        separated_list0(
            handle_whitespace(tag(",")), 
            object_member
        ),
        handle_whitespace(tag("}")),
    );
    map(parser, |s| 
        Node::Object(s)
    )
    (input)
}

fn json_value(input: &str) -> IResult<&str, Node, JSONParseError> {
    alt((
        json_string, 
        json_float, 
        json_integer, 
        json_bool, 
        json_null,
        json_array,
        json_object,
    ))
    (input)
}

fn handle_whitespace<F, I, O, E>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Fn(I) -> IResult<I, O, E>,
    I: nom::InputTakeAtPosition,
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
    E: nom::error::ParseError<I>,
{
    delimited(multispace0, f, multispace0)
}

pub fn parse_json(input: &str) -> Result<Node, JSONParseError> {
    let (_, result) = all_consuming(json_value)(input).map_err(|nom_err| {
        match nom_err {
            nom::Err::Incomplete(_) => unreachable!(),
            nom::Err::Error(e) => e,
            nom::Err::Failure(e) => e,
        }
    })?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        // Plain Unicode strings with no escaping
        assert_eq!(json_string(r#""""#), Ok(("", Node::Str("".into()))));
        assert_eq!(json_string(r#"" ""#), Ok(("", Node::Str(" ".into()))));
        assert_eq!(json_string(r#""Hello""#), Ok(("", Node::Str("Hello".into()))));
        assert_eq!(json_string(r#""の""#), Ok(("", Node::Str("の".into()))));
        assert_eq!(json_string(r#""𝄞""#), Ok(("", Node::Str("𝄞".into()))));

        // valid 2-character escapes
        //assert_eq!(json_string(r#""  \\  ""#), Ok(("", Node::Str("  \\  ".into()))));
        //assert_eq!(json_string(r#""  \"  ""#), Ok(("", Node::Str("  \"  ".into()))));

        // valid 6-character escapes
        assert_eq!(json_string(r#""\u0000""#), Ok(("", Node::Str("\x00".into()))));
        assert_eq!(json_string(r#""\u00DF""#), Ok(("", Node::Str("ß".into()))));
        assert_eq!(json_string(r#""\uD834\uDD1E""#), Ok(("", Node::Str("𝄞".into()))));

        // Invalid because surrogate characters must come in pairs
        assert!(json_string(r#""\ud800""#).is_err());
        // Unknown 2-character escape
        assert!(json_string(r#""\x""#).is_err());
        // Not enough hex digits
        assert!(json_string(r#""\u""#).is_err());
        assert!(json_string(r#""\u001""#).is_err());
        // Naked control character
        assert!(json_string(r#""\x0a""#).is_err());
        // Not a JSON string because it's not wrapped in quotes
        assert!(json_string("abc").is_err());
    }

    #[test]
    fn test_integer() {
        assert_eq!(json_integer("42"), Ok(("", Node::Integer(42))));
        assert_eq!(json_integer("-123"), Ok(("", Node::Integer(-123))));
        assert_eq!(json_integer("0"), Ok(("", Node::Integer(0))));
        assert_eq!(json_integer("01"), Ok(("1", Node::Integer(0))));
    }

    #[test]
    fn test_float() {
        assert_eq!(json_float("4.2"), Ok(("", Node::Float(4.2))));
        assert_eq!(json_float("-4.2"), Ok(("", Node::Float(-4.2))));
        assert_eq!(json_float("42e-1"), Ok(("", Node::Float(4.2))));
    }

    #[test]
    fn test_bool() {
        assert_eq!(json_bool("false"), Ok(("", Node::Bool(false))));
        assert_eq!(json_bool("true"), Ok(("", Node::Bool(true))));
        assert!(json_bool("foo").is_err());
    }

    #[test]
    fn test_null() {
        assert_eq!(json_null("null"), Ok(("", Node::Null)));
    }

    #[test]
    fn test_literal() {
        assert_eq!(json_value("56"), Ok(("", Node::Integer(56))));
        assert_eq!(json_value("78.0"), Ok(("", Node::Float(78.0))));
    }

    #[test]
    fn test_array() {
        assert_eq!(json_array("[]"), Ok(("", Node::Array(vec![]))));
        assert_eq!(json_array("[1]"), Ok(("", Node::Array(vec![Node::Integer(1)]))));

        let expected = Node::Array(vec![Node::Integer(1), Node::Integer(2)]);
        assert_eq!(json_array("[  1  ,  2  ]  "), Ok(("", expected)));
    }

    #[test]
    fn test_object() {
        let expected = Node::Object(vec![(String::from("test"), Node::Integer(1))]);
        assert_eq!(json_object(r#"{  "test"  :  1  }"#), Ok(("", expected)));

        let expected = Node::Object(vec![
            (String::from("int"), Node::Integer(1)),
            (String::from("str"), Node::Str(String::from("test"))),
        ]);
        assert_eq!(
            json_object(r#"{  "int": 1, "str": "test"  }"#),
            Ok(("", expected))
        );
    }

    #[test]
    fn test_json() {
        let expected = Node::Object(vec![(String::from("test"), Node::Integer(1))]);
        assert_eq!(parse_json(r#"{  "test"  :  1  }"#), Ok(expected));
    }
}
