use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{digit0, digit1, multispace0, multispace1, one_of},
    combinator::{map, map_res, opt, recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, separated_pair, tuple},
    IResult,
};
use escape8259::unescape;

#[derive(PartialEq, Debug, Clone)]
pub enum Node {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    Str(String),
    Array(Vec<Node>),
    Object(Vec<(String, Node)>)
}

// STRING

fn is_nonescaped_string_char(c: char) -> bool {
    let cv = c as u32;
    (cv > 0x20) && (cv != 0x22) && (cv != 0x5C)
}

fn nonescaped_string(input: &str) -> IResult<&str, &str> {
    take_while1(is_nonescaped_string_char)
    (input)
}

fn escape_code(input: &str) -> IResult<&str, &str> {
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

fn string_body(input: &str) -> IResult<&str, &str> {
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

fn string_literal(input: &str) -> IResult<&str, String> {
    let parser = delimited(
        tag("\""), 
        string_body,
        tag("\"")
    );
    map_res(parser, |s| {
        unescape(s)
    })
    (input)
}

fn json_string(input: &str) -> IResult<&str, Node> {
    map(string_literal, |s| {
        Node::Str(s)
    })
    (input)
}

// NUMBER

fn digit1to9(input: &str) -> IResult<&str, char> {
    one_of("123456789")
    (input)
}

fn uint(input: &str) -> IResult<&str, &str> {
    alt((
        tag("0"), 
        recognize(
            pair(digit1to9, digit0)
        ))
    )
    (input)
}

//FLOAT

fn frac(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(tag("."), digit1)
    )
    (input)
}

fn exp(input: &str) -> IResult<&str, &str> {
    recognize(
        tuple((
            tag("e"), 
            opt(
                alt((
                    tag("-"), 
                    tag("+")
                ))
            ), 
            digit1
        ))
    )
    (input)
}

fn json_float(input: &str) -> IResult<&str, Node> {
    let parser = recognize(
        tuple((
            opt(tag("-")),
            uint,
            alt((
                recognize(pair(
                    frac,
                    opt(exp)
                )),
                exp
            ))
        ))
    );
    map(parser, |s| {
        let n = s.parse::<f64>().unwrap();
        Node::Float(n)
    })
    (input)
}

// INTEGER

fn json_integer(input: &str) -> IResult<&str, Node> {
    let parser = recognize(
        pair(opt(
            tag("-")
        ), uint
    ));
    map(parser, |s| {
        let n = s.parse::<i64>().unwrap();
        Node::Integer(n)
    })(input)
}

//BOOL

fn json_bool(input: &str) -> IResult<&str, Node> {
    alt((
        value(Node::Bool(false), tag("false")),
        value(Node::Bool(true), tag("true")),
    ))(input)
}

// NULL

fn json_null(input: &str) -> IResult<&str, Node> {
    value(Node::Null {}, tag("null"))(input)
}

fn json_value(input: &str) -> IResult<&str, Node> {
    alt((
        json_string,
        json_float,
        json_integer,
        json_bool,
        json_null
    ))
    (input)
}

// ARRAY 

fn json_array(input: &str) -> IResult<&str, Node> {
    let parser = delimited(
        handle_whitespace(tag("[")), 
        separated_list0(
            handle_whitespace(tag(",")), 
            json_value
        ), 
        handle_whitespace(tag("]")));
    map(parser, |s| {
        Node::Array(s)
    })
    (input)
}

// OBJECT

fn object_member(input: &str) -> IResult<&str, (String, Node)> {
    separated_pair(string_literal, handle_whitespace(tag(":")), json_value)
    (input)
}

fn json_object(input: &str) -> IResult<&str, Node> {
    let parser = delimited(
        handle_whitespace(tag("{")), 
        separated_list0(
            handle_whitespace(tag(",")), 
            object_member
        ), 
        handle_whitespace(tag("}"))
    );
    map(parser, |s| {
        Node::Object(s)
    })
    (input)
}

fn handle_whitespace<F, I, O, E>(f: F) -> impl FnMut(I) -> IResult<I, O, E> 
where 
    F: Fn(I) -> IResult<I, O, E>, 
    I: nom::InputTakeAtPosition,
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
    E: nom::error::ParseError<I>,
{
    delimited(multispace0, f , multispace0)
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

        let expected = Node::Object(vec![(String::from("int"), Node::Integer(1)), (String::from("str"), Node::Str(String::from("test")))]);
        assert_eq!(json_object(r#"{  "int": 1, "str": "test"  }"#), Ok(("", expected)));
    }
}
