use json_parser::parse_json;

const JSON: &str = r#"{ "str": "test", "int": 1, "float": 1.5, "array": ["1", "2", "3"] }"#;

fn main() {
    let j = parse_json(JSON);
    println!("{:?}", j);
}
