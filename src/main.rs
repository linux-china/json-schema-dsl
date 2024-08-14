use std::env::args;

const VERSION: &str = "0.2.1";

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        println!("json-schema-dsl \"User{{id: int}}\"");
        return;
    }
    if args[1] == "-v" || args[1] == "--version" {
        println!("json-schema-dsl {}", VERSION);
        return;
    }
    let schema = args[1].as_str();
    let json_schema = json_schema_dsl::to_json_schema(schema).unwrap();
    println!("{}", serde_json::to_string_pretty(&json_schema).unwrap());
}
