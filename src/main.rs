use std::env::args;

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        println!("json-schema-dsl \"User{{id: int}}\"");
        return;
    }
    let schema = args[1].as_str();
    let json_schema = json_schema_dsl::to_json_schema(schema).unwrap();
    println!("{}", serde_json::to_string_pretty(&json_schema).unwrap());
}