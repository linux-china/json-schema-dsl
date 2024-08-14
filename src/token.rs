use std::str::FromStr;
use indexmap::IndexMap;
use logos::{Lexer, Logos};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

const FORMAT_NAMES: &'static [&'static str] = &["Date", "Time", "DateTime", "Timestamp", "Interval", "Duration", "Email", "Ipv4", "Ipv6", "Uri", "Hostname", "Uuid", "UUID", "Json", "JSON", "Xml", "XML"];
const NUMBER_NAMES: &'static [&'static str] = &["price", "rate", "height", "width", "weight", "amount", "total", "percent", "ratio"];
const INTEGER_NAMES: &'static [&'static str] = &["age", "year", "count", "size", "length", "delay", "time", "duration", "level", "index", "position", "order", "size", "limit", "offset", "page", "quantity", "capacity", "interval", "retries", "max", "min"];
const BOOLEAN_NAMES: &'static [&'static str] = &["has", "is", "does", "allow", "should", "if", "can", "may", "will", "must"];

fn array_type_callback(lex: &mut Lexer<Token>) -> (String, String, String) {
    let complex_type = lex.slice().to_owned();
    let offset = complex_type.find('<').unwrap();
    let container_type = complex_type[..offset].to_owned();
    let end_offset = complex_type.rfind('>').unwrap();
    let item_type = complex_type[offset + 1..end_offset].to_owned();
    let range_type = complex_type[end_offset + 1..].trim();
    (container_type, item_type, range_type.to_string())
}

#[derive(Debug, Logos)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum ArrayToken {
    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    #[token(",")]
    Comma,

    #[regex(r"-?(?:0|[1-9]\d*)?",
    |lex| lex.slice().parse::<i64>().unwrap())]
    Integer(i64),

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)(?:[eE][+-]?\d+)?",
    |lex| lex.slice().parse::<f64>().unwrap())]
    Number(f64),

    #[regex(r#"'([^']*)'"#, |lex| lex.slice().to_owned())]
    Text1(String),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned())]
    Text2(String),
}

#[derive(Debug, Logos)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Token {
    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("...")]
    Ellipsis,

    #[regex(r#"(integer|int|long|bigint|number|float|double|real|decimal)\([^)]+\)"#,
    |lex| lex.slice().to_owned())]
    RangeType(String),

    #[regex(r#"(string|bytes|varchar|String|Text)\([^)]+\)"#,
    |lex| lex.slice().to_owned())]
    StringLengthType(String),

    #[regex(r#"\[[^]]+\]"#, |lex| lex.slice().to_owned())]
    TupleType(String),

    #[regex("integer|Integer|int|long|bigint|serial|bigserial|number|Number|float|double|real|decimal|boolean|Boolean|bool|string|bytes|bytea|varchar|String|Text",
    |lex| lex.slice().to_owned())]
    PrimitiveType(String),

    #[regex("Date|Time|DateTime|Duration|Email|Ipv4|Ipv6|Uri|Hostname|Uuid|UUID",
    |lex| lex.slice().to_owned())]
    FormatType(String),

    #[regex(r#"(List|list|Set|set|Array|array)<(integer|Integer|int|long|bigint|number|Number|float|double|real|decimal|boolean|Boolean|bool|string|bytes|bytea|varchar|String|Text|Date|Time|DateTime|Timestamp|Interval|Duration|Email|Ipv4|Ipv6|Uri|Hostname|Uuid|UUID)>(\([^)]+\))?"#,
        array_type_callback
    )]
    ArrayType((String, String, String)),

    #[regex("(integer|Integer|int|long|bigint|number|Number|float|double|real|decimal|boolean|Boolean|bool|string|bytes|bytea|varchar|String|Text|Date|Time|DateTime|Timestamp|Interval|Duration|Email|Ipv4|Ipv6|Uri|Hostname|Uuid|UUID)([|](integer|Integer|int|long|bigint|number|Number|float|double|real|decimal|boolean|Boolean|bool|string|bytes|bytea|varchar|String|Text|Date|Time|DateTime|Timestamp|Interval|Duration|Email|Ipv4|Ipv6|Uri|Hostname|Uuid|UUID))+",
    |lex| lex.slice().to_owned())]
    AnyOf(String),

    #[regex(r#"enum\([^)]+\)"#, |lex| lex.slice().to_owned())]
    EnumType(String),

    #[regex(r#"regex\(((['][^']+['])|([\"][^\"]+[\"]))\)"#, |lex| lex.slice().to_owned())]
    RegexType(String),

    #[regex("[A-Z][a-zA-Z0-9_]+", |lex| lex.slice().to_owned())]
    ObjectName(String),

    #[regex(r#"[a-z0-9][a-zA-Z0-9_]+[\?]?"#, |lex| lex.slice().to_owned())]
    FieldName(String),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonSchema {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "$schema")]
    pub version: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "$id")]
    pub id: Option<String>,
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(rename = "type")]
    pub type_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<IndexMap<String, JsonSchemaEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub required: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "additionalProperties")]
    pub additional_properties: Option<bool>,
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct JsonSchemaEntry {
    #[serde(skip)]
    pub name: String,
    #[serde(skip)]
    pub optional: bool,
    #[serde(rename = "type")]
    #[serde(skip_serializing_if = "String::is_empty")]
    pub type_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub format: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pattern: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "anyOf")]
    pub any_of: Option<Vec<JsonSchemaEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub items: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "minItems")]
    pub min_items: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "maxItems")]
    pub max_items: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "enum")]
    pub enums: Option<Vec<Value>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub minimum: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub maximum: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "minLength")]
    pub min_length: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "maxLength")]
    pub max_length: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "uniqueItems")]
    pub unique_items: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<IndexMap<String, JsonSchemaEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub required: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_properties: Option<bool>,
}

impl JsonSchema {
    pub fn new(title: &str) -> Self {
        JsonSchema {
            version: None,
            id: None,
            title: title.to_string(),
            description: None,
            type_name: "object".to_owned(),
            properties: Some(IndexMap::new()),
            required: None,
            additional_properties: None,
        }
    }
    pub fn version_2020(title: &str) -> Self {
        JsonSchema {
            version: Some("https://json-schema.org/draft/2020-12/schema".to_owned()),
            id: None,
            title: title.to_string(),
            description: None,
            type_name: "object".to_owned(),
            properties: Some(IndexMap::new()),
            required: None,
            additional_properties: None,
        }
    }
}

impl JsonSchemaEntry {
    pub fn new(type_name: &str) -> Self {
        JsonSchemaEntry {
            type_name: convert_to_json_type(type_name),
            ..Default::default()
        }
    }

    pub fn format(format_name: &str) -> Self {
        JsonSchemaEntry {
            type_name: "string".to_string(),
            format: Some(convert_to_json_format(format_name)),
            ..Default::default()
        }
    }

    pub fn revise(&mut self) {
        if self.type_name.is_empty() {
            let field_name = &self.name;
            if field_name.contains("time") || field_name.contains("_at") {
                self.type_name = "string".to_owned();
                self.format = Some("date-time".to_owned());
            } else if field_name.contains("date") {
                self.type_name = "string".to_owned();
                self.format = Some("date".to_owned());
            } else if BOOLEAN_NAMES.iter().any(|&item| field_name.starts_with(item)) {
                self.type_name = "boolean".to_owned();
            } else if NUMBER_NAMES.iter().any(|&item| field_name.contains(item)) {
                self.type_name = "number".to_owned();
            } else if INTEGER_NAMES.iter().any(|&item| field_name.contains(item)) {
                self.type_name = "integer".to_owned();
            } else {
                self.type_name = "string".to_owned();
            }
        }
    }

    pub fn add_entry(&mut self, name: &str, type_name: &str) {
        if self.properties.is_none() {
            self.properties = Some(IndexMap::new());
        }
        if let Some(ref mut properties) = self.properties {
            let entry = JsonSchemaEntry {
                type_name: type_name.to_string(),
                ..Default::default()
            };
            properties.insert(name.to_string(), entry);
        }
    }
}

pub fn to_json_schema(struct_text: &str) -> Result<JsonSchema, String> {
    let offset = struct_text.find('{').unwrap();
    let title = struct_text[0..offset].trim();
    let lexer_text = &struct_text[offset..].trim().trim_matches(&['{', '}']);
    let mut json_schema = JsonSchema::version_2020(title);
    let mut entries: IndexMap<String, JsonSchemaEntry> = IndexMap::new();
    let mut entry: JsonSchemaEntry = Default::default();
    let mut parent_entries: Option<IndexMap<String, JsonSchemaEntry>> = None;
    let mut parent_entry: Option<JsonSchemaEntry> = None;
    let mut lexer = Token::lexer(lexer_text);
    while let Some(result) = lexer.next() {
        if let Ok(token) = result {
            match token {
                Token::BraceOpen => {
                    //backup entry and entries
                    parent_entry = Some(entry.clone());
                    parent_entries = Some(entries.clone());
                    // reset
                    entry = Default::default();
                    entries = IndexMap::new();
                }
                Token::BraceClose => {
                    if !entry.name.is_empty() {
                        entry.revise();
                        entries.insert(entry.name.clone(), entry.clone());
                    }
                    let additional_properties = entry.additional_properties.clone();
                    // bring back parent
                    entry = parent_entry.clone().unwrap();
                    entry.required = find_required_fields(&entries);
                    entry.properties = Some(entries.clone());
                    entry.additional_properties = additional_properties;
                    entries = parent_entries.clone().unwrap();
                    entries.insert(entry.name.clone(), entry.clone());
                    // reset
                    entry = Default::default();
                    parent_entries = None;
                    parent_entry = None;
                }
                Token::Colon => {}
                Token::Comma => {
                    if !entry.name.is_empty() {
                        entry.revise();
                        entries.insert(entry.name.clone(), entry.clone());
                    }
                    entry = Default::default();
                }
                Token::PrimitiveType(type_name) => {
                    entry.type_name = convert_to_json_type(&type_name);
                }
                Token::FormatType(format_name) => {
                    entry.type_name = "string".to_owned();
                    entry.format = Some(convert_to_json_format(&format_name));
                }
                Token::ArrayType(array) => {
                    entry.type_name = "array".to_owned();
                    let container_type = array.0;
                    if container_type.to_lowercase().starts_with("set") {
                        entry.unique_items = Some(true);
                    }
                    let item_type = array.1;
                    let item_entry = if FORMAT_NAMES.contains(&item_type.as_str()) {
                        let format = convert_to_json_format(&item_type);
                        json!({
                            "type": "string",
                            "format": format
                        })
                    } else {
                        json!({
                            "type": "string"
                        })
                    };
                    let range_type = array.2;
                    if range_type.starts_with('(') {
                        let items_text = range_type.trim_matches(&['(', ')']).trim();
                        if !items_text.contains(",") {
                            entry.min_items = Some(items_text.parse().unwrap());
                            entry.max_items = Some(items_text.parse().unwrap());
                        } else if items_text.starts_with(",") { //maxItems
                            entry.max_items = Some(items_text[1..].parse().unwrap());
                        } else if items_text.ends_with(",") { //minItems
                            entry.min_items = Some(items_text[..items_text.len() - 1].parse().unwrap());
                        } else {
                            let items = items_text.split(',').collect::<Vec<&str>>();
                            if items.len() == 2 {
                                entry.min_items = Some(items[0].parse().unwrap());
                                entry.max_items = Some(items[1].parse().unwrap());
                            }
                        }
                    }
                    entry.items = Some(item_entry);
                }
                Token::AnyOf(any_of) => {
                    let mut types: Vec<JsonSchemaEntry> = vec![];
                    for type_name in any_of.split('|') {
                        types.push(JsonSchemaEntry::new(type_name));
                    }
                    entry.any_of = Some(types);
                }
                Token::EnumType(enum_type) => {
                    let items_text = enum_type[4..].trim();
                    let items = parse_array(items_text);
                    if !items.is_empty() {
                        entry.enums = Some(items);
                    }
                }
                Token::TupleType(tuple_type) => {
                    entry.type_name = "array".to_owned();
                    let items_text = tuple_type.trim_matches(&['[', ']']).trim();
                    let values = items_text.split(',').map(|item| {
                        json!({
                            "type": convert_to_json_type(item.trim())
                        })
                    }).collect::<Vec<Value>>();
                    entry.items = Some(Value::from(values));
                }
                Token::RangeType(range_type) => {
                    let offset = range_type.find('(').unwrap();
                    let type_name = convert_to_json_type(range_type[..offset].trim());
                    entry.type_name = type_name;
                    let items_text = range_type[offset..].trim_matches(&['(', ')']).trim();
                    if items_text.starts_with(",") { //maximum
                        entry.maximum = Some(Value::from_str(items_text[1..].trim()).unwrap());
                    } else if items_text.ends_with(",") { //minimum
                        entry.minimum = Some(Value::from_str(items_text[..items_text.len() - 1].trim()).unwrap());
                    } else {
                        let items = items_text.split(',').collect::<Vec<&str>>();
                        if items.len() == 2 {
                            entry.minimum = Some(Value::from_str(items[0].trim()).unwrap());
                            entry.maximum = Some(Value::from_str(items[1].trim()).unwrap());
                        }
                    }
                }
                Token::StringLengthType(length_type) => {
                    let offset = length_type.find('(').unwrap();
                    let type_name = length_type[0..offset].trim().to_lowercase();
                    entry.type_name = "string".to_owned();
                    let items_text = length_type[offset..].trim_matches(&['(', ')']).trim();
                    if !items_text.contains(',') {
                        let length = items_text.parse().unwrap();
                        if type_name == "varchar" {
                            entry.max_length = Some(length);
                        } else {
                            entry.min_length = Some(length);
                            entry.max_length = Some(length);
                        }
                    } else if items_text.starts_with(",") { //maxLength
                        entry.max_length = Some(items_text[1..].trim().parse().unwrap());
                    } else if items_text.ends_with(",") { //minLength
                        entry.min_length = Some(items_text[1..].trim().parse().unwrap());
                    } else {
                        let items = items_text.split(',').collect::<Vec<&str>>();
                        if items.len() == 2 {
                            entry.min_length = Some(items[0].trim().parse().unwrap());
                            entry.max_length = Some(items[1].trim().parse().unwrap());
                        }
                    }
                }
                Token::RegexType(regex_type) => {
                    let pattern = regex_type[5..].trim()
                        .trim_matches(&['(', ')'])
                        .trim()
                        .trim_matches(&['"', '\'']);
                    entry.pattern = Some(pattern.to_string());
                }
                Token::ObjectName(_object_name) => {
                    entry.type_name = "object".to_string();
                }
                Token::FieldName(field_name) => {
                    if field_name.ends_with('?') {
                        entry.optional = true;
                        entry.name = field_name[..field_name.len() - 1].to_string();
                    } else {
                        entry.name = field_name;
                    }
                }
                Token::Ellipsis => {
                    entry.additional_properties = Some(true);
                }
            }
        } else {
            return Err("Failed to parse struct".to_string());
        }
    }
    if !entry.name.is_empty() {
        entry.revise();
        entries.insert(entry.name.clone(), entry.clone());
    } else if entry.additional_properties.is_some() {
        json_schema.additional_properties = entry.additional_properties.clone();
    }
    json_schema.required = find_required_fields(&entries);
    json_schema.properties = Some(entries);

    Ok(json_schema)
}

fn parse_array(text: &str) -> Vec<Value> {
    let mut lexer = ArrayToken::lexer(text);
    let mut items: Vec<Value> = vec![];
    while let Some(result) = lexer.next() {
        if let Ok(token) = result {
            match token {
                ArrayToken::Integer(value) => {
                    items.push(Value::from(value));
                }
                ArrayToken::Number(value) => {
                    items.push(Value::from(value));
                }
                ArrayToken::Text1(value) => {
                    let temp = value.trim_matches('\'').replace("\"", "\\\"");
                    let text2 = format!("\"{}\"", temp);
                    items.push(Value::from_str(&text2).unwrap());
                }
                ArrayToken::Text2(value) => {
                    items.push(Value::from_str(&value).unwrap());
                }
                _ => {}
            }
        }
    }
    items
}

fn convert_to_json_type(type_name: &str) -> String {
    let name = type_name.to_lowercase();
    match name.as_str() {
        "varchar" | "text" | "bytes" | "bytea" => "string".to_string(),
        "int" | "long" | "bigint" | "serial" | "bigserial" => "integer".to_string(),
        "float" | "double" | "real" | "decimal" => "number".to_string(),
        "bool" => "boolean".to_string(),
        _ => name
    }
}

fn convert_to_json_format(format_name: &str) -> String {
    let name = format_name.to_lowercase();
    match name.as_str() {
        "datetime" | "timestamp" => "date-time".to_string(),
        "interval" => "duration".to_string(),
        "json" | "xml" => "string".to_string(),
        _ => name
    }
}

fn find_required_fields(entries: &IndexMap<String, JsonSchemaEntry>) -> Option<Vec<String>> {
    let mut required: Vec<String> = vec![];
    for entry in entries {
        if !entry.1.optional {
            required.push(entry.0.clone());
        }
    }
    if !required.is_empty() {
        Some(required)
    } else {
        None
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let text = r#"User { id: int, nick: string(6,64) }"#;
        let mut lexer = Token::lexer(text);
        while let Some(token) = lexer.next() {
            println!("{:?}", token);
        }
    }
    #[test]
    fn test_parse() {
        let text = r#"User { id: int, tags: list<string>(2,4) }"#;
        let json_schema = to_json_schema(text).unwrap();
        println!("{}", serde_json::to_string_pretty(&json_schema).unwrap())
    }

    #[test]
    fn test_parse_nested() {
        let text = r#"User { id: int, contact: Contact { phone: string, email: string, ... }, status: enum('First',"Second", 1, 2) }"#;
        let json_schema = to_json_schema(text).unwrap();
        println!("{}", serde_json::to_string_pretty(&json_schema).unwrap())
    }

    #[test]
    fn test_to_json() {
        let mut json_schema = JsonSchema::version_2020("User");
        let mut entries: IndexMap<String, JsonSchemaEntry> = IndexMap::new();
        entries.insert("nick".to_owned(), JsonSchemaEntry::new("string"));
        entries.insert("email".to_owned(), JsonSchemaEntry::format("Email"));
        json_schema.properties = Some(entries);
        println!("{}", serde_json::to_string_pretty(&json_schema).unwrap())
    }

    #[test]
    fn test_array() {
        let text = r#"['First',"Second", -1, 2, 3.0]"#;
        let array = parse_array(text);
        println!("{}", serde_json::to_string_pretty(&array).unwrap())
    }
}
