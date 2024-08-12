use std::str::FromStr;
use indexmap::IndexMap;
use logos::{Lexer, Logos};
use serde::{Deserialize, Serialize};
use serde_json::{Value};

const FORMAT_NAMES: &'static [&'static str] = &["Date", "Time", "DateTime", "Duration", "Email", "Ipv4", "Ipv6", "Uri", "Hostname", "Uuid", "UUID"];

fn array_type_callback(lex: &mut Lexer<Token>) -> (String, String) {
    let complex_type = lex.slice().to_owned();
    let offset = complex_type.find('<').unwrap();
    let container_type = complex_type[..offset].to_owned();
    let item_type = complex_type[offset + 1..(complex_type.len() - 1)].to_owned();
    (container_type, item_type)
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

    #[regex("integer|int|long|bigint|number|float|double|real|decimal|boolean|bool|string|bytes|varchar|String|Text",
    |lex| lex.slice().to_owned())]
    PrimitiveType(String),

    #[regex("Date|Time|DateTime|Duration|Email|Ipv4|Ipv6|Uri|Hostname|Uuid|UUID",
    |lex| lex.slice().to_owned())]
    FormatType(String),

    #[regex("(List|list|Set|set|Array|array)<(integer|int|long|bigint|number|float|double|real|decimal|boolean|bool|string|bytes|varchar|Text|Date|Time|DateTime|Duration|Email|Ipv4|Ipv6|Uri|Hostname|Uuid|UUID)>",
        array_type_callback
    )]
    ArrayType((String, String)),

    #[regex("(integer|int|long|bigint|number|float|double|real|decimal|boolean|bool|string|bytes|varchar|String|Text|Date|Time|DateTime|Duration|Email|Ipv4|Ipv6|Uri|Hostname|Uuid|UUID)([|](integer|int|long|bigint|number|float|double|real|decimal|boolean|bool|string|bytes|varchar|String|Text|Date|Time|DateTime|Duration|Email|Ipv4|Ipv6|Uri|Hostname|Uuid|UUID))+",
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
    pub items: Option<Box<JsonSchemaEntry>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "enum")]
    pub enums: Option<Vec<Value>>,
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
                    let item_type = array.1;
                    let mut item_entry: JsonSchemaEntry = Default::default();
                    if FORMAT_NAMES.contains(&item_type.as_str()) {
                        item_entry.type_name = "string".to_owned();
                        item_entry.format = Some(convert_to_json_format(&item_type));
                    } else {
                        item_entry.type_name = convert_to_json_type(&item_type);
                    }
                    if container_type.to_lowercase().starts_with("set") {
                        item_entry.unique_items = Some(true);
                    }
                    entry.items = Some(Box::new(item_entry))
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
        "varchar" | "text" | "bytes" => "string".to_string(),
        "int" | "long" => "integer".to_string(),
        "float" | "double" | "real" | "decimal" => "number".to_string(),
        "bool" => "boolean".to_string(),
        _ => name
    }
}

fn convert_to_json_format(format_name: &str) -> String {
    let name = format_name.to_lowercase();
    match name.as_str() {
        "datetime" => "date-time".to_string(),
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
        let text = r#"User { id: int, name: string, birth_date: Date, cell_phone: regex('[\d]+'), email?: Email, tags: List<string>, birth: Date|DateTime, status: enum('First',"Second", 1, 2) }"#;
        let mut lexer = Token::lexer(text);
        while let Some(token) = lexer.next() {
            println!("{:?}", token);
        }
    }
    #[test]
    fn test_parse() {
        let text = r#"User { id: int, name: string, birth_date: Date, cell_phone: regex('[\d]+'), email?: Email, tags: List<string>, birth: Date|DateTime, status: enum('First',"Second", 1, 2), ... }"#;
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
