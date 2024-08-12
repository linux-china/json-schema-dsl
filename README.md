JSON Schema DSL
==================

A simple DSL to write JSON Schema.

# Why JSON Schema DSL?

- Make JSON Schema concise
- AI friendly: Function calling, Structured Output with simple DSL
- Schema friendly for CSV, Excel, Database table.

![JSON Schema DSL](json-schema-dsl.png)

# Get Started

CLI: `cargo install json-schema-dsl`

```shell
$ json-schema-dsl "User{ id: int, name: string, email: Email}"
```

Output as following: 

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "User",
  "type": "object",
  "properties": {
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    },
    "email": {
      "type": "string",
      "format": "email"
    }
  },
  "required": [
    "id",
    "name",
    "email"
  ]
}
```

Rust library: `cargo add json-schema-dsl serde_json`

```rust
fn main() {
    let struct_text = "User {id: int, name: string, email: Email}";
    let json_schema = json_schema_dsl::to_json_schema(struct_text).unwrap();
    println!("{}", serde_json::to_string_pretty(&json_schema).unwrap());
}
```

# Syntax

`User { id: int, name: string, birth_date: Date, email?: Email, tags: List<string>}`

- Object Name: starts with capital character, such as `ObjectName { field: type }`.
- Field name: starts with lower-case character. 
- Optional field: `field?: type`

### Basic Types

JSON Schema basic types:

- `string`: aliases:  `varchar`, `Text`, `String`, `bytes`(base64)
- `integer`: aliases: `int`, `long`
- `number`: aliases: `float`, `double`, `real`, `decimal`
- `boolean`: aliases: `bool`

### array Types

array type is alike `List<T>`, and T is a basic type or format name. 

- `List`: aliases: `list`
- `Array`: aliases: `array`
- `Set`(uniqueItems): aliases: `set`

### object Type

Declare object type: `field: ObjectName {field: type}`.

**Attention**: `ObjectName` should start with Capital Character.

### Formats

JSON Schema formats, and name should start with a capital letter:

- `Date`
- `Time`
- `Datetime`
- `Duration`
- `Email`
- `Hostname`
- `Ipv4`
- `Ipv6`
- `Uri`
- `Hostname`
- `Uuid` or `UUID`

### Misc

- enum: `enum('a', 'b', 'c')` or `enum(1, 2, 3)`
- regex: `regex('^[a-z]+$')`
- anyOf: `field: type1|type2`, no space between types
- additionalProperties: `{field: type, ...}`

# References

* JSON Schema: https://json-schema.org/
* JSON Schema formats: https://json-schema.org/understanding-json-schema/reference/string#built-in-formats