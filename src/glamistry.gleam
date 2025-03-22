import gleam/list
import gleam/result
import gleam/string

pub fn main() {
  let data =
    "
    create table test (
    	`id` integer,
      Name integer,
    )
    "

  let assert Ok(lex) = lexer(data, [])
  parse(lex) |> echo
}

pub type Token {
  Identifier(name: String)

  Create
  Database
  Table

  OpenParenthesis
  CloseParenthesis

  Comma
  Backtick

  Tinyint
  Smallint
  Mediumint
  Integer
  Bigint
  Decimal
  Numeric
  Float
  Double
  Bit
  Boolean
  Time
  Timestamp
  Date
  Datetime
  Year
  Char
  Varchar
  Binary
  Varbinary
  Tinyblob
  Blob
  Mediumblob
  Longblob
  Tinytext
  Text
  MediumText
  Longtext
  Enum
  Set
  Geometry
  Point
  Linestring
  Polygon
  Multipoint
  Multipolygon
  Geometrycollection
  Json
}

fn lexer(input: String, tokens: List(Token)) -> Result(List(Token), Nil) {
  case input {
    // new line
    "\r" <> rest | "\n" <> rest | "\r\n" <> rest -> lexer(rest, tokens)

    // white space
    " " <> rest | "\t" <> rest -> lexer(rest, tokens)

    // parenthesis
    "(" <> rest -> lexer(rest, list.append(tokens, [OpenParenthesis]))
    ")" <> rest -> lexer(rest, list.append(tokens, [CloseParenthesis]))

    "," <> rest -> lexer(rest, list.append(tokens, [Comma]))
    "`" <> rest -> lexer(rest, list.append(tokens, [Backtick]))

    "a" <> _
    | "b" <> _
    | "c" <> _
    | "d" <> _
    | "e" <> _
    | "f" <> _
    | "g" <> _
    | "h" <> _
    | "i" <> _
    | "j" <> _
    | "k" <> _
    | "l" <> _
    | "m" <> _
    | "n" <> _
    | "o" <> _
    | "p" <> _
    | "q" <> _
    | "r" <> _
    | "s" <> _
    | "t" <> _
    | "u" <> _
    | "v" <> _
    | "w" <> _
    | "x" <> _
    | "y" <> _
    | "z" <> _
    | "A" <> _
    | "B" <> _
    | "C" <> _
    | "D" <> _
    | "E" <> _
    | "F" <> _
    | "G" <> _
    | "H" <> _
    | "I" <> _
    | "J" <> _
    | "K" <> _
    | "L" <> _
    | "M" <> _
    | "N" <> _
    | "O" <> _
    | "P" <> _
    | "Q" <> _
    | "R" <> _
    | "S" <> _
    | "T" <> _
    | "U" <> _
    | "V" <> _
    | "W" <> _
    | "X" <> _
    | "Y" <> _
    | "Z" <> _
    | "_" <> _ -> {
      let #(name, rest) = take_content(input, "", is_name_grapheme)

      case string.lowercase(name) {
        "create" -> lexer(rest, list.append(tokens, [Create]))
        "database" -> lexer(rest, list.append(tokens, [Database]))
        "table" -> lexer(rest, list.append(tokens, [Table]))
        "tinyint" -> lexer(rest, list.append(tokens, [Tinyint]))
        "smallint" -> lexer(rest, list.append(tokens, [Smallint]))
        "mediumint" -> lexer(rest, list.append(tokens, [Mediumint]))
        "integer" -> lexer(rest, list.append(tokens, [Integer]))
        "bigint" -> lexer(rest, list.append(tokens, [Bigint]))
        "decimal" -> lexer(rest, list.append(tokens, [Decimal]))
        "numeric" -> lexer(rest, list.append(tokens, [Numeric]))
        "float" -> lexer(rest, list.append(tokens, [Float]))
        "double" -> lexer(rest, list.append(tokens, [Double]))
        "bit " -> lexer(rest, list.append(tokens, [Bit]))
        "boolean" -> lexer(rest, list.append(tokens, [Boolean]))
        "time" -> lexer(rest, list.append(tokens, [Time]))
        "timestamp" -> lexer(rest, list.append(tokens, [Timestamp]))
        "date " -> lexer(rest, list.append(tokens, [Date]))
        "datetime" -> lexer(rest, list.append(tokens, [Datetime]))
        "year" -> lexer(rest, list.append(tokens, [Year]))
        "char " -> lexer(rest, list.append(tokens, [Char]))
        "varchar" -> lexer(rest, list.append(tokens, [Varchar]))
        "binary" -> lexer(rest, list.append(tokens, [Binary]))
        "varbinary" -> lexer(rest, list.append(tokens, [Varbinary]))
        "tinyblob" -> lexer(rest, list.append(tokens, [Tinyblob]))
        "blob" -> lexer(rest, list.append(tokens, [Blob]))
        "mediumblob" -> lexer(rest, list.append(tokens, [Mediumblob]))
        "longblob" -> lexer(rest, list.append(tokens, [Longblob]))
        "tinytext" -> lexer(rest, list.append(tokens, [Tinytext]))
        "text " -> lexer(rest, list.append(tokens, [Text]))
        "mediumText" -> lexer(rest, list.append(tokens, [MediumText]))
        "longtext" -> lexer(rest, list.append(tokens, [Longtext]))
        "Enum" -> lexer(rest, list.append(tokens, [Enum]))
        "set " -> lexer(rest, list.append(tokens, [Set]))
        "geometry" -> lexer(rest, list.append(tokens, [Geometry]))
        "point" -> lexer(rest, list.append(tokens, [Point]))
        "linestring" -> lexer(rest, list.append(tokens, [Linestring]))
        "polygon" -> lexer(rest, list.append(tokens, [Polygon]))
        "multipoint" -> lexer(rest, list.append(tokens, [Multipoint]))
        "multipolygon" -> lexer(rest, list.append(tokens, [Multipolygon]))
        "geometrycollection" ->
          lexer(rest, list.append(tokens, [Geometrycollection]))
        "json" -> lexer(rest, list.append(tokens, [Json]))
        _ -> lexer(rest, list.append(tokens, [Identifier(name)]))
      }
    }

    "" -> Ok(tokens)
    _ -> Error(Nil)
  }
}

fn is_name_grapheme(grapheme: String) -> Bool {
  case grapheme {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z"
    | "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "_" -> True

    _ -> False
  }
}

fn take_content(
  source: String,
  content: String,
  predicate: fn(String) -> Bool,
) -> #(String, String) {
  case string.pop_grapheme(source) {
    Error(_) -> #(content, "")
    Ok(#(grapheme, rest)) -> {
      case predicate(grapheme) {
        True -> take_content(rest, content <> grapheme, predicate)
        False -> #(content, source)
      }
    }
  }
}

pub type TableAst {
  TableAst(name: String, columns: List(ColumnAst))
}

pub type ColumnAst {
  ColumnAst(name: String, column_type: TypeAst)
}

pub type TypeAst {
  TinyintType
  SmallintType
  MediumintType
  IntegerType
  BigintType
  DecimalType
  NumericType
  FloatType
  DoubleType
  BitType
  BooleanType
  TimeType
  TimestampType
  DateType
  DatetimeType
  YearType
  CharType
  VarcharType
  BinaryType
  VarbinaryType
  TinyblobType
  BlobType
  MediumblobType
  LongblobType
  TinytextType
  TextType
  MediumTextType
  LongtextType
  EnumType
  SetType
  GeometryType
  PointType
  LinestringType
  PolygonType
  MultipointType
  MultipolygonType
  GeometrycollectionType
  JsonType
}

// create table wibble ()

pub fn parse(tokens: List(Token)) -> Result(TableAst, Nil) {
  parse_table(tokens)
}

fn parse_table(tokens: List(Token)) -> Result(TableAst, Nil) {
  use #(create, tokens) <- result.try(get_next_token(tokens))
  use _ <- result.try(parse_create_token(create))

  use #(table, tokens) <- result.try(get_next_token(tokens))
  use _ <- result.try(parse_table_token(table))

  use #(identifer, tokens) <- result.try(get_next_token(tokens))
  use table_name <- result.try(identifier_to_string(identifer))

  use #(open_parenthesis_token, tokens) <- result.try(get_next_token(tokens))
  use _ <- result.try(parse_open_parenthesis_token(open_parenthesis_token))

  use #(columns_ast, tokens) <- result.try(parse_columns(tokens, []))

  use #(close_parenthesis_token, _) <- result.try(get_next_token(tokens))
  use _ <- result.try(parse_close_parenthesis_token(close_parenthesis_token))

  Ok(TableAst(table_name, columns_ast))
}

pub fn parse_columns(
  tokens: List(Token),
  columns: List(ColumnAst),
) -> Result(#(List(ColumnAst), List(Token)), Nil) {
  case tokens {
    [Identifier(_), ..] | [Backtick, Identifier(_), Backtick, ..] -> {
      use #(column_ast, tokens) <- result.try(parse_column(tokens))
      let columns = list.append(columns, [column_ast])
      parse_columns(tokens, columns)
    }
    [CloseParenthesis] | [CloseParenthesis, ..] -> Ok(#(columns, tokens))
    _ -> {
      Error(Nil)
    }
  }
}

fn parse_column(tokens: List(Token)) -> Result(#(ColumnAst, List(Token)), Nil) {
  use #(row_name, tokens) <- result.try(parse_maybe_quoted_identifier_token(
    tokens,
  ))

  use #(row_type_token, tokens) <- result.try(get_next_token(tokens))
  use row_type <- result.try(parse_identifier_type_token(row_type_token))

  use #(comma_token, tokens) <- result.try(get_next_token(tokens))
  use _ <- result.try(parse_comma_token(comma_token))

  Ok(#(ColumnAst(row_name, row_type), tokens))
}

fn parse_comma_token(token: Token) -> Result(Nil, Nil) {
  case token {
    Comma -> Ok(Nil)
    _ -> Error(Nil)
  }
}

fn parse_identifier_type_token(token: Token) -> Result(TypeAst, Nil) {
  case token {
    Bigint -> Ok(BigintType)
    Binary -> Ok(BinaryType)
    Bit -> Ok(BitType)
    Blob -> Ok(BlobType)
    Boolean -> Ok(BooleanType)
    Char -> Ok(CharType)
    Date -> Ok(DateType)
    Datetime -> Ok(DatetimeType)
    Decimal -> Ok(DecimalType)
    Double -> Ok(DoubleType)
    Enum -> Ok(EnumType)
    Float -> Ok(FloatType)
    Geometry -> Ok(GeometryType)
    Geometrycollection -> Ok(GeometrycollectionType)
    Integer -> Ok(IntegerType)
    Json -> Ok(JsonType)
    Linestring -> Ok(LinestringType)
    Longblob -> Ok(LongblobType)
    Longtext -> Ok(LongtextType)
    MediumText -> Ok(MediumTextType)
    Mediumblob -> Ok(MediumblobType)
    Mediumint -> Ok(MediumintType)
    Multipoint -> Ok(MultipointType)
    Multipolygon -> Ok(MultipolygonType)
    Numeric -> Ok(NumericType)
    Point -> Ok(PointType)
    Polygon -> Ok(PolygonType)
    Set -> Ok(SetType)
    Smallint -> Ok(SmallintType)
    Text -> Ok(TextType)
    Time -> Ok(TimeType)
    Timestamp -> Ok(TimestampType)
    Tinyblob -> Ok(TinyblobType)
    Tinyint -> Ok(TinyintType)
    Tinytext -> Ok(TinytextType)
    Varbinary -> Ok(VarbinaryType)
    Varchar -> Ok(VarcharType)
    Year -> Ok(YearType)
    _ -> Error(Nil)
  }
}

fn parse_maybe_quoted_identifier_token(
  tokens: List(Token),
) -> Result(#(String, List(Token)), Nil) {
  case list.take(tokens, 3) {
    [Backtick, Identifier(value), Backtick] -> {
      Ok(#(value, list.drop(tokens, 3)))
    }
    [Identifier(value), _, _] -> {
      Ok(#(value, list.drop(tokens, 1)))
    }
    _ -> Error(Nil)
  }
}

fn identifier_to_string(token: Token) -> Result(String, Nil) {
  case token {
    Identifier(value) -> Ok(value)
    _ -> Error(Nil)
  }
}

fn get_next_token(tokens: List(Token)) -> Result(#(Token, List(Token)), Nil) {
  use token <- result.try(list.first(tokens))
  let list = list.drop(tokens, 1)
  Ok(#(token, list))
}

fn parse_create_token(token: Token) -> Result(Nil, Nil) {
  case token {
    Create -> Ok(Nil)
    _ -> Error(Nil)
  }
}

fn parse_table_token(token: Token) -> Result(Nil, Nil) {
  case token {
    Table -> Ok(Nil)
    _ -> Error(Nil)
  }
}

fn parse_open_parenthesis_token(token: Token) -> Result(Nil, Nil) {
  case token {
    OpenParenthesis -> Ok(Nil)
    _ -> Error(Nil)
  }
}

fn parse_close_parenthesis_token(token: Token) -> Result(Nil, Nil) {
  case token {
    CloseParenthesis -> Ok(Nil)
    _ -> Error(Nil)
  }
}
