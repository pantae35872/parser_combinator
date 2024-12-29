use std::{collections::HashMap, fmt::Debug};

use crate::{
    boolean, either, identifier, left, match_literal, number, optional, pair, quoted_string, right,
    whitespace_wrap, zero_or_more, ArcedParser, Parser,
};
type Array = Vec<JsonObject>;
type Table = HashMap<String, JsonObject>;

#[derive(Clone, Debug, PartialEq)]
pub enum JsonObject {
    String(String),
    Integer(i64),
    Boolean(bool),
    Array(Array),
    Table(Table),
}

/// Just a bunch of either
macro_rules! choose {
    ($first:expr, $($rest:expr),+ $(,)?) => {{
        let next = $first;
        $(
            let next = either(next, $rest);
        )+
        next
    }};
}

fn json_value<'a>() -> impl Parser<'a, JsonObject> {
    choose!(
        quoted_string().map(|e| JsonObject::String(e)),
        number().map(|e| JsonObject::Integer(e)),
        boolean().map(|e| JsonObject::Boolean(e)),
        array().map(|e| JsonObject::Array(e)),
        table().map(|e| JsonObject::Table(e))
    )
}

fn array<'a>() -> impl Parser<'a, Vec<JsonObject>> {
    whitespace_wrap(match_literal("[")).and_then(|_| {
        left(
            comma_seperated(ArcedParser::new(json_value())),
            whitespace_wrap(match_literal("]")),
        )
    })
}

fn comma_seperated<'a, A, P>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A> + Clone + 'a,
    A: Clone + 'a + Debug,
{
    optional(left(
        parser.clone().and_then(move |e| {
            zero_or_more(right(whitespace_wrap(match_literal(",")), parser.clone())).map(
                move |mut a| {
                    a.insert(0, e.clone());
                    a
                },
            )
        }),
        whitespace_wrap(optional(match_literal(","))), // trailing commas
    ))
    .map(|e| e.unwrap_or_default())
}

pub fn table<'a>() -> impl Parser<'a, Table> {
    whitespace_wrap(match_literal("{"))
        .and_then(|_| {
            left(
                comma_seperated(ArcedParser::new(key_value_pair())),
                match_literal("}"),
            )
        })
        .map(|pairs| {
            let mut map = HashMap::new();
            for (key, value) in pairs {
                map.insert(key, value);
            }
            map
        })
}

fn key_value_pair<'a>() -> impl Parser<'a, (String, JsonObject)> {
    pair(
        whitespace_wrap(either(identifier, quoted_string())),
        right(match_literal(":"), whitespace_wrap(json_value())),
    )
}
