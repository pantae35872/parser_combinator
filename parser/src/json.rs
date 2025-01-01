use std::{collections::HashMap, fmt::Debug};

use crate::{
    boolean, either, identifier, left, match_literal, number, optional, pair, quoted_string, right,
    whitespace_wrap, zero_or_more, ArcedParser, Parser, ParserErrorType,
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

pub fn json_value<'a>() -> impl Parser<'a, JsonObject> {
    choose!(
        quoted_string().map(|e| JsonObject::String(e)),
        number().map(|e| JsonObject::Integer(e)),
        boolean().map(|e| JsonObject::Boolean(e)),
        //array().map(|e| JsonObject::Array(e)),
        //table().map(|e| JsonObject::Table(e))
    )
    .map_err(|e| e.replace(ParserErrorType::ExpectedAnyOf { names: vec!["qq"] }))
}

fn array<'a>() -> impl Parser<'a, Vec<JsonObject>> {
    comma_seperated(ArcedParser::new(json_value()), "[", "]")
}

pub fn comma_seperated<'a, A, P>(
    parser: P,
    open: &'static str,
    close: &'static str,
) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A> + Clone + 'a,
    A: Clone + 'a + Debug,
{
    either(
        pair(
            whitespace_wrap(match_literal(open)),
            whitespace_wrap(match_literal(close)),
        )
        .map(|_| Vec::new()),
        match_literal(open).then(
            left(
                left(
                    parser.clone().and_then(move |e| {
                        zero_or_more(right(whitespace_wrap(match_literal(",")), parser.clone()))
                            .map(move |mut a| {
                                a.insert(0, e.clone());
                                a
                            })
                    }),
                    whitespace_wrap(optional(match_literal(","))), // trailing commas
                ),
                whitespace_wrap(match_literal(close)),
            )
            .arced(),
        ),
    )
    /*optional(left(
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
    .map(|e| e.unwrap_or_default())*/
}

pub fn table<'a>() -> impl Parser<'a, Table> {
    comma_seperated(ArcedParser::new(key_value_pair()), "{", "}").map(|pairs| {
        let mut map = HashMap::new();
        for (key, value) in pairs {
            map.insert(key, value);
        }
        map
    })
}

pub fn key_value_pair<'a>() -> impl Parser<'a, (String, JsonObject)> {
    pair(
        whitespace_wrap(either(identifier, quoted_string())),
        right(match_literal(":"), whitespace_wrap(json_value())),
    )
}
