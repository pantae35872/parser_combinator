use std::{
    any::TypeId,
    fmt::{self, Debug, Display, Formatter},
    process::Output,
    sync::Arc,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserErrorType {
    UnexpectedValue { expected: String },
    ExpectedIdentifier,
    ExpectedNumber,
    ExpectedBoolean,
    ExpectedOneOrMoreOf { name: &'static str },
    ExpectedAnyOf { names: Vec<&'static str> },
    UnexpectedEndOfStream,
}

pub struct WithInput<'a, T> {
    value: T,
    input: &'a str,
}

impl<T> Debug for WithInput<'_, T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{ Value: {:?}, Input: {} }}", self.value, self.input)
    }
}

pub type ParserError<'a> = WithInput<'a, ParserErrorType>;

type ParseResult<'a, Output> = Result<(&'a str, Output), ParserError<'a>>;

pub mod json;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn named(self, name: &'static str) -> NamedParser<'a, Output>
    where
        Self: Sized + 'a,
    {
        NamedParser::new(move |input| self.parse(input), name)
    }

    fn arced(self) -> ArcedParser<'a, Output>
    where
        Self: Sized + 'a,
    {
        ArcedParser::new(self)
    }

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn map_err<F>(self, map_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(ParserError<'a>) -> ParserError<'a> + 'a,
    {
        BoxedParser::new(map_err(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }

    fn then<P, NewOutput>(self, parser: P) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        P: Parser<'a, NewOutput> + 'a + Clone,
        NewOutput: 'a,
    {
        BoxedParser::new(and_then(self, move |_| parser.clone()))
    }
}

pub trait AsStaticStr {
    fn as_static_str(&self) -> &'static str;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

pub struct NamedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
    name: &'static str,
}

impl<'a, Output> NamedParser<'a, Output> {
    fn new<P>(parser: P, name: &'static str) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        NamedParser {
            parser: Box::new(parser),
            name,
        }
    }
}

impl<'a, Output> Parser<'a, Output> for NamedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

impl<'a, Output> AsStaticStr for NamedParser<'a, Output> {
    fn as_static_str(&self) -> &'static str {
        self.name
    }
}

pub struct ArcedParser<'a, Output> {
    parser: Arc<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> ArcedParser<'a, Output> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        ArcedParser {
            parser: Arc::new(parser),
        }
    }
}

impl<'a, Output> Clone for ArcedParser<'a, Output> {
    fn clone(&self) -> Self {
        Self {
            parser: self.parser.clone(),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for ArcedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

impl<'a, T> WithInput<'a, T> {
    pub fn new(value: T, input: &'a str) -> Self {
        Self { value, input }
    }

    pub fn replace(self, new_value: T) -> Self {
        Self {
            value: new_value,
            input: self.input,
        }
    }
}

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(WithInput::new(
            ParserErrorType::UnexpectedValue {
                expected: expected.to_string(),
            },
            input,
        )),
    }
}

fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(WithInput::new(ParserErrorType::ExpectedIdentifier, input)),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn map_err<'a, P, F, A>(parser: P, map_fn: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(ParserError<'a>) -> ParserError<'a>,
{
    move |input| parser.parse(input).map_err(|e| map_fn(e))
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A> + AsStaticStr,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(WithInput::new(
                ParserErrorType::ExpectedOneOrMoreOf {
                    name: parser.as_static_str(),
                },
                input,
            ));
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn zero_or_more_until<'a, P, A, E, F>(parser: P, end: E) -> impl Parser<'a, (Vec<A>, F)>
where
    P: Parser<'a, A>,
    E: Parser<'a, F>,
{
    move |mut input| {
        let mut result = Vec::new();

        let err;
        loop {
            match parser.parse(input) {
                Ok((next_input, next_item)) => {
                    input = next_input;
                    result.push(next_item);
                }
                Err(e) => {
                    err = e;
                    break; // Exit the loop on error
                }
            }
        }
        match end.parse(input) {
            Ok((next_input, end)) => Ok((next_input, (result, end))),
            Err(_) => Err(err),
        }
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(WithInput::new(
            ParserErrorType::UnexpectedEndOfStream,
            input,
        )),
    }
}

fn number<'a>() -> impl Parser<'a, i64> {
    one_or_more(any_char.pred(|e| e.is_numeric()).named("digit"))
        .map(|e| i64::from_str_radix(&e.iter().collect::<String>(), 10).unwrap())
        .map_err(|e| e.replace(ParserErrorType::ExpectedNumber))
}

fn boolean<'a>() -> impl Parser<'a, bool> {
    either(
        match_literal("true").map(|_| true),
        match_literal("false").map(|_| false),
    )
    .map_err(|e| e.replace(ParserErrorType::ExpectedBoolean))
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(WithInput::new(ParserErrorType::ExpectedIdentifier, input))
    }
}

fn optional<'a, P, A>(parser: P) -> impl Parser<'a, Option<A>>
where
    P: Parser<'a, A>,
{
    move |input| {
        Ok(parser
            .parse(input)
            .map(|(n, e)| (n, Some(e)))
            .unwrap_or((input, None)))
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(
        zero_or_more(whitespace_char()),
        left(parser, zero_or_more(whitespace_char())),
    )
}
/*
#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello Joe!");
    assert_eq!(Ok(("", ())), parse_joe.parse("Hello Joe!"));
    assert_eq!(
        Ok((" Hello Robert!", ())),
        parse_joe.parse("Hello Joe! Hello Robert!")
    );
    assert_eq!(Err("Hello Mike!"), parse_joe.parse("Hello Mike!"));
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("", "i-am-an-identifier".to_string())),
        identifier("i-am-an-identifier")
    );
    assert_eq!(
        Ok((" entirely an identifier", "not".to_string())),
        identifier("not entirely an identifier")
    );
    assert_eq!(
        Err("!not at all an identifier"),
        identifier("!not at all an identifier")
    );
}

#[test]
fn pair_combinator() {
    let tag_opener = pair(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", ((), "my-first-element".to_string()))),
        tag_opener.parse("<my-first-element/>")
    );
    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

#[test]
fn right_combinator() {
    let tag_opener = right(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", "my-first-element".to_string())),
        tag_opener.parse("<my-first-element/>")
    );
    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Err("ahah"), parser.parse("ahah"));
    assert_eq!(Err(""), parser.parse(""));
}

#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
    assert_eq!(Ok(("", vec![])), parser.parse(""));
}

#[test]
fn predicate_combinator() {
    let parser = pred(any_char, |c| *c == 'o');
    assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
    assert_eq!(Err("lol"), parser.parse("lol"));
}

#[test]
fn quoted_string_parser() {
    assert_eq!(
        Ok(("", "Hello Joe!".to_string())),
        quoted_string().parse("\"Hello Joe!\"")
    );
}*/
