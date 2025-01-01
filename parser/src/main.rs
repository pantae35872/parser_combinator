use parser_combinator::{
    json::{comma_seperated, json_value, key_value_pair, table},
    ArcedParser, Parser,
};

fn main() {
    println!(
        "{:?}",
        comma_seperated(ArcedParser::new(key_value_pair()), "{", "}")
            .parse("{bb: \"aa\", ee: bb }")
    );
    /*println!(
        "{:?}",
        table().parse(
            r#"
            {
                aaa: 50,
                ccc: aa,
                bbb: {
                    haha: 500,
                    str: "bbb",
                    "quoted": true,
                },
            }"#
        )
    );*/
}
